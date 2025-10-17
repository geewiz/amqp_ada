-- AMQP 0-9-1 Connection Management Implementation
with AMQP.Constants;
with Ada.Streams;
with Ada.Exceptions; use Ada.Exceptions;

package body AMQP.Connection is

   procedure Connect (
      Conn : in out Connection;
      Config : Connection_Config
   ) is
      use GNAT.Sockets;
   begin
      if Conn.State /= Disconnected then
         raise Connection_Error with "Connection already established";
      end if;

      Conn.Config := Config;
      Conn.State := Connecting;

      -- Create socket
      Create_Socket (Conn.Socket);

      -- Resolve hostname
      Conn.Address.Addr := Addresses (Get_Host_By_Name (Config.Host.all), 1);
      Conn.Address.Port := Config.Port;

      -- Connect to broker
      Connect_Socket (Conn.Socket, Conn.Address);

      Conn.State := Protocol_Header_Sent;

   exception
      when E : Socket_Error =>
         Conn.State := Disconnected;
         raise Connection_Error with "Socket connection failed: " & Exception_Message (E);
   end Connect;

   procedure Disconnect (Conn : in out Connection) is
      use GNAT.Sockets;
   begin
      if Conn.State = Disconnected then
         return;
      end if;

      Conn.State := Closing;

      -- Close socket
      Close_Socket (Conn.Socket);

      Conn.State := Closed;
      Conn.State := Disconnected;  -- Ready for reconnect

   exception
      when Socket_Error =>
         Conn.State := Disconnected;
   end Disconnect;

   function Is_Connected (Conn : Connection) return Boolean is
   begin
      return Conn.State in Protocol_Header_Sent | Connected;
   end Is_Connected;

   function Get_State (Conn : Connection) return Connection_State is
   begin
      return Conn.State;
   end Get_State;

   procedure Send_Frame (
      Conn : in out Connection;
      F : Frame
   ) is
      use GNAT.Sockets;
      use Ada.Streams;
      Buf : Buffer;
      Data : Stream_Element_Array (1 .. Stream_Element_Offset (Calculate_Frame_Size (F)));
      Last : Stream_Element_Offset;
   begin
      if not Is_Connected (Conn) then
         raise Connection_Error with "Not connected";
      end if;

      -- Encode frame to buffer
      Reset (Buf);
      Encode_Frame (Buf, F);

      -- Copy to stream array
      Data (1 .. Buf.Length) := Buf.Data (1 .. Buf.Length);

      -- Send over socket
      Send_Socket (Conn.Socket, Data (1 .. Buf.Length), Last);

      if Last /= Buf.Length then
         raise Connection_Error with "Failed to send complete frame";
      end if;
   end Send_Frame;

   procedure Receive_Frame (
      Conn : in out Connection;
      F : out Frame;
      Success : out Boolean
   ) is
      use GNAT.Sockets;
      use Ada.Streams;
      Chunk : Stream_Element_Array (1 .. 4096);
      Last : Stream_Element_Offset;
   begin
      Success := False;

      if not Is_Connected (Conn) then
         raise Connection_Error with "Not connected";
      end if;

      -- Try to decode a frame from existing buffer
      Decode_Frame (Conn.Receive_Buffer, F, Success);
      if Success then
         return;
      end if;

      -- Need more data - receive from socket
      Receive_Socket (Conn.Socket, Chunk, Last);

      if Last = 0 then
         raise Connection_Error with "Connection closed by peer";
      end if;

      -- Append to receive buffer
      for I in 1 .. Last loop
         Conn.Receive_Buffer.Data (Conn.Receive_Buffer.Length + I) := Chunk (I);
      end loop;
      Conn.Receive_Buffer.Length := Conn.Receive_Buffer.Length + Last;

      -- Try to decode again
      Decode_Frame (Conn.Receive_Buffer, F, Success);

   exception
      when E : Socket_Error =>
         raise Connection_Error with "Socket receive failed: " & Exception_Message (E);
   end Receive_Frame;

   procedure Send_Protocol_Header (Conn : in out Connection) is
      use GNAT.Sockets;
      use Ada.Streams;
      -- AMQP 0-9-1 protocol header: "AMQP" + 0 + 0 + 9 + 1
      Header : constant Stream_Element_Array := (
         Character'Pos ('A'),
         Character'Pos ('M'),
         Character'Pos ('Q'),
         Character'Pos ('P'),
         0,  -- Protocol ID
         AMQP.Constants.PROTOCOL_VERSION_MAJOR,
         AMQP.Constants.PROTOCOL_VERSION_MINOR,
         AMQP.Constants.PROTOCOL_VERSION_REVISION
      );
      Last : Stream_Element_Offset;
   begin
      if Conn.State /= Protocol_Header_Sent then
         raise Connection_Error with "Invalid state for sending protocol header";
      end if;

      Send_Socket (Conn.Socket, Header, Last);

      if Last /= Header'Last then
         raise Connection_Error with "Failed to send complete protocol header";
      end if;

   end Send_Protocol_Header;

end AMQP.Connection;
