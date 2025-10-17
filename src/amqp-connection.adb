-- AMQP 0-9-1 Connection Management Implementation
with AMQP.Constants;
with AMQP.Methods; use AMQP.Methods;
with Ada.Streams;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;
with Interfaces; use Interfaces;

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
      return Conn.State in Protocol_Header_Sent | Start_Received | Start_Ok_Sent |
                          Tune_Received | Tune_Ok_Sent | Open_Sent | Connected;
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

   procedure Perform_Handshake (Conn : in out Connection) is
      use Ada.Streams;
      F : Frame;
      Success : Boolean;
      Payload_Buf : Buffer;

      -- Helper to send a method frame
      procedure Send_Method_Frame (
         Class_Id : Short;
         Method_Id : Short;
         Arguments : Buffer
      ) is
         F : Frame;
         Temp_Buf : Buffer;
      begin
         -- Build method frame payload: class_id + method_id + arguments
         Reset (Temp_Buf);
         Encode_Short (Temp_Buf, Class_Id);
         Encode_Short (Temp_Buf, Method_Id);

         -- Copy arguments
         for I in 1 .. Arguments.Length loop
            Temp_Buf.Data (Temp_Buf.Length + I) := Arguments.Data (I);
         end loop;
         Temp_Buf.Length := Temp_Buf.Length + Arguments.Length;

         -- Create frame
         F.Kind := Method_Frame;  -- The Frame_Type enum value
         F.Channel := 0;  -- Connection methods use channel 0
         F.Payload.Length := Temp_Buf.Length;

         -- Copy payload data
         for I in 1 .. Temp_Buf.Length loop
            F.Payload.Data (I) := Temp_Buf.Data (I);
         end loop;

         Send_Frame (Conn, F);
      end Send_Method_Frame;

      -- Helper to extract method details from a method frame
      procedure Extract_Method (
         F : Frame;
         Class_Id : out Short;
         Method_Id : out Short;
         Arguments : out Buffer
      ) is
         Temp_Buf : Buffer;
      begin
         -- Convert frame payload to buffer
         Reset (Temp_Buf);
         for I in 1 .. F.Payload.Length loop
            Temp_Buf.Data (I) := F.Payload.Data (I);
         end loop;
         Temp_Buf.Length := F.Payload.Length;
         Temp_Buf.Position := 1;

         -- Decode class_id and method_id
         Decode_Short (Temp_Buf, Class_Id);
         Decode_Short (Temp_Buf, Method_Id);

         -- Copy remaining data to arguments buffer
         Reset (Arguments);
         for I in Temp_Buf.Position .. Temp_Buf.Length loop
            Arguments.Data (Arguments.Length + 1) := Temp_Buf.Data (I);
            Arguments.Length := Arguments.Length + 1;
         end loop;
      end Extract_Method;

   begin
      if Conn.State /= Protocol_Header_Sent then
         raise Connection_Error with "Cannot perform handshake in current state";
      end if;

      pragma Debug (Ada.Text_IO.Put_Line ("Starting AMQP handshake..."));

      -- Step 1: Wait for Connection.Start from server
      pragma Debug (Ada.Text_IO.Put_Line ("Waiting for Connection.Start..."));
      Receive_Frame (Conn, F, Success);
      if not Success or else F.Kind /= Method_Frame then
         raise Connection_Error with "Expected Connection.Start method frame";
      end if;

      declare
         Class_Id : Short;
         Method_Id : Short;
         Args_Buf : Buffer;
      begin
         Extract_Method (F, Class_Id, Method_Id, Args_Buf);

         if Class_Id /= AMQP.Constants.CLASS_CONNECTION or else
            Method_Id /= AMQP.Constants.CONNECTION_START
         then
            raise Connection_Error with "Expected Connection.Start method";
         end if;

         Conn.State := Start_Received;
         pragma Debug (Ada.Text_IO.Put_Line ("Received Connection.Start"));

         declare
            Start_Method : Connection_Start;
         begin
            Decode_Connection_Start (Args_Buf, Start_Method, Success);
            if not Success then
               raise Connection_Error with "Failed to decode Connection.Start";
            end if;

            pragma Debug (Ada.Text_IO.Put_Line ("  Server version: " &
               Octet'Image (Start_Method.Version_Major) & "." &
               Octet'Image (Start_Method.Version_Minor)));
            pragma Debug (Ada.Text_IO.Put_Line ("  Mechanisms: " & Start_Method.Mechanisms.all));
         end;
      end;

      -- Step 2: Send Connection.Start-Ok
      pragma Debug (Ada.Text_IO.Put_Line ("Sending Connection.Start-Ok..."));
      declare
         Start_Ok : Connection_Start_Ok;
         Client_Props : Field_Table;
      begin
         -- Build client properties (empty for now)
         Client_Props.Head := null;

         Start_Ok.Client_Properties := Client_Props;
         Start_Ok.Mechanism := new String'("PLAIN");
         Start_Ok.Response := Create_Plain_Auth_Response (
            Conn.Config.Username.all,
            Conn.Config.Password.all
         );
         Start_Ok.Locale := new String'("en_US");

         Reset (Payload_Buf);
         Encode_Connection_Start_Ok (Payload_Buf, Start_Ok);
         Send_Method_Frame (
            AMQP.Constants.CLASS_CONNECTION,
            AMQP.Constants.CONNECTION_START_OK,
            Payload_Buf
         );
      end;

      Conn.State := Start_Ok_Sent;
      pragma Debug (Ada.Text_IO.Put_Line ("Sent Connection.Start-Ok"));

      -- Step 3: Wait for Connection.Tune
      pragma Debug (Ada.Text_IO.Put_Line ("Waiting for Connection.Tune..."));
      Receive_Frame (Conn, F, Success);
      if not Success or else F.Kind /= Method_Frame then
         raise Connection_Error with "Expected Connection.Tune method frame";
      end if;

      declare
         Class_Id : Short;
         Method_Id : Short;
         Args_Buf : Buffer;
      begin
         Extract_Method (F, Class_Id, Method_Id, Args_Buf);

         if Class_Id /= AMQP.Constants.CLASS_CONNECTION or else
            Method_Id /= AMQP.Constants.CONNECTION_TUNE
         then
            raise Connection_Error with "Expected Connection.Tune method";
         end if;

         Conn.State := Tune_Received;
         pragma Debug (Ada.Text_IO.Put_Line ("Received Connection.Tune"));

         declare
            Tune_Method : Connection_Tune;
         begin
            Decode_Connection_Tune (Args_Buf, Tune_Method, Success);
            if not Success then
               raise Connection_Error with "Failed to decode Connection.Tune";
            end if;

            pragma Debug (Ada.Text_IO.Put_Line ("  Server offers - Channel Max: " & Short'Image (Tune_Method.Channel_Max) &
                      ", Frame Max: " & Long'Image (Tune_Method.Frame_Max) &
                      ", Heartbeat: " & Short'Image (Tune_Method.Heartbeat)));

            -- Negotiate parameters (use minimums where applicable)
            if Tune_Method.Channel_Max > 0 then
               Conn.Negotiated_Channel_Max := Tune_Method.Channel_Max;
            else
               Conn.Negotiated_Channel_Max := 2047;  -- Default
            end if;

            if Tune_Method.Frame_Max > 0 then
               if Conn.Config.Frame_Max > 0 then
                  Conn.Negotiated_Frame_Max := Long'Min (Tune_Method.Frame_Max, Conn.Config.Frame_Max);
               else
                  Conn.Negotiated_Frame_Max := Tune_Method.Frame_Max;
               end if;
            else
               Conn.Negotiated_Frame_Max := Conn.Config.Frame_Max;
            end if;

            -- For heartbeat, use client preference if server allows it
            Conn.Negotiated_Heartbeat := Conn.Config.Heartbeat;
         end;
      end;

      -- Step 4: Send Connection.Tune-Ok
      pragma Debug (Ada.Text_IO.Put_Line ("Sending Connection.Tune-Ok..."));
      declare
         Tune_Ok : Connection_Tune_Ok;
      begin
         Tune_Ok.Channel_Max := Conn.Negotiated_Channel_Max;
         Tune_Ok.Frame_Max := Conn.Negotiated_Frame_Max;
         Tune_Ok.Heartbeat := Conn.Negotiated_Heartbeat;

         pragma Debug (Ada.Text_IO.Put_Line ("  Negotiated - Channel Max: " & Short'Image (Tune_Ok.Channel_Max) &
                   ", Frame Max: " & Long'Image (Tune_Ok.Frame_Max) &
                   ", Heartbeat: " & Short'Image (Tune_Ok.Heartbeat)));

         Reset (Payload_Buf);
         Encode_Connection_Tune_Ok (Payload_Buf, Tune_Ok);
         Send_Method_Frame (
            AMQP.Constants.CLASS_CONNECTION,
            AMQP.Constants.CONNECTION_TUNE_OK,
            Payload_Buf
         );
      end;

      Conn.State := Tune_Ok_Sent;
      pragma Debug (Ada.Text_IO.Put_Line ("Sent Connection.Tune-Ok"));

      -- Step 5: Send Connection.Open
      pragma Debug (Ada.Text_IO.Put_Line ("Sending Connection.Open..."));
      declare
         Open_Method : Connection_Open;
         VHost : Methods.String_Access;
      begin
         VHost := new String'(Conn.Config.Virtual_Host.all);
         Open_Method.Virtual_Host := VHost;
         Open_Method.Reserved_1 := new String'("");
         Open_Method.Reserved_2 := False;

         Reset (Payload_Buf);
         Encode_Connection_Open (Payload_Buf, Open_Method);
         Send_Method_Frame (
            AMQP.Constants.CLASS_CONNECTION,
            AMQP.Constants.CONNECTION_OPEN,
            Payload_Buf
         );
      end;

      Conn.State := Open_Sent;
      pragma Debug (Ada.Text_IO.Put_Line ("Sent Connection.Open (vhost: " & Conn.Config.Virtual_Host.all & ")"));

      -- Step 6: Wait for Connection.Open-Ok
      pragma Debug (Ada.Text_IO.Put_Line ("Waiting for Connection.Open-Ok..."));
      Receive_Frame (Conn, F, Success);
      if not Success or else F.Kind /= Method_Frame then
         raise Connection_Error with "Expected Connection.Open-Ok method frame";
      end if;

      declare
         Class_Id : Short;
         Method_Id : Short;
         Args_Buf : Buffer;
      begin
         Extract_Method (F, Class_Id, Method_Id, Args_Buf);

         if Class_Id /= AMQP.Constants.CLASS_CONNECTION or else
            Method_Id /= AMQP.Constants.CONNECTION_OPEN_OK
         then
            raise Connection_Error with "Expected Connection.Open-Ok method";
         end if;

         pragma Debug (Ada.Text_IO.Put_Line ("Received Connection.Open-Ok"));
      end;

      Conn.State := Connected;
      pragma Debug (Ada.Text_IO.Put_Line ("AMQP handshake complete - connection established!"));

   exception
      when E : others =>
         Conn.State := Disconnected;
         raise Connection_Error with "Handshake failed: " & Exception_Message (E);
   end Perform_Handshake;

end AMQP.Connection;
