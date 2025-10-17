-- AMQP 0-9-1 Connection Management
with AMQP.Types; use AMQP.Types;
with AMQP.Frames; use AMQP.Frames;
with AMQP.Codec; use AMQP.Codec;
with GNAT.Sockets;

package AMQP.Connection is

   type String_Access is access constant String;

   -- Connection state
   type Connection_State is (
      Disconnected,
      Connecting,
      Protocol_Header_Sent,
      Connected,
      Closing,
      Closed
   );

   -- Connection configuration
   type Connection_Config is record
      Host : String_Access;
      Port : GNAT.Sockets.Port_Type := 5672;
      Virtual_Host : String_Access;
      Username : String_Access;
      Password : String_Access;
      Frame_Max : Long := Long (Default_Frame_Max);
      Heartbeat : Short := 60;  -- seconds
   end record;

   -- Connection handle
   type Connection is tagged limited private;

   -- Connection lifecycle
   procedure Connect (
      Conn : in out Connection;
      Config : Connection_Config
   );

   procedure Disconnect (Conn : in out Connection);

   function Is_Connected (Conn : Connection) return Boolean;
   function Get_State (Conn : Connection) return Connection_State;

   -- Frame I/O
   procedure Send_Frame (
      Conn : in out Connection;
      F : Frame
   );

   procedure Receive_Frame (
      Conn : in out Connection;
      F : out Frame;
      Success : out Boolean
   );

   -- Protocol header
   procedure Send_Protocol_Header (Conn : in out Connection);

private

   type Connection is tagged limited record
      Socket : GNAT.Sockets.Socket_Type;
      Address : GNAT.Sockets.Sock_Addr_Type;
      State : Connection_State := Disconnected;
      Config : Connection_Config;
      Receive_Buffer : Buffer;
   end record;

end AMQP.Connection;
