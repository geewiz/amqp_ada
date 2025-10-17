-- AMQP 0-9-1 Channel Management
with AMQP.Types; use AMQP.Types;
with AMQP.Connection;

package AMQP.Channel is

   type Connection_Access is access all AMQP.Connection.Connection;

   type Channel_State is (
      Closed,
      Opening,
      Open,
      Closing
   );

   type Channel is tagged limited private;
   type Channel_Access is access all Channel;

   -- Channel lifecycle
   procedure Open (
      Chan : in out Channel;
      Conn : Connection_Access;
      Channel_Number : AMQP.Types.Channel_Number
   );

   procedure Close (Chan : in out Channel);

   function Is_Open (Chan : Channel) return Boolean;
   function Get_State (Chan : Channel) return Channel_State;
   function Get_Channel_Number (Chan : Channel) return AMQP.Types.Channel_Number;

private

   type Channel is tagged limited record
      Conn : Connection_Access;
      Number : Channel_Number := 0;
      State : Channel_State := Closed;
   end record;

end AMQP.Channel;
