-- AMQP 0-9-1 Channel Management
with AMQP.Types; use AMQP.Types;
with AMQP.Connection;
with AMQP.Methods;

package AMQP.Channel is

   type Channel_State is (
      Closed,
      Opening,
      Open,
      Closing
   );

   type Channel (Conn : access AMQP.Connection.Connection) is tagged limited private;
   type Channel_Access is access all Channel;

   -- Channel lifecycle
   procedure Open (
      Chan : in out Channel;
      Channel_Number : AMQP.Types.Channel_Number
   );

   procedure Close (Chan : in out Channel);

   function Is_Open (Chan : Channel) return Boolean;
   function Get_State (Chan : Channel) return Channel_State;
   function Get_Channel_Number (Chan : Channel) return AMQP.Types.Channel_Number;

   -- Queue operations
   procedure Queue_Declare (
      Chan : in out Channel;
      Queue : String;
      Durable : Boolean := False;
      Exclusive : Boolean := False;
      Auto_Delete : Boolean := False
   );

   -- Basic operations
   procedure Basic_Publish (
      Chan : in out Channel;
      Exchange : String;
      Routing_Key : String;
      Message_Body : String
   );

   type Message is record
      Content : Methods.String_Access;
      Delivery_Tag : Long_Long;
      Exchange : Methods.String_Access;
      Routing_Key : Methods.String_Access;
   end record;

   procedure Basic_Consume (
      Chan : in out Channel;
      Queue : String;
      Consumer_Tag : String;
      No_Ack : Boolean := False
   );

   procedure Basic_Get (
      Chan : in out Channel;
      Msg : out Message;
      Success : out Boolean
   );

   procedure Basic_Ack (
      Chan : in out Channel;
      Delivery_Tag : Long_Long
   );

private

   type Channel (Conn : access AMQP.Connection.Connection) is tagged limited record
      Number : Channel_Number := 0;
      State : Channel_State := Closed;
   end record;

end AMQP.Channel;
