-- Example AMQP Subscriber
-- Subscribes to a subset of topics and prints received messages
with AMQP.Connection; use AMQP.Connection;
with AMQP.Channel; use AMQP.Channel;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

procedure Subscriber is
   Conn : aliased Connection;
   Chan : aliased Channel;
   Config : Connection_Config;
   Msg : Message;
   Success : Boolean;

   type Queue_Info is record
      Name : access String;
      Description : access String;
   end record;

   -- Subscribe to only sensor queues (not log queues)
   Subscribed_Queues : constant array (1 .. 2) of Queue_Info := (
      (new String'("sensor.temperature"), new String'("Temperature sensor data")),
      (new String'("sensor.humidity"), new String'("Humidity sensor data"))
   );

begin
   Put_Line ("AMQP Subscriber Example");
   Put_Line ("=======================");
   Put_Line ("Subscribing to topics:");
   for Q of Subscribed_Queues loop
      Put_Line ("  - " & Q.Name.all & " (" & Q.Description.all & ")");
   end loop;
   Put_Line ("(Not subscribing to log.* topics)");
   New_Line;

   -- Configure connection
   Config.Host := new String'("localhost");
   Config.Port := 5672;
   Config.Virtual_Host := new String'("/");
   Config.Username := new String'("guest");
   Config.Password := new String'("guest");
   Config.Frame_Max := 131072;
   Config.Heartbeat := 60;

   begin
      -- Connect and authenticate
      Put_Line ("Connecting to RabbitMQ...");
      Connect_And_Authenticate (Conn, Config);
      Put_Line ("Connected!");
      New_Line;

      -- Open channel
      Open (Chan, Conn'Unchecked_Access, 1);
      Put_Line ("Channel opened");
      New_Line;

      -- Declare queues and start consumers
      Put_Line ("Setting up subscriptions...");
      for Q of Subscribed_Queues loop
         Queue_Declare (
            Chan,
            Queue => Q.Name.all,
            Durable => False,
            Exclusive => False,
            Auto_Delete => True
         );

         Basic_Consume (
            Chan,
            Queue => Q.Name.all,
            Consumer_Tag => "subscriber_" & Q.Name.all,
            No_Ack => False
         );

         Put_Line ("  Subscribed to: " & Q.Name.all);
      end loop;
      New_Line;

      Put_Line ("Waiting for messages (Ctrl+C to stop)...");
      Put_Line ("------------------------------------------");
      New_Line;

      -- Receive and process messages
      loop
         Basic_Get (Chan, Msg, Success);
         if Success then
            Put_Line ("[" & Msg.Routing_Key.all & "] " & Msg.Content.all);

            -- Acknowledge message
            Basic_Ack (Chan, Msg.Delivery_Tag);
         else
            -- No message available, small delay
            delay 0.1;
         end if;
      end loop;

   exception
      when E : others =>
         Put_Line ("Error: " & Exception_Name (E) & ": " & Exception_Message (E));
         begin
            Close (Chan);
            Disconnect (Conn);
         exception
            when others => null;
         end;
   end;
end Subscriber;
