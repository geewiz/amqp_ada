-- AMQP Publish/Subscribe Test
with AMQP.Connection; use AMQP.Connection;
with AMQP.Channel; use AMQP.Channel;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

procedure Pubsub_Test is
   Conn : aliased Connection;
   Config : Connection_Config;
   Chan : aliased Channel;
   Msg : Message;
   Success : Boolean;
begin
   Put_Line ("AMQP Publish/Subscribe Test");
   Put_Line ("===========================");
   New_Line;

   -- Configure connection using factory function
   Config := Create_Config (Host => "localhost");

   Put_Line ("Connecting to " & Config.Host.all & ":" & Config.Port'Image);

   begin
      -- Establish connection
      Connect_And_Authenticate (Conn, Config);
      Put_Line ("[PASS] Connection established");
      New_Line;

      -- Open channel
      Open (Chan, Conn'Unchecked_Access, 1);
      Put_Line ("[PASS] Channel opened");
      New_Line;

      -- Declare a queue
      Put_Line ("Declaring queue 'test_queue'...");
      Queue_Declare (
         Chan,
         Queue => "test_queue",
         Durable => False,
         Exclusive => False,
         Auto_Delete => True
      );
      Put_Line ("[PASS] Queue declared");
      New_Line;

      -- Publish a message
      Put_Line ("Publishing message...");
      Basic_Publish (
         Chan         => Chan,
         Exchange     => "",
         Routing_Key  => "test_queue",
         Message_Body => "Hello from Ada AMQP!"
      );
      Put_Line ("[PASS] Message published");
      New_Line;

      -- Start consuming
      Put_Line ("Starting consumer...");
      Basic_Consume (
         Chan,
         Queue => "test_queue",
         Consumer_Tag => "ada_consumer",
         No_Ack => False
      );
      Put_Line ("[PASS] Consumer started");
      New_Line;

      -- Receive the message
      Put_Line ("Waiting for message...");
      Basic_Get (Chan, Msg, Success);
      if Success then
         Put_Line ("[PASS] Message received");
         Put_Line ("  Content: " & Msg.Content.all);
         Put_Line ("  Delivery Tag:" & Msg.Delivery_Tag'Image);
         Put_Line ("  Exchange: " & Msg.Exchange.all);
         Put_Line ("  Routing Key: " & Msg.Routing_Key.all);
         New_Line;

         -- Acknowledge the message
         Put_Line ("Acknowledging message...");
         Basic_Ack (Chan, Msg.Delivery_Tag);
         Put_Line ("[PASS] Message acknowledged");
         New_Line;
      else
         Put_Line ("[FAIL] Failed to receive message");
      end if;

      -- Close channel
      Close (Chan);
      Put_Line ("[PASS] Channel closed");
      New_Line;

      -- Disconnect
      Disconnect (Conn);
      Put_Line ("[PASS] Disconnected cleanly");

   exception
      when E : others =>
         Put_Line ("[FAIL] " & Exception_Name (E) & ": " & Exception_Message (E));
         begin
            Close (Chan);
            Disconnect (Conn);
         exception
            when others => null;  -- Best effort cleanup
         end;
   end;

   New_Line;
   Put_Line ("Publish/Subscribe test complete");
end Pubsub_Test;
