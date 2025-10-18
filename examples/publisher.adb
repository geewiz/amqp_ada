-- Example AMQP Publisher
-- Publishes messages to multiple topics in an endless loop
with AMQP.Connection; use AMQP.Connection;
with AMQP.Channel; use AMQP.Channel;
with AMQP.URL;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

procedure Publisher is
   Conn : aliased Connection;
   Chan : aliased Channel (Conn'Access);
   Config : Connection_Config;
   Message_Count : Natural := 0;

   type Topic_Info is record
      Queue_Name : access String;
      Message_Template : access String;
   end record;

   -- Publish to 4 different queues
   -- Subscriber will only consume from sensor.* queues
   Topics : constant array (1 .. 4) of Topic_Info := (
      (new String'("sensor.temperature"), new String'("Temperature reading: ")),
      (new String'("sensor.humidity"), new String'("Humidity reading: ")),
      (new String'("log.info"), new String'("INFO: System status: ")),
      (new String'("log.error"), new String'("ERROR: Alert condition: "))
   );

begin
   Put_Line ("AMQP Publisher Example");
   Put_Line ("======================");
   Put_Line ("Publishing to queues:");
   for Topic of Topics loop
      Put_Line ("  - " & Topic.Queue_Name.all);
   end loop;
   New_Line;

   -- Configure connection from URL
   Config := AMQP.URL.Parse ("amqp://guest:guest@localhost:5672/");

   begin
      -- Connect and authenticate
      Put_Line ("Connecting to RabbitMQ...");
      Connect_And_Authenticate (Conn, Config);
      Put_Line ("Connected!");
      New_Line;

      -- Open channel
      Open (Chan, 1);
      Put_Line ("Channel opened");
      New_Line;

      -- Publish messages in a loop
      Put_Line ("Publishing messages (Ctrl+C to stop)...");
      New_Line;

      loop
         for Topic of Topics loop
            Message_Count := Message_Count + 1;

            declare
               Message : constant String :=
                  Topic.Message_Template.all &
                  Natural'Image (Message_Count);
            begin
               Basic_Publish (
                  Chan         => Chan,
                  Exchange     => "",  -- Default exchange
                  Routing_Key  => Topic.Queue_Name.all,
                  Message_Body => Message
               );

               Put_Line ("[" & Topic.Queue_Name.all & "] " & Message);
            end;

            -- Small delay between messages
            delay 0.5;
         end loop;

         New_Line;
         delay 1.0;  -- Pause between rounds
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
end Publisher;
