-- Connection test program
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with AMQP;
with AMQP.Connection; use AMQP.Connection;

procedure Connection_Test is
   Conn : AMQP.Connection.Connection;
   Config : Connection_Config;
begin
   Put_Line ("AMQP Connection Test");
   Put_Line ("====================");
   Put_Line ("");

   -- Configure connection using factory function
   Config := Create_Config (Host => "localhost");

   Put_Line ("Attempting to connect to " & Config.Host.all & ":5672");

   begin
      -- Connect to broker
      Connect (Conn, Config);
      Put_Line ("[PASS] TCP connection established");
      Put_Line ("  State: " & Connection_State'Image (Get_State (Conn)));

      -- Send protocol header
      Send_Protocol_Header (Conn);
      Put_Line ("[PASS] Protocol header sent");

      -- Disconnect
      Disconnect (Conn);
      Put_Line ("[PASS] Disconnected cleanly");

   exception
      when E : AMQP.Connection_Error =>
         Put_Line ("[FAIL] Connection error: " & Ada.Exceptions.Exception_Message (E));
         Put_Line ("");
         Put_Line ("Note: This test requires RabbitMQ running on localhost:5672");
         Put_Line ("      Start RabbitMQ with: docker run -d -p 5672:5672 rabbitmq");

      when E : others =>
         Put_Line ("[FAIL] Unexpected error: " & Ada.Exceptions.Exception_Message (E));
   end;

   Put_Line ("");
   Put_Line ("Connection test complete");
end Connection_Test;
