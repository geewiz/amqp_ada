-- AMQP Connection Handshake Test
with AMQP.Connection; use AMQP.Connection;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

procedure Handshake_Test is
   Conn : Connection;
   Config : Connection_Config;
begin
   Put_Line ("AMQP Connection Handshake Test");
   Put_Line ("================================");
   New_Line;

   -- Configure connection
   Config.Host := new String'("localhost");
   Config.Port := 5672;
   Config.Virtual_Host := new String'("/");
   Config.Username := new String'("guest");
   Config.Password := new String'("guest");
   Config.Frame_Max := 131072;  -- 128KB
   Config.Heartbeat := 60;

   Put_Line ("Connecting to " & Config.Host.all & ":" & Config.Port'Image);
   New_Line;

   begin
      -- Establish TCP connection
      Connect (Conn, Config);
      Put_Line ("[PASS] TCP connection established");

      -- Send protocol header
      Send_Protocol_Header (Conn);
      Put_Line ("[PASS] Protocol header sent");
      New_Line;

      -- Perform handshake
      Perform_Handshake (Conn);
      New_Line;

      Put_Line ("[PASS] Connection handshake successful!");
      Put_Line ("  State: " & Get_State (Conn)'Image);

      -- Disconnect
      Disconnect (Conn);
      Put_Line ("[PASS] Disconnected cleanly");

   exception
      when E : others =>
         Put_Line ("[FAIL] " & Exception_Name (E) & ": " & Exception_Message (E));
         Disconnect (Conn);
   end;

   New_Line;
   Put_Line ("Handshake test complete");
end Handshake_Test;
