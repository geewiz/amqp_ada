-- AMQP Channel Management Test
with AMQP.Connection; use AMQP.Connection;
with AMQP.Channel; use AMQP.Channel;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

procedure Channel_Test is
   Conn : aliased Connection;
   Config : Connection_Config;
   Chan1 : aliased Channel;
   Chan2 : aliased Channel;
begin
   Put_Line ("AMQP Channel Management Test");
   Put_Line ("=============================");
   New_Line;

   -- Configure connection using factory function
   Config := Create_Config (Host => "localhost");

   Put_Line ("Connecting to " & Config.Host.all & ":" & Config.Port'Image);

   begin
      -- Establish connection
      Connect_And_Authenticate (Conn, Config);
      Put_Line ("[PASS] Connection established");
      New_Line;

      -- Open first channel
      Open (Chan1, Conn'Unchecked_Access, 1);
      Put_Line ("[PASS] Channel 1 opened");
      Put_Line ("  State: " & Get_State (Chan1)'Image);
      Put_Line ("  Is Open: " & Is_Open (Chan1)'Image);
      New_Line;

      -- Open second channel
      Open (Chan2, Conn'Unchecked_Access, 2);
      Put_Line ("[PASS] Channel 2 opened");
      Put_Line ("  State: " & Get_State (Chan2)'Image);
      Put_Line ("  Is Open: " & Is_Open (Chan2)'Image);
      New_Line;

      -- Close channels
      Close (Chan1);
      Put_Line ("[PASS] Channel 1 closed");
      Put_Line ("  Is Open: " & Is_Open (Chan1)'Image);
      New_Line;

      Close (Chan2);
      Put_Line ("[PASS] Channel 2 closed");
      Put_Line ("  Is Open: " & Is_Open (Chan2)'Image);
      New_Line;

      -- Disconnect
      Disconnect (Conn);
      Put_Line ("[PASS] Disconnected cleanly");

   exception
      when E : others =>
         Put_Line ("[FAIL] " & Exception_Name (E) & ": " & Exception_Message (E));
         begin
            Close (Chan1);
            Close (Chan2);
            Disconnect (Conn);
         exception
            when others => null;  -- Best effort cleanup
         end;
   end;

   New_Line;
   Put_Line ("Channel test complete");
end Channel_Test;
