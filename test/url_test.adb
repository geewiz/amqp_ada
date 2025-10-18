-- AMQP URL Parsing Test
with AMQP.URL;
with AMQP.Connection; use AMQP.Connection;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

procedure URL_Test is

   procedure Test_URL (Description : String; URL : String; Should_Fail : Boolean := False) is
      Config : Connection_Config;
   begin
      Put ("  [TEST] " & Description);
      New_Line;
      Put_Line ("    URL: " & URL);

      begin
         Config := AMQP.URL.Parse (URL);

         if Should_Fail then
            Put_Line ("    [FAIL] Should have raised Parse_Error but succeeded");
         else
            Put_Line ("    [PASS] Parsed successfully");
            Put_Line ("      Host: " & Config.Host.all);
            Put_Line ("      Port:" & Config.Port'Image);
            Put_Line ("      Virtual Host: " & Config.Virtual_Host.all);
            Put_Line ("      Username: " & Config.Username.all);
            Put_Line ("      Password: " & Config.Password.all);
         end if;

      exception
         when E : AMQP.URL.Parse_Error =>
            if Should_Fail then
               Put_Line ("    [PASS] Correctly rejected: " & Exception_Message (E));
            else
               Put_Line ("    [FAIL] Parse error: " & Exception_Message (E));
            end if;

         when E : others =>
            Put_Line ("    [FAIL] Unexpected error: " & Exception_Name (E) &
                     ": " & Exception_Message (E));
      end;

      New_Line;
   end Test_URL;

begin
   Put_Line ("AMQP URL Parsing Test");
   Put_Line ("=====================");
   New_Line;

   -- Valid URL tests
   Put_Line ("Valid URLs:");
   Put_Line ("-----------");
   New_Line;

   Test_URL (
      Description => "Minimal URL",
      URL => "amqp://localhost"
   );

   Test_URL (
      Description => "URL with port",
      URL => "amqp://localhost:5672"
   );

   Test_URL (
      Description => "URL with custom port",
      URL => "amqp://localhost:15672"
   );

   Test_URL (
      Description => "URL with credentials",
      URL => "amqp://guest:guest@localhost"
   );

   Test_URL (
      Description => "URL with credentials and port",
      URL => "amqp://myuser:mypass@localhost:5672"
   );

   Test_URL (
      Description => "URL with virtual host",
      URL => "amqp://localhost/"
   );

   Test_URL (
      Description => "URL with custom virtual host",
      URL => "amqp://localhost/myvhost"
   );

   Test_URL (
      Description => "Full URL",
      URL => "amqp://admin:secret@rabbitmq.example.com:5672/production"
   );

   Test_URL (
      Description => "URL with percent-encoded virtual host",
      URL => "amqp://localhost/%2Fvhost"
   );

   Test_URL (
      Description => "URL with percent-encoded password",
      URL => "amqp://user:p%40ss@localhost"
   );

   -- Invalid URL tests
   Put_Line ("Invalid URLs (should fail):");
   Put_Line ("---------------------------");
   New_Line;

   Test_URL (
      Description => "Missing protocol",
      URL => "localhost:5672",
      Should_Fail => True
   );

   Test_URL (
      Description => "Wrong protocol",
      URL => "http://localhost:5672",
      Should_Fail => True
   );

   Test_URL (
      Description => "AMQPS protocol (TLS not yet supported)",
      URL => "amqps://localhost:5671",
      Should_Fail => True
   );

   Test_URL (
      Description => "Empty URL after protocol",
      URL => "amqp://",
      Should_Fail => True
   );

   Test_URL (
      Description => "Invalid port (non-numeric)",
      URL => "amqp://localhost:abc",
      Should_Fail => True
   );

   Test_URL (
      Description => "Invalid port (too large)",
      URL => "amqp://localhost:99999",
      Should_Fail => True
   );

   Test_URL (
      Description => "Invalid percent encoding",
      URL => "amqp://localhost/%ZZ",
      Should_Fail => True
   );

   Put_Line ("URL parsing test complete");

end URL_Test;
