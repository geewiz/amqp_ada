-- AMQP URL Parsing Implementation
with Ada.Strings.Fixed;
with GNAT.Sockets;

package body AMQP.URL is

   use Ada.Strings.Fixed;

   function Decode_Percent (Encoded : String) return String is
      -- Decode percent-encoded strings (%20, %2F, etc.)
      Result : String (1 .. Encoded'Length);
      Src_Pos : Natural := Encoded'First;
      Dst_Pos : Natural := Result'First;

      function Hex_To_Char (Hex : String) return Character is
         Val : Natural := 0;
      begin
         for I in Hex'Range loop
            Val := Val * 16;
            if Hex (I) in '0' .. '9' then
               Val := Val + (Character'Pos (Hex (I)) - Character'Pos ('0'));
            elsif Hex (I) in 'A' .. 'F' then
               Val := Val + (Character'Pos (Hex (I)) - Character'Pos ('A') + 10);
            elsif Hex (I) in 'a' .. 'f' then
               Val := Val + (Character'Pos (Hex (I)) - Character'Pos ('a') + 10);
            else
               raise Parse_Error with "Invalid hex digit in percent encoding";
            end if;
         end loop;
         return Character'Val (Val);
      end Hex_To_Char;

   begin
      while Src_Pos <= Encoded'Last loop
         if Encoded (Src_Pos) = '%' then
            if Src_Pos + 2 > Encoded'Last then
               raise Parse_Error with "Invalid percent encoding";
            end if;
            Result (Dst_Pos) := Hex_To_Char (Encoded (Src_Pos + 1 .. Src_Pos + 2));
            Src_Pos := Src_Pos + 3;
            Dst_Pos := Dst_Pos + 1;
         else
            Result (Dst_Pos) := Encoded (Src_Pos);
            Src_Pos := Src_Pos + 1;
            Dst_Pos := Dst_Pos + 1;
         end if;
      end loop;

      return Result (Result'First .. Dst_Pos - 1);
   end Decode_Percent;

   function Parse (URL : String) return AMQP.Connection.Connection_Config is
      Pos : Natural;
      Start : Natural := URL'First;

      -- Default values
      Protocol : String (1 .. 10);
      Protocol_Len : Natural := 0;
      Username : String (1 .. 256);
      Username_Len : Natural := 0;
      Password : String (1 .. 256);
      Password_Len : Natural := 0;
      Host : String (1 .. 256);
      Host_Len : Natural := 0;
      Port : GNAT.Sockets.Port_Type := 5672;
      VHost : String (1 .. 256);
      VHost_Len : Natural := 1;

   begin
      -- Initialize vhost to default "/"
      VHost (1) := '/';

      -- Step 1: Parse protocol (amqp:// only - TLS not yet supported)
      Pos := Index (URL, "://");
      if Pos = 0 then
         raise Parse_Error with "Missing protocol (expected amqp://)";
      end if;

      Protocol_Len := Pos - Start;
      if Protocol_Len > Protocol'Length then
         raise Parse_Error with "Protocol name too long";
      end if;
      Protocol (1 .. Protocol_Len) := URL (Start .. Pos - 1);

      if Protocol (1 .. Protocol_Len) = "amqps" then
         raise Parse_Error with "TLS (amqps://) is not yet supported";
      elsif Protocol (1 .. Protocol_Len) /= "amqp" then
         raise Parse_Error with "Protocol must be 'amqp' (amqps not yet supported)";
      end if;

      Start := Pos + 3;  -- Skip "://"

      -- Check if URL is complete (must have something after protocol)
      if Start > URL'Last then
         raise Parse_Error with "URL incomplete after protocol";
      end if;

      -- Step 2: Check for username:password@
      Pos := Index (URL (Start .. URL'Last), "@");
      if Pos > 0 then
         -- Has credentials
         declare
            Creds_Start : constant Natural := Start;
            Creds_End : constant Natural := Pos - 1;
            Colon_Pos : constant Natural := Index (URL (Creds_Start .. Creds_End), ":");
         begin
            if Colon_Pos > 0 then
               -- Both username and password
               Username_Len := Colon_Pos - Creds_Start;
               Username (1 .. Username_Len) := URL (Creds_Start .. Colon_Pos - 1);

               Password_Len := Creds_End - Colon_Pos;
               Password (1 .. Password_Len) := URL (Colon_Pos + 1 .. Creds_End);
            else
               -- Only username
               Username_Len := Creds_End - Creds_Start + 1;
               Username (1 .. Username_Len) := URL (Creds_Start .. Creds_End);
               Password (1 .. 5) := "guest";
               Password_Len := 5;
            end if;
         end;
         Start := Pos + 1;
      else
         -- Use default credentials
         Username (1 .. 5) := "guest";
         Username_Len := 5;
         Password (1 .. 5) := "guest";
         Password_Len := 5;
      end if;

      -- Step 3: Parse host[:port][/vhost]
      -- Find end of host:port part (either '/' or end of string)
      Pos := Index (URL (Start .. URL'Last), "/");

      declare
         Host_End : constant Natural := (if Pos > 0 then Pos - 1 else URL'Last);
         Host_Part : constant String := URL (Start .. Host_End);
         Colon_Pos : constant Natural := Index (Host_Part, ":");
      begin
         if Colon_Pos > 0 then
            -- Has port
            Host_Len := Colon_Pos - Host_Part'First;
            Host (1 .. Host_Len) := Host_Part (Host_Part'First .. Colon_Pos - 1);

            -- Parse port number
            declare
               Port_Str : constant String := Host_Part (Colon_Pos + 1 .. Host_Part'Last);
            begin
               Port := GNAT.Sockets.Port_Type'Value (Port_Str);
            exception
               when Constraint_Error =>
                  raise Parse_Error with "Invalid port number: " & Port_Str;
            end;
         else
            -- No port, use default
            Host_Len := Host_Part'Length;
            Host (1 .. Host_Len) := Host_Part;
         end if;
      end;

      -- Validate host is not empty
      if Host_Len = 0 then
         raise Parse_Error with "Host cannot be empty";
      end if;

      -- Step 4: Parse vhost if present
      if Pos > 0 and then Pos < URL'Last then
         declare
            VHost_Raw : constant String := URL (Pos + 1 .. URL'Last);
            VHost_Decoded : constant String := Decode_Percent (VHost_Raw);
         begin
            VHost_Len := VHost_Decoded'Length;
            if VHost_Len > VHost'Length then
               raise Parse_Error with "Virtual host path too long";
            end if;
            VHost (1 .. VHost_Len) := VHost_Decoded;
         end;
      end if;

      -- Create and return configuration
      return AMQP.Connection.Create_Config (
         Host => Host (1 .. Host_Len),
         Port => Port,
         Virtual_Host => VHost (1 .. VHost_Len),
         Username => Decode_Percent (Username (1 .. Username_Len)),
         Password => Decode_Percent (Password (1 .. Password_Len))
      );

   exception
      when Parse_Error =>
         raise;
      when others =>
         raise Parse_Error with "Failed to parse URL";
   end Parse;

end AMQP.URL;
