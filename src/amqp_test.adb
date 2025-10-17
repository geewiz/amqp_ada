-- Simple test program for AMQP library
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams;
with Interfaces; use Interfaces;
with AMQP;
with AMQP.Constants;
with AMQP.Types; use AMQP.Types;
with AMQP.Codec; use AMQP.Codec;

procedure AMQP_Test is
   Buf : Buffer;
   Test_Value : constant Short := 1234;
   Decoded_Value : Short;
begin
   Put_Line ("AMQP Ada Library Test - Version " & AMQP.VERSION);
   Put_Line ("Protocol: " & Integer'Image (AMQP.Constants.PROTOCOL_VERSION_MAJOR) &
             "." & Integer'Image (AMQP.Constants.PROTOCOL_VERSION_MINOR) &
             "." & Integer'Image (AMQP.Constants.PROTOCOL_VERSION_REVISION));

   -- Test basic encoding/decoding
   Put_Line ("");
   Put_Line ("Testing codec:");
   Put_Line ("  Encoding Short value:" & Short'Image (Test_Value));

   Reset (Buf);
   Encode_Short (Buf, Test_Value);
   Put_Line ("  Buffer length:" & Ada.Streams.Stream_Element_Offset'Image (Buf.Length));

   Reset (Buf);
   Buf.Length := 2;  -- Simulate received data
   Decode_Short (Buf, Decoded_Value);
   Put_Line ("  Decoded value:" & Short'Image (Decoded_Value));

   if Decoded_Value = Test_Value then
      Put_Line ("  ✓ Codec test passed!");
   else
      Put_Line ("  ✗ Codec test failed!");
   end if;

   Put_Line ("");
   Put_Line ("Basic infrastructure ready. Next steps:");
   Put_Line ("  1. Implement frame processing");
   Put_Line ("  2. Add socket connection management");
   Put_Line ("  3. Implement connection handshake");
   Put_Line ("  4. Build channel management");
   Put_Line ("  5. Add publish/consume operations");

end AMQP_Test;
