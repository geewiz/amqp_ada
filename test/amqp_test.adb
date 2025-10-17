-- Simple test program for AMQP library
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams;
with Interfaces; use Interfaces;
with AMQP;
with AMQP.Constants;
with AMQP.Types; use AMQP.Types;
with AMQP.Codec; use AMQP.Codec;
with AMQP.Frames; use AMQP.Frames;

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
      Put_Line ("  [PASS] Codec test passed");
   else
      Put_Line ("  [FAIL] Codec test failed");
   end if;

   -- Test frame processing
   Put_Line ("");
   Put_Line ("Testing frame processing:");

   declare
      Test_Payload : constant Ada.Streams.Stream_Element_Array (1 .. 4) :=
         (16#01#, 16#02#, 16#03#, 16#04#);
      Frame_Out : Frame;
      Frame_In : Frame;
      Frame_Buf : Buffer;
      Success : Boolean;
   begin
      -- Create a heartbeat frame
      Frame_Out := Make_Heartbeat_Frame;
      Put_Line ("  Created heartbeat frame");

      -- Encode it
      Reset (Frame_Buf);
      Encode_Frame (Frame_Buf, Frame_Out);
      Put_Line ("  Encoded frame size:" &
                Ada.Streams.Stream_Element_Offset'Image (Frame_Buf.Length));

      -- Decode it
      Reset (Frame_Buf);
      Frame_Buf.Length := Frame_Buf.Data'Last;  -- Simulate received data
      Decode_Frame (Frame_Buf, Frame_In, Success);

      if Success and then Frame_In.Kind = Heartbeat_Frame then
         Put_Line ("  [PASS] Heartbeat frame encode/decode");
      else
         Put_Line ("  [FAIL] Heartbeat frame encode/decode");
      end if;

      -- Test method frame with payload
      Frame_Out := Make_Method_Frame (Channel => 1, Payload => Test_Payload);
      Reset (Frame_Buf);
      Encode_Frame (Frame_Buf, Frame_Out);
      Put_Line ("  Method frame size:" &
                Ada.Streams.Stream_Element_Offset'Image (Frame_Buf.Length));

      Reset (Frame_Buf);
      Frame_Buf.Length := Frame_Buf.Data'Last;
      Decode_Frame (Frame_Buf, Frame_In, Success);

      if Success and then
         Frame_In.Kind = Method_Frame and then
         Frame_In.Channel = 1 and then
         Natural (Frame_In.Payload.Length) = 4
      then
         Put_Line ("  [PASS] Method frame encode/decode");
      else
         Put_Line ("  [FAIL] Method frame encode/decode");
      end if;
   end;

   Put_Line ("");
   Put_Line ("Next steps:");
   Put_Line ("  1. Add socket connection management");
   Put_Line ("  2. Implement connection handshake");
   Put_Line ("  3. Build channel management");
   Put_Line ("  4. Add publish/consume operations");

end AMQP_Test;
