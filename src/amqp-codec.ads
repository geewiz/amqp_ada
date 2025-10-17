-- AMQP 0-9-1 Encoding and Decoding
with AMQP.Types; use AMQP.Types;
with Ada.Streams;

package AMQP.Codec is

   type Buffer is record
      Data : Ada.Streams.Stream_Element_Array (1 .. 65536);
      Position : Ada.Streams.Stream_Element_Offset := 1;
      Length : Ada.Streams.Stream_Element_Offset := 0;
   end record;

   -- Encoding operations (write to buffer)
   procedure Encode_Octet (Buf : in out Buffer; Value : Octet);
   procedure Encode_Short (Buf : in out Buffer; Value : Short);
   procedure Encode_Long (Buf : in out Buffer; Value : Long);
   procedure Encode_Long_Long (Buf : in out Buffer; Value : Long_Long);
   procedure Encode_Short_String (Buf : in out Buffer; Value : String);
   procedure Encode_Long_String (Buf : in out Buffer; Value : String);
   procedure Encode_Field_Table (Buf : in out Buffer; Value : Field_Table);

   -- Decoding operations (read from buffer)
   procedure Decode_Octet (Buf : in out Buffer; Value : out Octet);
   procedure Decode_Short (Buf : in out Buffer; Value : out Short);
   procedure Decode_Long (Buf : in out Buffer; Value : out Long);
   procedure Decode_Long_Long (Buf : in out Buffer; Value : out Long_Long);
   procedure Decode_Short_String (Buf : in out Buffer; Value : out Short_String);
   procedure Decode_Long_String (Buf : in out Buffer; Value : out Long_String);
   procedure Decode_Field_Table (Buf : in out Buffer; Value : out Field_Table);

   -- Buffer management
   procedure Reset (Buf : in out Buffer);
   function Available (Buf : Buffer) return Ada.Streams.Stream_Element_Offset;
   function Remaining (Buf : Buffer) return Ada.Streams.Stream_Element_Offset;

end AMQP.Codec;
