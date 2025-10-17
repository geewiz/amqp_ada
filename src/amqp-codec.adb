-- AMQP 0-9-1 Encoding and Decoding implementation
with Interfaces; use Interfaces;
with Ada.Streams; use Ada.Streams;

package body AMQP.Codec is

   procedure Encode_Octet (Buf : in out Buffer; Value : Octet) is
   begin
      if Buf.Position > Buf.Data'Last then
         raise Encoding_Error with "Buffer overflow";
      end if;
      Buf.Data (Buf.Position) := Ada.Streams.Stream_Element (Value);
      Buf.Position := Buf.Position + 1;
      if Buf.Position - 1 > Buf.Length then
         Buf.Length := Buf.Position - 1;
      end if;
   end Encode_Octet;

   procedure Encode_Short (Buf : in out Buffer; Value : Short) is
   begin
      -- Big-endian encoding
      Encode_Octet (Buf, Octet (Shift_Right (Value, 8) and 16#FF#));
      Encode_Octet (Buf, Octet (Value and 16#FF#));
   end Encode_Short;

   procedure Encode_Long (Buf : in out Buffer; Value : Long) is
   begin
      -- Big-endian encoding
      Encode_Octet (Buf, Octet (Shift_Right (Value, 24) and 16#FF#));
      Encode_Octet (Buf, Octet (Shift_Right (Value, 16) and 16#FF#));
      Encode_Octet (Buf, Octet (Shift_Right (Value, 8) and 16#FF#));
      Encode_Octet (Buf, Octet (Value and 16#FF#));
   end Encode_Long;

   procedure Encode_Long_Long (Buf : in out Buffer; Value : Long_Long) is
   begin
      -- Big-endian encoding
      Encode_Octet (Buf, Octet (Shift_Right (Value, 56) and 16#FF#));
      Encode_Octet (Buf, Octet (Shift_Right (Value, 48) and 16#FF#));
      Encode_Octet (Buf, Octet (Shift_Right (Value, 40) and 16#FF#));
      Encode_Octet (Buf, Octet (Shift_Right (Value, 32) and 16#FF#));
      Encode_Octet (Buf, Octet (Shift_Right (Value, 24) and 16#FF#));
      Encode_Octet (Buf, Octet (Shift_Right (Value, 16) and 16#FF#));
      Encode_Octet (Buf, Octet (Shift_Right (Value, 8) and 16#FF#));
      Encode_Octet (Buf, Octet (Value and 16#FF#));
   end Encode_Long_Long;

   procedure Encode_Short_String (Buf : in out Buffer; Value : String) is
   begin
      if Value'Length > 255 then
         raise Encoding_Error with "Short string too long";
      end if;
      Encode_Octet (Buf, Octet (Value'Length));
      for C of Value loop
         Encode_Octet (Buf, Character'Pos (C));
      end loop;
   end Encode_Short_String;

   procedure Encode_Long_String (Buf : in out Buffer; Value : String) is
   begin
      Encode_Long (Buf, Long (Value'Length));
      for C of Value loop
         Encode_Octet (Buf, Character'Pos (C));
      end loop;
   end Encode_Long_String;

   procedure Encode_Field_Table (Buf : in out Buffer; Value : Field_Table) is
      -- Simplified implementation - to be expanded
      Start_Pos : constant Stream_Element_Offset := Buf.Position;
   begin
      -- Reserve space for length
      Encode_Long (Buf, 0);

      -- Encode entries (simplified - needs full implementation)
      declare
         Entry_Ptr : Field_Table_Entry_Access := Value.Head;
      begin
         while Entry_Ptr /= null loop
            -- Encode field name and value
            -- (This is a placeholder - needs full type encoding)
            Entry_Ptr := Entry_Ptr.Next;
         end loop;
      end;

      -- Update length field (TODO: Implement proper length backfill)
      pragma Unreferenced (Start_Pos);
   end Encode_Field_Table;

   procedure Decode_Octet (Buf : in out Buffer; Value : out Octet) is
   begin
      if Buf.Position > Buf.Length then
         raise Encoding_Error with "Buffer underflow";
      end if;
      Value := Octet (Buf.Data (Buf.Position));
      Buf.Position := Buf.Position + 1;
   end Decode_Octet;

   procedure Decode_Short (Buf : in out Buffer; Value : out Short) is
      B1, B2 : Octet;
   begin
      Decode_Octet (Buf, B1);
      Decode_Octet (Buf, B2);
      Value := Shift_Left (Short (B1), 8) or Short (B2);
   end Decode_Short;

   procedure Decode_Long (Buf : in out Buffer; Value : out Long) is
      B1, B2, B3, B4 : Octet;
   begin
      Decode_Octet (Buf, B1);
      Decode_Octet (Buf, B2);
      Decode_Octet (Buf, B3);
      Decode_Octet (Buf, B4);
      Value := Shift_Left (Long (B1), 24) or
               Shift_Left (Long (B2), 16) or
               Shift_Left (Long (B3), 8) or
               Long (B4);
   end Decode_Long;

   procedure Decode_Long_Long (Buf : in out Buffer; Value : out Long_Long) is
      B1, B2, B3, B4, B5, B6, B7, B8 : Octet;
   begin
      Decode_Octet (Buf, B1);
      Decode_Octet (Buf, B2);
      Decode_Octet (Buf, B3);
      Decode_Octet (Buf, B4);
      Decode_Octet (Buf, B5);
      Decode_Octet (Buf, B6);
      Decode_Octet (Buf, B7);
      Decode_Octet (Buf, B8);
      Value := Shift_Left (Long_Long (B1), 56) or
               Shift_Left (Long_Long (B2), 48) or
               Shift_Left (Long_Long (B3), 40) or
               Shift_Left (Long_Long (B4), 32) or
               Shift_Left (Long_Long (B5), 24) or
               Shift_Left (Long_Long (B6), 16) or
               Shift_Left (Long_Long (B7), 8) or
               Long_Long (B8);
   end Decode_Long_Long;

   procedure Decode_Short_String (Buf : in out Buffer; Value : out Short_String) is
      Len : Octet;
   begin
      Decode_Octet (Buf, Len);
      declare
         Str : String (1 .. Natural (Len));
      begin
         for I in Str'Range loop
            declare
               B : Octet;
            begin
               Decode_Octet (Buf, B);
               Str (I) := Character'Val (B);
            end;
         end loop;
         Value := new String'(Str);
      end;
   end Decode_Short_String;

   procedure Decode_Long_String (Buf : in out Buffer; Value : out Long_String) is
      Len : Long;
   begin
      Decode_Long (Buf, Len);
      declare
         Str : String (1 .. Natural (Len));
      begin
         for I in Str'Range loop
            declare
               B : Octet;
            begin
               Decode_Octet (Buf, B);
               Str (I) := Character'Val (B);
            end;
         end loop;
         Value := new String'(Str);
      end;
   end Decode_Long_String;

   procedure Decode_Field_Table (Buf : in out Buffer; Value : out Field_Table) is
      Len : Long;
   begin
      Decode_Long (Buf, Len);
      -- Simplified implementation - to be expanded
      Value := (Head => null);
   end Decode_Field_Table;

   procedure Reset (Buf : in out Buffer) is
   begin
      Buf.Position := 1;
      Buf.Length := 0;
   end Reset;

   function Available (Buf : Buffer) return Ada.Streams.Stream_Element_Offset is
   begin
      return Buf.Length - Buf.Position + 1;
   end Available;

   function Remaining (Buf : Buffer) return Ada.Streams.Stream_Element_Offset is
   begin
      return Buf.Data'Last - Buf.Position + 1;
   end Remaining;

end AMQP.Codec;
