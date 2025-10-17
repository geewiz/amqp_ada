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

   procedure Encode_Field_Value (Buf : in out Buffer; Value : Field_Value_Access);

   procedure Encode_Field_Value (Buf : in out Buffer; Value : Field_Value_Access) is
   begin
      if Value = null then
         Encode_Octet (Buf, Character'Pos ('V'));  -- Void
         return;
      end if;

      case Value.Value_Type is
         when FV_Boolean =>
            Encode_Octet (Buf, Character'Pos ('t'));
            Encode_Octet (Buf, (if Value.Boolean_Value then 1 else 0));

         when FV_Short_Short_Int =>
            Encode_Octet (Buf, Character'Pos ('b'));
            Encode_Octet (Buf, Octet (Unsigned_8 (Value.Short_Short_Int_Value) and 16#FF#));

         when FV_Short_Short_Uint =>
            Encode_Octet (Buf, Character'Pos ('B'));
            Encode_Octet (Buf, Value.Short_Short_Uint_Value);

         when FV_Short_Int =>
            Encode_Octet (Buf, Character'Pos ('s'));
            Encode_Short (Buf, Short (Unsigned_16 (Value.Short_Int_Value) and 16#FFFF#));

         when FV_Short_Uint =>
            Encode_Octet (Buf, Character'Pos ('u'));
            Encode_Short (Buf, Value.Short_Uint_Value);

         when FV_Long_Int =>
            Encode_Octet (Buf, Character'Pos ('I'));
            Encode_Long (Buf, Long (Value.Long_Int_Value));

         when FV_Long_Uint =>
            Encode_Octet (Buf, Character'Pos ('i'));
            Encode_Long (Buf, Value.Long_Uint_Value);

         when FV_Long_Long_Int =>
            Encode_Octet (Buf, Character'Pos ('l'));
            Encode_Long_Long (Buf, Long_Long (Value.Long_Long_Int_Value));

         when FV_Long_Long_Uint =>
            Encode_Octet (Buf, Character'Pos ('L'));
            Encode_Long_Long (Buf, Value.Long_Long_Uint_Value);

         when FV_Float =>
            Encode_Octet (Buf, Character'Pos ('f'));
            -- Float encoding requires bit manipulation - simplified for now
            raise Encoding_Error with "Float encoding not yet implemented";

         when FV_Double =>
            Encode_Octet (Buf, Character'Pos ('d'));
            -- Double encoding requires bit manipulation - simplified for now
            raise Encoding_Error with "Double encoding not yet implemented";

         when FV_Short_String =>
            Encode_Octet (Buf, Character'Pos ('s'));
            if Value.Short_String_Value /= null then
               Encode_Short_String (Buf, Value.Short_String_Value.all);
            else
               Encode_Short_String (Buf, "");
            end if;

         when FV_Long_String =>
            Encode_Octet (Buf, Character'Pos ('S'));
            if Value.Long_String_Value /= null then
               Encode_Long_String (Buf, Value.Long_String_Value.all);
            else
               Encode_Long_String (Buf, "");
            end if;

         when FV_Timestamp =>
            Encode_Octet (Buf, Character'Pos ('T'));
            Encode_Long_Long (Buf, Value.Timestamp_Value);

         when FV_Field_Table =>
            Encode_Octet (Buf, Character'Pos ('F'));
            if Value.Field_Table_Value /= null then
               Encode_Field_Table (Buf, Value.Field_Table_Value.all);
            else
               Encode_Long (Buf, 0);  -- Empty table
            end if;

         when FV_Void =>
            Encode_Octet (Buf, Character'Pos ('V'));

         when FV_Field_Array | FV_Decimal =>
            raise Encoding_Error with "Field array/decimal encoding not yet implemented";
      end case;
   end Encode_Field_Value;

   procedure Encode_Field_Table (Buf : in out Buffer; Value : Field_Table) is
      Length_Pos : constant Stream_Element_Offset := Buf.Position;
      Start_Pos : Stream_Element_Offset;
      Table_Length : Long;
   begin
      -- Reserve space for length
      Encode_Long (Buf, 0);
      Start_Pos := Buf.Position;

      -- Encode entries
      declare
         Entry_Ptr : Field_Table_Entry_Access := Value.Head;
      begin
         while Entry_Ptr /= null loop
            -- Encode field name (as short string)
            if Entry_Ptr.Name /= null then
               Encode_Short_String (Buf, Entry_Ptr.Name.all);
            else
               Encode_Short_String (Buf, "");
            end if;

            -- Encode field value with type indicator
            Encode_Field_Value (Buf, Entry_Ptr.Value);

            Entry_Ptr := Entry_Ptr.Next;
         end loop;
      end;

      -- Calculate and backfill the length
      Table_Length := Long (Buf.Position - Start_Pos);

      -- Save current position and update length field
      declare
         Current_Pos : constant Stream_Element_Offset := Buf.Position;
      begin
         Buf.Position := Length_Pos;
         Encode_Long (Buf, Table_Length);
         Buf.Position := Current_Pos;
      end;
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

   procedure Decode_Field_Value (Buf : in out Buffer; Value : out Field_Value_Access);

   procedure Decode_Field_Value (Buf : in out Buffer; Value : out Field_Value_Access) is
      Type_Indicator : Octet;
   begin
      Decode_Octet (Buf, Type_Indicator);

      case Character'Val (Type_Indicator) is
         when 't' =>  -- Boolean
            declare
               B : Octet;
            begin
               Decode_Octet (Buf, B);
               Value := new Field_Value'(Value_Type => FV_Boolean,
                                        Boolean_Value => (B /= 0));
            end;

         when 'b' =>  -- Short-short-int
            declare
               B : Octet;
            begin
               Decode_Octet (Buf, B);
               Value := new Field_Value'(Value_Type => FV_Short_Short_Int,
                                        Short_Short_Int_Value => Integer_8 (B));
            end;

         when 'B' =>  -- Short-short-uint
            declare
               B : Octet;
            begin
               Decode_Octet (Buf, B);
               Value := new Field_Value'(Value_Type => FV_Short_Short_Uint,
                                        Short_Short_Uint_Value => B);
            end;

         when 's' =>  -- Short-int
            declare
               S : Short;
            begin
               Decode_Short (Buf, S);
               Value := new Field_Value'(Value_Type => FV_Short_Int,
                                        Short_Int_Value => Integer_16 (S));
            end;

         when 'u' =>  -- Short-uint
            declare
               S : Short;
            begin
               Decode_Short (Buf, S);
               Value := new Field_Value'(Value_Type => FV_Short_Uint,
                                        Short_Uint_Value => S);
            end;

         when 'I' =>  -- Long-int
            declare
               L : Long;
            begin
               Decode_Long (Buf, L);
               Value := new Field_Value'(Value_Type => FV_Long_Int,
                                        Long_Int_Value => Integer_32 (L));
            end;

         when 'i' =>  -- Long-uint
            declare
               L : Long;
            begin
               Decode_Long (Buf, L);
               Value := new Field_Value'(Value_Type => FV_Long_Uint,
                                        Long_Uint_Value => L);
            end;

         when 'l' =>  -- Long-long-int
            declare
               LL : Long_Long;
            begin
               Decode_Long_Long (Buf, LL);
               Value := new Field_Value'(Value_Type => FV_Long_Long_Int,
                                        Long_Long_Int_Value => Integer_64 (LL));
            end;

         when 'L' =>  -- Long-long-uint
            declare
               LL : Long_Long;
            begin
               Decode_Long_Long (Buf, LL);
               Value := new Field_Value'(Value_Type => FV_Long_Long_Uint,
                                        Long_Long_Uint_Value => LL);
            end;

         when 'S' =>  -- Long string
            declare
               Str : Long_String;
            begin
               Decode_Long_String (Buf, Str);
               Value := new Field_Value'(Value_Type => FV_Long_String,
                                        Long_String_Value => Str);
            end;

         when 'T' =>  -- Timestamp
            declare
               TS : Long_Long;
            begin
               Decode_Long_Long (Buf, TS);
               Value := new Field_Value'(Value_Type => FV_Timestamp,
                                        Timestamp_Value => TS);
            end;

         when 'F' =>  -- Field table
            declare
               Table : Field_Table;
            begin
               Decode_Field_Table (Buf, Table);
               Value := new Field_Value'(Value_Type => FV_Field_Table,
                                        Field_Table_Value => new Field_Table'(Table));
            end;

         when 'V' =>  -- Void
            Value := new Field_Value'(Value_Type => FV_Void);

         when others =>
            raise Encoding_Error with "Unknown field type indicator: " &
                  Character'Val (Type_Indicator);
      end case;
   end Decode_Field_Value;

   procedure Decode_Field_Table (Buf : in out Buffer; Value : out Field_Table) is
      Table_Length : Long;
      End_Pos : Stream_Element_Offset;
      Last_Entry : Field_Table_Entry_Access := null;
   begin
      Decode_Long (Buf, Table_Length);
      End_Pos := Buf.Position + Stream_Element_Offset (Table_Length);

      Value := (Head => null);

      -- Decode entries until we reach the end position
      while Buf.Position < End_Pos loop
         declare
            New_Entry : constant Field_Table_Entry_Access := new Field_Table_Entry;
            Field_Name : Short_String;
            Field_Val : Field_Value_Access;
         begin
            -- Decode field name
            Decode_Short_String (Buf, Field_Name);
            New_Entry.Name := Field_Name;

            -- Decode field value
            Decode_Field_Value (Buf, Field_Val);
            New_Entry.Value := Field_Val;

            New_Entry.Next := null;

            -- Add to linked list
            if Value.Head = null then
               Value.Head := New_Entry;
            else
               Last_Entry.Next := New_Entry;
            end if;
            Last_Entry := New_Entry;
         end;
      end loop;

      -- Ensure we consumed exactly the right amount
      if Buf.Position /= End_Pos then
         raise Encoding_Error with "Field table length mismatch";
      end if;
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
