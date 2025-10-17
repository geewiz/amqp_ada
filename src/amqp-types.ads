-- AMQP 0-9-1 Basic Types
with Interfaces;

package AMQP.Types is

   use Interfaces;

   -- Basic AMQP types
   subtype Octet is Unsigned_8;
   subtype Short is Unsigned_16;
   subtype Long is Unsigned_32;
   subtype Long_Long is Unsigned_64;

   type Bit is new Boolean;

   -- Strings
   type Short_String is access String;
   type Long_String is access String;

   -- Arrays and tables
   type Field_Value_Type is (
      FV_Boolean,
      FV_Short_Short_Int,
      FV_Short_Short_Uint,
      FV_Short_Int,
      FV_Short_Uint,
      FV_Long_Int,
      FV_Long_Uint,
      FV_Long_Long_Int,
      FV_Long_Long_Uint,
      FV_Float,
      FV_Double,
      FV_Decimal,
      FV_Short_String,
      FV_Long_String,
      FV_Field_Array,
      FV_Timestamp,
      FV_Field_Table,
      FV_Void
   );

   type Field_Value;
   type Field_Value_Access is access Field_Value;

   type Field_Array is array (Positive range <>) of Field_Value_Access;
   type Field_Array_Access is access Field_Array;

   type Field_Table_Entry;
   type Field_Table_Entry_Access is access Field_Table_Entry;

   type Field_Table_Entry is record
      Name : Short_String;
      Value : Field_Value_Access;
      Next : Field_Table_Entry_Access;
   end record;

   type Field_Table is record
      Head : Field_Table_Entry_Access;
   end record;

   type Field_Table_Access is access Field_Table;

   type Field_Value (Value_Type : Field_Value_Type := FV_Void) is record
      case Value_Type is
         when FV_Boolean =>
            Boolean_Value : Boolean;
         when FV_Short_Short_Int =>
            Short_Short_Int_Value : Integer_8;
         when FV_Short_Short_Uint =>
            Short_Short_Uint_Value : Unsigned_8;
         when FV_Short_Int =>
            Short_Int_Value : Integer_16;
         when FV_Short_Uint =>
            Short_Uint_Value : Unsigned_16;
         when FV_Long_Int =>
            Long_Int_Value : Integer_32;
         when FV_Long_Uint =>
            Long_Uint_Value : Unsigned_32;
         when FV_Long_Long_Int =>
            Long_Long_Int_Value : Integer_64;
         when FV_Long_Long_Uint =>
            Long_Long_Uint_Value : Unsigned_64;
         when FV_Float =>
            Float_Value : Float;
         when FV_Double =>
            Double_Value : Long_Float;
         when FV_Short_String =>
            Short_String_Value : Short_String;
         when FV_Long_String =>
            Long_String_Value : Long_String;
         when FV_Field_Array =>
            Field_Array_Value : Field_Array_Access;
         when FV_Timestamp =>
            Timestamp_Value : Unsigned_64;
         when FV_Field_Table =>
            Field_Table_Value : Field_Table_Access;
         when FV_Void | FV_Decimal =>
            null;
      end case;
   end record;

   -- Frame types
   type Frame_Type is (Method_Frame, Header_Frame, Body_Frame, Heartbeat_Frame);
   for Frame_Type use (
      Method_Frame => 1,
      Header_Frame => 2,
      Body_Frame => 3,
      Heartbeat_Frame => 8
   );
   for Frame_Type'Size use 8;

   type Channel_Number is new Short range 0 .. 65535;

   type Frame_Header is record
      Frame_Kind : Frame_Type;
      Channel : Channel_Number;
      Size : Long;
   end record;

end AMQP.Types;
