-- AMQP 0-9-1 Frame Processing
with AMQP.Types; use AMQP.Types;
with AMQP.Codec; use AMQP.Codec;
with Ada.Streams;

package AMQP.Frames is

   -- Frame structure constants
   FRAME_HEADER_SIZE : constant := 7;  -- Type(1) + Channel(2) + Size(4)
   FRAME_END_SIZE : constant := 1;     -- End marker
   FRAME_OVERHEAD : constant := FRAME_HEADER_SIZE + FRAME_END_SIZE;

   -- Maximum frame size (configurable via Connection.Tune)
   Default_Frame_Max : constant := 131072;  -- 128KB

   -- Frame payload representation
   type Frame_Payload is record
      Data : Ada.Streams.Stream_Element_Array (1 .. Default_Frame_Max);
      Length : Ada.Streams.Stream_Element_Offset := 0;
   end record;

   -- Complete frame structure
   type Frame is record
      Kind : Frame_Type;
      Channel : Channel_Number;
      Payload : Frame_Payload;
   end record;

   -- Frame encoding
   procedure Encode_Frame (
      Buf : in out Buffer;
      F : Frame
   );

   -- Frame decoding
   procedure Decode_Frame (
      Buf : in out Buffer;
      F : out Frame;
      Success : out Boolean
   );

   -- Helper functions for creating specific frame types
   function Make_Method_Frame (
      Channel : Channel_Number;
      Payload : Ada.Streams.Stream_Element_Array
   ) return Frame;

   function Make_Header_Frame (
      Channel : Channel_Number;
      Payload : Ada.Streams.Stream_Element_Array
   ) return Frame;

   function Make_Body_Frame (
      Channel : Channel_Number;
      Payload : Ada.Streams.Stream_Element_Array
   ) return Frame;

   function Make_Heartbeat_Frame return Frame;

   -- Validation
   function Is_Valid_Frame_Type (FT : Frame_Type) return Boolean;
   function Calculate_Frame_Size (F : Frame) return Long;

end AMQP.Frames;
