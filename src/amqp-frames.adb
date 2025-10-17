-- AMQP 0-9-1 Frame Processing Implementation
with AMQP.Constants;
with Interfaces; use Interfaces;
with Ada.Streams; use Ada.Streams;

package body AMQP.Frames is

   procedure Encode_Frame (
      Buf : in out Buffer;
      F : Frame
   ) is
      Frame_Size : constant Long := Long (F.Payload.Length);
   begin
      -- Encode frame header
      case F.Kind is
         when Method_Frame =>
            Encode_Octet (Buf, AMQP.Constants.FRAME_METHOD);
         when Header_Frame =>
            Encode_Octet (Buf, AMQP.Constants.FRAME_HEADER);
         when Body_Frame =>
            Encode_Octet (Buf, AMQP.Constants.FRAME_BODY);
         when Heartbeat_Frame =>
            Encode_Octet (Buf, AMQP.Constants.FRAME_HEARTBEAT);
      end case;

      Encode_Short (Buf, Short (F.Channel));
      Encode_Long (Buf, Frame_Size);

      -- Encode payload
      for I in 1 .. F.Payload.Length loop
         Encode_Octet (Buf, Octet (F.Payload.Data (I)));
      end loop;

      -- Encode frame end marker
      Encode_Octet (Buf, AMQP.Constants.FRAME_END);
   end Encode_Frame;

   procedure Decode_Frame (
      Buf : in out Buffer;
      F : out Frame;
      Success : out Boolean
   ) is
      Frame_Type_Byte : Octet;
      Channel_Num : Short;
      Payload_Size : Long;
      End_Marker : Octet;
   begin
      Success := False;

      -- Check if we have enough data for frame header
      if Available (Buf) < FRAME_HEADER_SIZE then
         return;
      end if;

      -- Save position in case we need to rollback
      declare
         Start_Pos : constant Ada.Streams.Stream_Element_Offset := Buf.Position;
      begin
         -- Decode frame header
         Decode_Octet (Buf, Frame_Type_Byte);

         -- Validate and convert frame type
         case Frame_Type_Byte is
            when AMQP.Constants.FRAME_METHOD =>
               F.Kind := Method_Frame;
            when AMQP.Constants.FRAME_HEADER =>
               F.Kind := Header_Frame;
            when AMQP.Constants.FRAME_BODY =>
               F.Kind := Body_Frame;
            when AMQP.Constants.FRAME_HEARTBEAT =>
               F.Kind := Heartbeat_Frame;
            when others =>
               Buf.Position := Start_Pos;  -- Rollback
               raise Frame_Error with "Invalid frame type: " & Octet'Image (Frame_Type_Byte);
         end case;

         Decode_Short (Buf, Channel_Num);
         F.Channel := Channel_Number (Channel_Num);

         Decode_Long (Buf, Payload_Size);

         -- Check if we have complete frame (payload + end marker)
         if Available (Buf) < Ada.Streams.Stream_Element_Offset (Payload_Size) + 1 then
            Buf.Position := Start_Pos;  -- Rollback - need more data
            return;
         end if;

         -- Validate payload size
         if Payload_Size > Long (Default_Frame_Max) then
            raise Frame_Error with "Frame payload too large: " & Long'Image (Payload_Size);
         end if;

         -- Decode payload
         F.Payload.Length := Ada.Streams.Stream_Element_Offset (Payload_Size);
         for I in 1 .. F.Payload.Length loop
            Decode_Octet (Buf, Frame_Type_Byte);
            F.Payload.Data (I) := Ada.Streams.Stream_Element (Frame_Type_Byte);
         end loop;

         -- Decode and validate frame end marker
         Decode_Octet (Buf, End_Marker);
         if End_Marker /= AMQP.Constants.FRAME_END then
            raise Frame_Error with "Invalid frame end marker: " & Octet'Image (End_Marker);
         end if;

         Success := True;
      end;
   end Decode_Frame;

   function Make_Method_Frame (
      Channel : Channel_Number;
      Payload : Ada.Streams.Stream_Element_Array
   ) return Frame
   is
      F : Frame;
   begin
      F.Kind := Method_Frame;
      F.Channel := Channel;
      F.Payload.Length := Payload'Length;
      F.Payload.Data (1 .. Payload'Length) := Payload;
      return F;
   end Make_Method_Frame;

   function Make_Header_Frame (
      Channel : Channel_Number;
      Payload : Ada.Streams.Stream_Element_Array
   ) return Frame
   is
      F : Frame;
   begin
      F.Kind := Header_Frame;
      F.Channel := Channel;
      F.Payload.Length := Payload'Length;
      F.Payload.Data (1 .. Payload'Length) := Payload;
      return F;
   end Make_Header_Frame;

   function Make_Body_Frame (
      Channel : Channel_Number;
      Payload : Ada.Streams.Stream_Element_Array
   ) return Frame
   is
      F : Frame;
   begin
      F.Kind := Body_Frame;
      F.Channel := Channel;
      F.Payload.Length := Payload'Length;
      F.Payload.Data (1 .. Payload'Length) := Payload;
      return F;
   end Make_Body_Frame;

   function Make_Heartbeat_Frame return Frame is
      F : Frame;
   begin
      F.Kind := Heartbeat_Frame;
      F.Channel := 0;
      F.Payload.Length := 0;
      return F;
   end Make_Heartbeat_Frame;

   function Is_Valid_Frame_Type (FT : Frame_Type) return Boolean is
   begin
      return FT in Method_Frame | Header_Frame | Body_Frame | Heartbeat_Frame;
   end Is_Valid_Frame_Type;

   function Calculate_Frame_Size (F : Frame) return Long is
   begin
      return Long (F.Payload.Length) + FRAME_OVERHEAD;
   end Calculate_Frame_Size;

end AMQP.Frames;
