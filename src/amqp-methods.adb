-- AMQP 0-9-1 Method Implementations
with Ada.Streams;
with Interfaces; use Interfaces;

package body AMQP.Methods is

   -- Encode Connection.Start-Ok
   procedure Encode_Connection_Start_Ok (
      Buf : in out Buffer;
      Method : Connection_Start_Ok
   ) is
   begin
      Encode_Field_Table (Buf, Method.Client_Properties);
      Encode_Short_String (Buf, Method.Mechanism.all);
      Encode_Long_String (Buf, Method.Response.all);
      Encode_Short_String (Buf, Method.Locale.all);
   end Encode_Connection_Start_Ok;

   -- Encode Connection.Tune-Ok
   procedure Encode_Connection_Tune_Ok (
      Buf : in out Buffer;
      Method : Connection_Tune_Ok
   ) is
   begin
      Encode_Short (Buf, Method.Channel_Max);
      Encode_Long (Buf, Method.Frame_Max);
      Encode_Short (Buf, Method.Heartbeat);
   end Encode_Connection_Tune_Ok;

   -- Encode Connection.Open
   procedure Encode_Connection_Open (
      Buf : in out Buffer;
      Method : Connection_Open
   ) is
      Flags : Octet := 0;
   begin
      Encode_Short_String (Buf, Method.Virtual_Host.all);
      Encode_Short_String (Buf, "");  -- Reserved_1 (deprecated)

      if Method.Reserved_2 then
         Flags := Flags or 16#01#;
      end if;
      Encode_Octet (Buf, Flags);
   end Encode_Connection_Open;

   -- Encode Connection.Close
   procedure Encode_Connection_Close (
      Buf : in out Buffer;
      Method : Connection_Close
   ) is
   begin
      Encode_Short (Buf, Method.Reply_Code);
      Encode_Short_String (Buf, Method.Reply_Text.all);
      Encode_Short (Buf, Method.Class_Id);
      Encode_Short (Buf, Method.Method_Id);
   end Encode_Connection_Close;

   -- Encode Connection.Close-Ok
   procedure Encode_Connection_Close_Ok (
      Buf : in out Buffer;
      Method : Connection_Close_Ok
   ) is
      pragma Unreferenced (Buf, Method);
   begin
      null;  -- No fields
   end Encode_Connection_Close_Ok;

   -- Decode Connection.Start
   procedure Decode_Connection_Start (
      Buf : in out Buffer;
      Method : out Connection_Start;
      Success : out Boolean
   ) is
      Mechanisms_Str : Long_String;
      Locales_Str : Long_String;
      use Ada.Streams;
   begin
      -- Minimum check for version bytes
      if Buf.Length - Buf.Position + 1 < Stream_Element_Offset (2) then
         Success := False;
         return;
      end if;

      Decode_Octet (Buf, Method.Version_Major);
      Decode_Octet (Buf, Method.Version_Minor);
      Decode_Field_Table (Buf, Method.Server_Properties);

      Decode_Long_String (Buf, Mechanisms_Str);
      Method.Mechanisms := String_Access (Mechanisms_Str);

      Decode_Long_String (Buf, Locales_Str);
      Method.Locales := String_Access (Locales_Str);

      Success := True;
   end Decode_Connection_Start;

   -- Decode Connection.Tune
   procedure Decode_Connection_Tune (
      Buf : in out Buffer;
      Method : out Connection_Tune;
      Success : out Boolean
   ) is
      use Ada.Streams;
   begin
      -- Need 8 bytes total (2 + 4 + 2)
      if Buf.Length - Buf.Position + 1 < Stream_Element_Offset (8) then
         Success := False;
         return;
      end if;

      Decode_Short (Buf, Method.Channel_Max);
      Decode_Long (Buf, Method.Frame_Max);
      Decode_Short (Buf, Method.Heartbeat);

      Success := True;
   end Decode_Connection_Tune;

   -- Decode Connection.Open-Ok
   procedure Decode_Connection_Open_Ok (
      Buf : in out Buffer;
      Method : out Connection_Open_Ok;
      Success : out Boolean
   ) is
      Reserved_Str : Short_String;
   begin
      Decode_Short_String (Buf, Reserved_Str);
      Method.Reserved := String_Access (Reserved_Str);

      Success := True;
   end Decode_Connection_Open_Ok;

   -- Decode Connection.Close
   procedure Decode_Connection_Close (
      Buf : in out Buffer;
      Method : out Connection_Close;
      Success : out Boolean
   ) is
      Text : Short_String;
      use Ada.Streams;
   begin
      -- Minimum check for reply_code + text length + class_id + method_id
      if Buf.Length - Buf.Position + 1 < Stream_Element_Offset (6) then
         Success := False;
         return;
      end if;

      Decode_Short (Buf, Method.Reply_Code);
      Decode_Short_String (Buf, Text);
      Method.Reply_Text := String_Access (Text);

      Decode_Short (Buf, Method.Class_Id);
      Decode_Short (Buf, Method.Method_Id);

      Success := True;
   end Decode_Connection_Close;

   -- Decode Connection.Close-Ok
   procedure Decode_Connection_Close_Ok (
      Buf : in out Buffer;
      Method : out Connection_Close_Ok;
      Success : out Boolean
   ) is
      pragma Unreferenced (Buf, Method);
   begin
      Success := True;  -- No fields to decode
   end Decode_Connection_Close_Ok;

   -- Encode Channel.Open
   procedure Encode_Channel_Open (
      Buf : in out Buffer;
      Method : Channel_Open
   ) is
      pragma Unreferenced (Method);
   begin
      Encode_Short_String (Buf, "");  -- Reserved (deprecated)
   end Encode_Channel_Open;

   -- Encode Channel.Close
   procedure Encode_Channel_Close (
      Buf : in out Buffer;
      Method : Channel_Close
   ) is
   begin
      Encode_Short (Buf, Method.Reply_Code);
      Encode_Short_String (Buf, Method.Reply_Text.all);
      Encode_Short (Buf, Method.Class_Id);
      Encode_Short (Buf, Method.Method_Id);
   end Encode_Channel_Close;

   -- Encode Channel.Close-Ok
   procedure Encode_Channel_Close_Ok (
      Buf : in out Buffer;
      Method : Channel_Close_Ok
   ) is
      pragma Unreferenced (Buf, Method);
   begin
      null;  -- No fields
   end Encode_Channel_Close_Ok;

   -- Decode Channel.Open-Ok
   procedure Decode_Channel_Open_Ok (
      Buf : in out Buffer;
      Method : out Channel_Open_Ok;
      Success : out Boolean
   ) is
      Reserved_Str : Short_String;
   begin
      Decode_Short_String (Buf, Reserved_Str);
      Method.Reserved := String_Access (Reserved_Str);
      Success := True;
   end Decode_Channel_Open_Ok;

   -- Decode Channel.Close
   procedure Decode_Channel_Close (
      Buf : in out Buffer;
      Method : out Channel_Close;
      Success : out Boolean
   ) is
      Text : Short_String;
      use Ada.Streams;
   begin
      -- Minimum check for reply_code + text length + class_id + method_id
      if Buf.Length - Buf.Position + 1 < Stream_Element_Offset (6) then
         Success := False;
         return;
      end if;

      Decode_Short (Buf, Method.Reply_Code);
      Decode_Short_String (Buf, Text);
      Method.Reply_Text := String_Access (Text);
      Decode_Short (Buf, Method.Class_Id);
      Decode_Short (Buf, Method.Method_Id);

      Success := True;
   end Decode_Channel_Close;

   -- Decode Channel.Close-Ok
   procedure Decode_Channel_Close_Ok (
      Buf : in out Buffer;
      Method : out Channel_Close_Ok;
      Success : out Boolean
   ) is
      pragma Unreferenced (Buf, Method);
   begin
      Success := True;  -- No fields to decode
   end Decode_Channel_Close_Ok;

   -- Encode Queue.Declare
   procedure Encode_Queue_Declare (
      Buf : in out Buffer;
      Method : Queue_Declare
   ) is
      Flags : Octet := 0;
   begin
      Encode_Short (Buf, Method.Reserved_1);
      Encode_Short_String (Buf, Method.Queue.all);

      if Method.Passive then
         Flags := Flags or 16#01#;
      end if;
      if Method.Durable then
         Flags := Flags or 16#02#;
      end if;
      if Method.Exclusive then
         Flags := Flags or 16#04#;
      end if;
      if Method.Auto_Delete then
         Flags := Flags or 16#08#;
      end if;
      if Method.No_Wait then
         Flags := Flags or 16#10#;
      end if;

      Encode_Octet (Buf, Flags);
      Encode_Field_Table (Buf, Method.Arguments);
   end Encode_Queue_Declare;

   -- Decode Queue.Declare-Ok
   procedure Decode_Queue_Declare_Ok (
      Buf : in out Buffer;
      Method : out Queue_Declare_Ok;
      Success : out Boolean
   ) is
      Queue_Str : Short_String;
   begin
      Decode_Short_String (Buf, Queue_Str);
      Method.Queue := String_Access (Queue_Str);
      Decode_Long (Buf, Method.Message_Count);
      Decode_Long (Buf, Method.Consumer_Count);
      Success := True;
   end Decode_Queue_Declare_Ok;

   -- Encode Basic.Publish
   procedure Encode_Basic_Publish (
      Buf : in out Buffer;
      Method : Basic_Publish
   ) is
      Flags : Octet := 0;
   begin
      Encode_Short (Buf, Method.Reserved_1);
      Encode_Short_String (Buf, Method.Exchange.all);
      Encode_Short_String (Buf, Method.Routing_Key.all);

      if Method.Mandatory then
         Flags := Flags or 16#01#;
      end if;
      if Method.Immediate then
         Flags := Flags or 16#02#;
      end if;

      Encode_Octet (Buf, Flags);
   end Encode_Basic_Publish;

   -- Encode Basic.Consume
   procedure Encode_Basic_Consume (
      Buf : in out Buffer;
      Method : Basic_Consume
   ) is
      Flags : Octet := 0;
   begin
      Encode_Short (Buf, Method.Reserved_1);
      Encode_Short_String (Buf, Method.Queue.all);
      Encode_Short_String (Buf, Method.Consumer_Tag.all);

      if Method.No_Local then
         Flags := Flags or 16#01#;
      end if;
      if Method.No_Ack then
         Flags := Flags or 16#02#;
      end if;
      if Method.Exclusive then
         Flags := Flags or 16#04#;
      end if;
      if Method.No_Wait then
         Flags := Flags or 16#08#;
      end if;

      Encode_Octet (Buf, Flags);
      Encode_Field_Table (Buf, Method.Arguments);
   end Encode_Basic_Consume;

   -- Encode Basic.Ack
   procedure Encode_Basic_Ack (
      Buf : in out Buffer;
      Method : Basic_Ack
   ) is
      Flags : Octet := 0;
   begin
      Encode_Long_Long (Buf, Method.Delivery_Tag);

      if Method.Multiple then
         Flags := Flags or 16#01#;
      end if;

      Encode_Octet (Buf, Flags);
   end Encode_Basic_Ack;

   -- Encode Content Header
   procedure Encode_Content_Header (
      Buf : in out Buffer;
      Method : Content_Header
   ) is
   begin
      Encode_Short (Buf, Method.Class_Id);
      Encode_Short (Buf, Method.Weight);
      Encode_Long_Long (Buf, Method.Body_Size);
      -- Property flags and properties would be encoded here
      -- For now, encode empty property flags
      Encode_Short (Buf, 0);
   end Encode_Content_Header;

   -- Decode Basic.Consume-Ok
   procedure Decode_Basic_Consume_Ok (
      Buf : in out Buffer;
      Method : out Basic_Consume_Ok;
      Success : out Boolean
   ) is
      Tag : Short_String;
   begin
      Decode_Short_String (Buf, Tag);
      Method.Consumer_Tag := String_Access (Tag);
      Success := True;
   end Decode_Basic_Consume_Ok;

   -- Decode Basic.Deliver
   procedure Decode_Basic_Deliver (
      Buf : in out Buffer;
      Method : out Basic_Deliver;
      Success : out Boolean
   ) is
      Tag : Short_String;
      Exch : Short_String;
      RKey : Short_String;
      Flags : Octet;
   begin
      Decode_Short_String (Buf, Tag);
      Method.Consumer_Tag := String_Access (Tag);

      Decode_Long_Long (Buf, Method.Delivery_Tag);

      Decode_Octet (Buf, Flags);
      Method.Redelivered := (Flags and 16#01#) /= 0;

      Decode_Short_String (Buf, Exch);
      Method.Exchange := String_Access (Exch);

      Decode_Short_String (Buf, RKey);
      Method.Routing_Key := String_Access (RKey);

      Success := True;
   end Decode_Basic_Deliver;

   -- Decode Content Header
   procedure Decode_Content_Header (
      Buf : in out Buffer;
      Method : out Content_Header;
      Success : out Boolean
   ) is
      Property_Flags : Short;
   begin
      Decode_Short (Buf, Method.Class_Id);
      Decode_Short (Buf, Method.Weight);
      Decode_Long_Long (Buf, Method.Body_Size);

      -- Read property flags (simplified - just skip for now)
      Decode_Short (Buf, Property_Flags);

      Success := True;
   end Decode_Content_Header;

   -- Create PLAIN authentication response
   -- Format: \0username\0password
   function Create_Plain_Auth_Response (
      Username : String;
      Password : String
   ) return String_Access is
      Response : constant String :=
         Character'Val (0) & Username & Character'Val (0) & Password;
   begin
      return new String'(Response);
   end Create_Plain_Auth_Response;

end AMQP.Methods;
