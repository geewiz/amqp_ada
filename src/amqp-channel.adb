-- AMQP 0-9-1 Channel Management Implementation
with AMQP.Constants;
with AMQP.Methods; use AMQP.Methods;
with AMQP.Frames; use AMQP.Frames;
with AMQP.Codec; use AMQP.Codec;
with Ada.Streams;
with Ada.Text_IO;
with Interfaces; use Interfaces;

package body AMQP.Channel is

   procedure Send_Method (
      Chan : in out Channel;
      Class_Id : Short;
      Method_Id : Short;
      Arguments : Buffer
   ) is
      use Ada.Streams;
      F : Frame;
      Temp_Buf : Buffer;
   begin
      -- Build method frame payload: class_id + method_id + arguments
      Reset (Temp_Buf);
      Encode_Short (Temp_Buf, Class_Id);
      Encode_Short (Temp_Buf, Method_Id);

      -- Copy arguments
      for I in 1 .. Arguments.Length loop
         Temp_Buf.Data (Temp_Buf.Length + I) := Arguments.Data (I);
      end loop;
      Temp_Buf.Length := Temp_Buf.Length + Arguments.Length;

      -- Create frame
      F.Kind := Method_Frame;
      F.Channel := Chan.Number;
      F.Payload.Length := Temp_Buf.Length;

      -- Copy payload data
      for I in 1 .. Temp_Buf.Length loop
         F.Payload.Data (I) := Temp_Buf.Data (I);
      end loop;

      AMQP.Connection.Send_Frame (Chan.Conn.all, F);
   end Send_Method;

   procedure Receive_Method (
      Chan : in out Channel;
      Class_Id : out Short;
      Method_Id : out Short;
      Arguments : out Buffer;
      Success : out Boolean
   ) is
      use Ada.Streams;
      F : Frame;
      Temp_Buf : Buffer;
   begin
      Success := False;

      -- Receive frame
      AMQP.Connection.Receive_Frame (Chan.Conn.all, F, Success);
      if not Success or else F.Kind /= Method_Frame then
         return;
      end if;

      -- Verify it's for this channel
      if F.Channel /= Chan.Number then
         raise Channel_Error with "Received frame for wrong channel";
      end if;

      -- Convert frame payload to buffer
      Reset (Temp_Buf);
      for I in 1 .. F.Payload.Length loop
         Temp_Buf.Data (I) := F.Payload.Data (I);
      end loop;
      Temp_Buf.Length := F.Payload.Length;
      Temp_Buf.Position := 1;

      -- Decode class_id and method_id
      Decode_Short (Temp_Buf, Class_Id);
      Decode_Short (Temp_Buf, Method_Id);

      -- Copy remaining data to arguments buffer
      Reset (Arguments);
      for I in Temp_Buf.Position .. Temp_Buf.Length loop
         Arguments.Data (Arguments.Length + 1) := Temp_Buf.Data (I);
         Arguments.Length := Arguments.Length + 1;
      end loop;

      Success := True;
   end Receive_Method;

   procedure Open (
      Chan : in out Channel;
      Channel_Number : AMQP.Types.Channel_Number
   ) is
      Open_Method : Channel_Open;
      Open_Ok_Method : Channel_Open_Ok;
      Args_Buf : Buffer;
      Class_Id : Short;
      Method_Id : Short;
      Success : Boolean;
   begin
      if Chan.State /= Closed then
         raise Channel_Error with "Channel already open";
      end if;

      if not AMQP.Connection.Is_Connected (Chan.Conn.all) then
         raise Channel_Error with "Connection not established";
      end if;

      Chan.Number := Channel_Number;
      Chan.State := Opening;

      pragma Debug (Ada.Text_IO.Put_Line ("Opening channel" & Channel_Number'Image & "..."));

      -- Send Channel.Open
      Open_Method.Reserved := new String'("");
      Reset (Args_Buf);
      Encode_Channel_Open (Args_Buf, Open_Method);
      Send_Method (Chan, AMQP.Constants.CLASS_CHANNEL, AMQP.Constants.CHANNEL_OPEN, Args_Buf);

      pragma Debug (Ada.Text_IO.Put_Line ("Sent Channel.Open"));

      -- Wait for Channel.Open-Ok
      pragma Debug (Ada.Text_IO.Put_Line ("Waiting for Channel.Open-Ok..."));
      Receive_Method (Chan, Class_Id, Method_Id, Args_Buf, Success);
      if not Success then
         Chan.State := Closed;
         raise Channel_Error with "Failed to receive Channel.Open-Ok";
      end if;

      if Class_Id /= AMQP.Constants.CLASS_CHANNEL or else
         Method_Id /= AMQP.Constants.CHANNEL_OPEN_OK
      then
         Chan.State := Closed;
         raise Channel_Error with "Expected Channel.Open-Ok";
      end if;

      Decode_Channel_Open_Ok (Args_Buf, Open_Ok_Method, Success);
      if not Success then
         Chan.State := Closed;
         raise Channel_Error with "Failed to decode Channel.Open-Ok";
      end if;

      Chan.State := Open;
      pragma Debug (Ada.Text_IO.Put_Line ("Channel" & Channel_Number'Image & " opened successfully"));
   end Open;

   procedure Close (Chan : in out Channel) is
      Close_Method : Channel_Close;
      Close_Ok_Method : Channel_Close_Ok;
      Args_Buf : Buffer;
      Class_Id : Short;
      Method_Id : Short;
      Success : Boolean;
   begin
      if Chan.State /= Open then
         return;  -- Already closed or not open
      end if;

      Chan.State := Closing;
      pragma Debug (Ada.Text_IO.Put_Line ("Closing channel" & Chan.Number'Image & "..."));

      -- Send Channel.Close
      Close_Method.Reply_Code := AMQP.Constants.REPLY_SUCCESS;
      Close_Method.Reply_Text := new String'("Normal shutdown");
      Close_Method.Class_Id := 0;
      Close_Method.Method_Id := 0;

      Reset (Args_Buf);
      Encode_Channel_Close (Args_Buf, Close_Method);
      Send_Method (Chan, AMQP.Constants.CLASS_CHANNEL, AMQP.Constants.CHANNEL_CLOSE, Args_Buf);

      pragma Debug (Ada.Text_IO.Put_Line ("Sent Channel.Close"));

      -- Wait for Channel.Close-Ok
      pragma Debug (Ada.Text_IO.Put_Line ("Waiting for Channel.Close-Ok..."));
      Receive_Method (Chan, Class_Id, Method_Id, Args_Buf, Success);
      if not Success then
         Chan.State := Closed;
         return;  -- Best effort
      end if;

      if Class_Id = AMQP.Constants.CLASS_CHANNEL and then
         Method_Id = AMQP.Constants.CHANNEL_CLOSE_OK
      then
         Decode_Channel_Close_Ok (Args_Buf, Close_Ok_Method, Success);
      end if;

      Chan.State := Closed;
      pragma Debug (Ada.Text_IO.Put_Line ("Channel" & Chan.Number'Image & " closed successfully"));
   end Close;

   function Is_Open (Chan : Channel) return Boolean is
   begin
      return Chan.State = Open;
   end Is_Open;

   function Get_State (Chan : Channel) return Channel_State is
   begin
      return Chan.State;
   end Get_State;

   function Get_Channel_Number (Chan : Channel) return AMQP.Types.Channel_Number is
   begin
      return Chan.Number;
   end Get_Channel_Number;

   procedure Queue_Declare (
      Chan : in out Channel;
      Queue : String;
      Durable : Boolean := False;
      Exclusive : Boolean := False;
      Auto_Delete : Boolean := False
   ) is
      Declare_Method : Methods.Queue_Declare;
      Declare_Ok_Method : Methods.Queue_Declare_Ok;
      Args_Buf : Buffer;
      Class_Id : Short;
      Method_Id : Short;
      Success : Boolean;
   begin
      if Chan.State /= Open then
         raise Channel_Error with "Channel not open";
      end if;

      pragma Debug (Ada.Text_IO.Put_Line ("Declaring queue: " & Queue));

      -- Setup Queue.Declare
      Declare_Method.Queue := new String'(Queue);
      Declare_Method.Durable := Durable;
      Declare_Method.Exclusive := Exclusive;
      Declare_Method.Auto_Delete := Auto_Delete;
      Declare_Method.Arguments.Head := null;

      Reset (Args_Buf);
      Encode_Queue_Declare (Args_Buf, Declare_Method);
      Send_Method (Chan, AMQP.Constants.CLASS_QUEUE, AMQP.Constants.QUEUE_DECLARE, Args_Buf);

      pragma Debug (Ada.Text_IO.Put_Line ("Sent Queue.Declare"));

      -- Wait for Queue.Declare-Ok
      pragma Debug (Ada.Text_IO.Put_Line ("Waiting for Queue.Declare-Ok..."));
      Receive_Method (Chan, Class_Id, Method_Id, Args_Buf, Success);
      if not Success then
         raise Channel_Error with "Failed to receive Queue.Declare-Ok";
      end if;

      if Class_Id /= AMQP.Constants.CLASS_QUEUE or else
         Method_Id /= AMQP.Constants.QUEUE_DECLARE_OK
      then
         raise Channel_Error with "Expected Queue.Declare-Ok";
      end if;

      Decode_Queue_Declare_Ok (Args_Buf, Declare_Ok_Method, Success);
      if not Success then
         raise Channel_Error with "Failed to decode Queue.Declare-Ok";
      end if;

      pragma Debug (Ada.Text_IO.Put_Line ("Queue declared: " & Declare_Ok_Method.Queue.all));
   end Queue_Declare;

   procedure Basic_Publish (
      Chan : in out Channel;
      Exchange : String;
      Routing_Key : String;
      Message_Body : String
   ) is
      use Ada.Streams;
      Publish_Method : Methods.Basic_Publish;
      Header_Method : Methods.Content_Header;
      Args_Buf : Buffer;
      F : Frame;
   begin
      if Chan.State /= Open then
         raise Channel_Error with "Channel not open";
      end if;

      pragma Debug (Ada.Text_IO.Put_Line ("Publishing message to exchange='" & Exchange & "' routing_key='" & Routing_Key & "'"));

      -- Send Basic.Publish method
      Publish_Method.Exchange := new String'(Exchange);
      Publish_Method.Routing_Key := new String'(Routing_Key);

      Reset (Args_Buf);
      Encode_Basic_Publish (Args_Buf, Publish_Method);
      Send_Method (Chan, AMQP.Constants.CLASS_BASIC, AMQP.Constants.BASIC_PUBLISH, Args_Buf);

      -- Send content header frame
      Header_Method.Class_Id := AMQP.Constants.CLASS_BASIC;
      Header_Method.Body_Size := Long_Long (Message_Body'Length);

      Reset (Args_Buf);
      Encode_Content_Header (Args_Buf, Header_Method);

      F.Kind := Header_Frame;
      F.Channel := Chan.Number;
      F.Payload.Length := Args_Buf.Length;
      for I in 1 .. Args_Buf.Length loop
         F.Payload.Data (I) := Args_Buf.Data (I);
      end loop;

      AMQP.Connection.Send_Frame (Chan.Conn.all, F);

      -- Send body frame
      F.Kind := Body_Frame;
      F.Channel := Chan.Number;
      F.Payload.Length := Stream_Element_Offset (Message_Body'Length);
      for I in Message_Body'Range loop
         F.Payload.Data (Stream_Element_Offset (I - Message_Body'First + 1)) :=
            Stream_Element (Character'Pos (Message_Body (I)));
      end loop;

      AMQP.Connection.Send_Frame (Chan.Conn.all, F);

      pragma Debug (Ada.Text_IO.Put_Line ("Message published"));
   end Basic_Publish;

   procedure Basic_Consume (
      Chan : in out Channel;
      Queue : String;
      Consumer_Tag : String;
      No_Ack : Boolean := False
   ) is
      Consume_Method : Methods.Basic_Consume;
      Consume_Ok_Method : Methods.Basic_Consume_Ok;
      Args_Buf : Buffer;
      Class_Id : Short;
      Method_Id : Short;
      Success : Boolean;
   begin
      if Chan.State /= Open then
         raise Channel_Error with "Channel not open";
      end if;

      pragma Debug (Ada.Text_IO.Put_Line ("Starting consumer on queue: " & Queue));

      -- Send Basic.Consume
      Consume_Method.Queue := new String'(Queue);
      Consume_Method.Consumer_Tag := new String'(Consumer_Tag);
      Consume_Method.No_Ack := No_Ack;
      Consume_Method.Arguments.Head := null;

      Reset (Args_Buf);
      Encode_Basic_Consume (Args_Buf, Consume_Method);
      Send_Method (Chan, AMQP.Constants.CLASS_BASIC, AMQP.Constants.BASIC_CONSUME, Args_Buf);

      pragma Debug (Ada.Text_IO.Put_Line ("Sent Basic.Consume"));

      -- Wait for Basic.Consume-Ok
      pragma Debug (Ada.Text_IO.Put_Line ("Waiting for Basic.Consume-Ok..."));
      Receive_Method (Chan, Class_Id, Method_Id, Args_Buf, Success);
      if not Success then
         raise Channel_Error with "Failed to receive Basic.Consume-Ok";
      end if;

      if Class_Id /= AMQP.Constants.CLASS_BASIC or else
         Method_Id /= AMQP.Constants.BASIC_CONSUME_OK
      then
         raise Channel_Error with "Expected Basic.Consume-Ok";
      end if;

      Decode_Basic_Consume_Ok (Args_Buf, Consume_Ok_Method, Success);
      if not Success then
         raise Channel_Error with "Failed to decode Basic.Consume-Ok";
      end if;

      pragma Debug (Ada.Text_IO.Put_Line ("Consumer started: " & Consume_Ok_Method.Consumer_Tag.all));
   end Basic_Consume;

   procedure Basic_Get (
      Chan : in out Channel;
      Msg : out Message;
      Success : out Boolean
   ) is
      use Ada.Streams;
      Deliver_Method : Methods.Basic_Deliver;
      Header_Method : Methods.Content_Header;
      Args_Buf : Buffer;
      Class_Id : Short;
      Method_Id : Short;
      F : Frame;
   begin
      Success := False;

      if Chan.State /= Open then
         raise Channel_Error with "Channel not open";
      end if;

      pragma Debug (Ada.Text_IO.Put_Line ("Waiting for message..."));

      -- Wait for Basic.Deliver
      Receive_Method (Chan, Class_Id, Method_Id, Args_Buf, Success);
      if not Success then
         return;
      end if;

      if Class_Id /= AMQP.Constants.CLASS_BASIC or else
         Method_Id /= AMQP.Constants.BASIC_DELIVER
      then
         Success := False;
         return;
      end if;

      Decode_Basic_Deliver (Args_Buf, Deliver_Method, Success);
      if not Success then
         return;
      end if;

      pragma Debug (Ada.Text_IO.Put_Line ("Received Basic.Deliver"));

      -- Receive content header frame
      AMQP.Connection.Receive_Frame (Chan.Conn.all, F, Success);
      if not Success or else F.Kind /= Header_Frame then
         Success := False;
         return;
      end if;

      Reset (Args_Buf);
      for I in 1 .. F.Payload.Length loop
         Args_Buf.Data (I) := F.Payload.Data (I);
      end loop;
      Args_Buf.Length := F.Payload.Length;
      Args_Buf.Position := 1;

      Decode_Content_Header (Args_Buf, Header_Method, Success);
      if not Success then
         return;
      end if;

      pragma Debug (Ada.Text_IO.Put_Line ("Received content header, body size:" & Long_Long'Image (Header_Method.Body_Size)));

      -- Receive body frame
      AMQP.Connection.Receive_Frame (Chan.Conn.all, F, Success);
      if not Success or else F.Kind /= Body_Frame then
         Success := False;
         return;
      end if;

      -- Extract body
      declare
         Body_Str : String (1 .. Integer (F.Payload.Length));
      begin
         for I in 1 .. F.Payload.Length loop
            Body_Str (Integer (I)) := Character'Val (F.Payload.Data (I));
         end loop;

         Msg.Content := new String'(Body_Str);
         Msg.Delivery_Tag := Deliver_Method.Delivery_Tag;
         Msg.Exchange := Deliver_Method.Exchange;
         Msg.Routing_Key := Deliver_Method.Routing_Key;

         pragma Debug (Ada.Text_IO.Put_Line ("Received message: " & Body_Str));
         Success := True;
      end;
   end Basic_Get;

   procedure Basic_Ack (
      Chan : in out Channel;
      Delivery_Tag : Long_Long
   ) is
      Ack_Method : Methods.Basic_Ack;
      Args_Buf : Buffer;
   begin
      if Chan.State /= Open then
         raise Channel_Error with "Channel not open";
      end if;

      pragma Debug (Ada.Text_IO.Put_Line ("Acknowledging delivery tag:" & Long_Long'Image (Delivery_Tag)));

      Ack_Method.Delivery_Tag := Delivery_Tag;

      Reset (Args_Buf);
      Encode_Basic_Ack (Args_Buf, Ack_Method);
      Send_Method (Chan, AMQP.Constants.CLASS_BASIC, AMQP.Constants.BASIC_ACK, Args_Buf);

      pragma Debug (Ada.Text_IO.Put_Line ("Sent Basic.Ack"));
   end Basic_Ack;

end AMQP.Channel;
