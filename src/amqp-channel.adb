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
      Conn : Connection_Access;
      Channel_Number : AMQP.Types.Channel_Number
   ) is
      use Ada.Text_IO;
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

      if not AMQP.Connection.Is_Connected (Conn.all) then
         raise Channel_Error with "Connection not established";
      end if;

      Chan.Conn := Conn;
      Chan.Number := Channel_Number;
      Chan.State := Opening;

      Put_Line ("Opening channel" & Channel_Number'Image & "...");

      -- Send Channel.Open
      Open_Method.Reserved := new String'("");
      Reset (Args_Buf);
      Encode_Channel_Open (Args_Buf, Open_Method);
      Send_Method (Chan, AMQP.Constants.CLASS_CHANNEL, AMQP.Constants.CHANNEL_OPEN, Args_Buf);

      Put_Line ("Sent Channel.Open");

      -- Wait for Channel.Open-Ok
      Put_Line ("Waiting for Channel.Open-Ok...");
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
      Put_Line ("Channel" & Channel_Number'Image & " opened successfully");
   end Open;

   procedure Close (Chan : in out Channel) is
      use Ada.Text_IO;
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
      Put_Line ("Closing channel" & Chan.Number'Image & "...");

      -- Send Channel.Close
      Close_Method.Reply_Code := AMQP.Constants.REPLY_SUCCESS;
      Close_Method.Reply_Text := new String'("Normal shutdown");
      Close_Method.Class_Id := 0;
      Close_Method.Method_Id := 0;

      Reset (Args_Buf);
      Encode_Channel_Close (Args_Buf, Close_Method);
      Send_Method (Chan, AMQP.Constants.CLASS_CHANNEL, AMQP.Constants.CHANNEL_CLOSE, Args_Buf);

      Put_Line ("Sent Channel.Close");

      -- Wait for Channel.Close-Ok
      Put_Line ("Waiting for Channel.Close-Ok...");
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
      Put_Line ("Channel" & Chan.Number'Image & " closed successfully");
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

end AMQP.Channel;
