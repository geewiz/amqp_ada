-- AMQP 0-9-1 Method Definitions
with AMQP.Types; use AMQP.Types;
with AMQP.Codec; use AMQP.Codec;

package AMQP.Methods is

   type String_Access is access constant String;

   -- Connection.Start (server -> client)
   type Connection_Start is record
      Version_Major : Octet;
      Version_Minor : Octet;
      Server_Properties : Field_Table;
      Mechanisms : String_Access;  -- Space-separated list (e.g., "PLAIN AMQPLAIN")
      Locales : String_Access;     -- Space-separated list (e.g., "en_US")
   end record;

   -- Connection.Start-Ok (client -> server)
   type Connection_Start_Ok is record
      Client_Properties : Field_Table;
      Mechanism : String_Access;   -- Selected mechanism (e.g., "PLAIN")
      Response : String_Access;    -- Authentication response
      Locale : String_Access;      -- Selected locale (e.g., "en_US")
   end record;

   -- Connection.Tune (server -> client)
   type Connection_Tune is record
      Channel_Max : Short;  -- Max channels (0 = no limit)
      Frame_Max : Long;     -- Max frame size (0 = no limit)
      Heartbeat : Short;    -- Heartbeat interval in seconds (0 = disabled)
   end record;

   -- Connection.Tune-Ok (client -> server)
   type Connection_Tune_Ok is record
      Channel_Max : Short;
      Frame_Max : Long;
      Heartbeat : Short;
   end record;

   -- Connection.Open (client -> server)
   type Connection_Open is record
      Virtual_Host : String_Access;  -- Virtual host path (e.g., "/")
      Reserved_1 : String_Access;    -- Deprecated
      Reserved_2 : Boolean;          -- Deprecated
   end record;

   -- Connection.Open-Ok (server -> client)
   type Connection_Open_Ok is record
      Reserved : String_Access;  -- Deprecated
   end record;

   -- Connection.Close (bidirectional)
   type Connection_Close is record
      Reply_Code : Short;
      Reply_Text : String_Access;
      Class_Id : Short;
      Method_Id : Short;
   end record;

   -- Connection.Close-Ok (bidirectional)
   type Connection_Close_Ok is null record;

   -- Channel.Open (client -> server)
   type Channel_Open is record
      Reserved : String_Access;  -- Deprecated
   end record;

   -- Channel.Open-Ok (server -> client)
   type Channel_Open_Ok is record
      Reserved : String_Access;  -- Deprecated
   end record;

   -- Channel.Close (bidirectional)
   type Channel_Close is record
      Reply_Code : Short;
      Reply_Text : String_Access;
      Class_Id : Short;
      Method_Id : Short;
   end record;

   -- Channel.Close-Ok (bidirectional)
   type Channel_Close_Ok is null record;

   -- Encoding procedures
   procedure Encode_Connection_Start_Ok (
      Buf : in out Buffer;
      Method : Connection_Start_Ok
   );

   procedure Encode_Connection_Tune_Ok (
      Buf : in out Buffer;
      Method : Connection_Tune_Ok
   );

   procedure Encode_Connection_Open (
      Buf : in out Buffer;
      Method : Connection_Open
   );

   procedure Encode_Connection_Close (
      Buf : in out Buffer;
      Method : Connection_Close
   );

   procedure Encode_Connection_Close_Ok (
      Buf : in out Buffer;
      Method : Connection_Close_Ok
   );

   -- Decoding procedures
   procedure Decode_Connection_Start (
      Buf : in out Buffer;
      Method : out Connection_Start;
      Success : out Boolean
   );

   procedure Decode_Connection_Tune (
      Buf : in out Buffer;
      Method : out Connection_Tune;
      Success : out Boolean
   );

   procedure Decode_Connection_Open_Ok (
      Buf : in out Buffer;
      Method : out Connection_Open_Ok;
      Success : out Boolean
   );

   procedure Decode_Connection_Close (
      Buf : in out Buffer;
      Method : out Connection_Close;
      Success : out Boolean
   );

   procedure Decode_Connection_Close_Ok (
      Buf : in out Buffer;
      Method : out Connection_Close_Ok;
      Success : out Boolean
   );

   -- Channel method encoding
   procedure Encode_Channel_Open (
      Buf : in out Buffer;
      Method : Channel_Open
   );

   procedure Encode_Channel_Close (
      Buf : in out Buffer;
      Method : Channel_Close
   );

   procedure Encode_Channel_Close_Ok (
      Buf : in out Buffer;
      Method : Channel_Close_Ok
   );

   -- Channel method decoding
   procedure Decode_Channel_Open_Ok (
      Buf : in out Buffer;
      Method : out Channel_Open_Ok;
      Success : out Boolean
   );

   procedure Decode_Channel_Close (
      Buf : in out Buffer;
      Method : out Channel_Close;
      Success : out Boolean
   );

   procedure Decode_Channel_Close_Ok (
      Buf : in out Buffer;
      Method : out Channel_Close_Ok;
      Success : out Boolean
   );

   -- Helper: Create PLAIN authentication response
   function Create_Plain_Auth_Response (
      Username : String;
      Password : String
   ) return String_Access;

end AMQP.Methods;
