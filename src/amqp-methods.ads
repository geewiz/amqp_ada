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

   -- Queue.Declare (client -> server)
   type Queue_Declare is record
      Reserved_1 : Short := 0;          -- Deprecated
      Queue : String_Access;            -- Queue name (empty = server-generated)
      Passive : Boolean := False;       -- Only check if queue exists
      Durable : Boolean := False;       -- Survive server restart
      Exclusive : Boolean := False;     -- Used by only one connection
      Auto_Delete : Boolean := False;   -- Delete when unused
      No_Wait : Boolean := False;       -- Don't wait for reply
      Arguments : Field_Table;          -- Optional arguments
   end record;

   -- Queue.Declare-Ok (server -> client)
   type Queue_Declare_Ok is record
      Queue : String_Access;            -- Queue name
      Message_Count : Long;             -- Number of messages
      Consumer_Count : Long;            -- Number of consumers
   end record;

   -- Basic.Publish (client -> server)
   type Basic_Publish is record
      Reserved_1 : Short := 0;          -- Deprecated
      Exchange : String_Access;         -- Exchange name
      Routing_Key : String_Access;      -- Routing key
      Mandatory : Boolean := False;     -- Return if unroutable
      Immediate : Boolean := False;     -- Return if no consumers (deprecated)
   end record;

   -- Basic.Consume (client -> server)
   type Basic_Consume is record
      Reserved_1 : Short := 0;          -- Deprecated
      Queue : String_Access;            -- Queue name
      Consumer_Tag : String_Access;     -- Consumer identifier
      No_Local : Boolean := False;      -- Don't send to same connection
      No_Ack : Boolean := False;        -- No acknowledgment needed
      Exclusive : Boolean := False;     -- Exclusive consumer
      No_Wait : Boolean := False;       -- Don't wait for reply
      Arguments : Field_Table;          -- Optional arguments
   end record;

   -- Basic.Consume-Ok (server -> client)
   type Basic_Consume_Ok is record
      Consumer_Tag : String_Access;     -- Consumer identifier
   end record;

   -- Basic.Deliver (server -> client)
   type Basic_Deliver is record
      Consumer_Tag : String_Access;     -- Consumer identifier
      Delivery_Tag : Long_Long;         -- Server-assigned delivery tag
      Redelivered : Boolean;            -- Message was redelivered
      Exchange : String_Access;         -- Exchange name
      Routing_Key : String_Access;      -- Routing key
   end record;

   -- Basic.Ack (client -> server)
   type Basic_Ack is record
      Delivery_Tag : Long_Long;         -- Delivery tag to acknowledge
      Multiple : Boolean := False;      -- Ack all up to this tag
   end record;

   -- Content header (for messages)
   type Content_Header is record
      Class_Id : Short;                 -- Always CLASS_BASIC for messages
      Weight : Short := 0;              -- Deprecated
      Body_Size : Long_Long;            -- Total message body size
      -- Property flags and properties would go here (simplified for now)
   end record;

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

   -- Queue method encoding
   procedure Encode_Queue_Declare (
      Buf : in out Buffer;
      Method : Queue_Declare
   );

   -- Queue method decoding
   procedure Decode_Queue_Declare_Ok (
      Buf : in out Buffer;
      Method : out Queue_Declare_Ok;
      Success : out Boolean
   );

   -- Basic method encoding
   procedure Encode_Basic_Publish (
      Buf : in out Buffer;
      Method : Basic_Publish
   );

   procedure Encode_Basic_Consume (
      Buf : in out Buffer;
      Method : Basic_Consume
   );

   procedure Encode_Basic_Ack (
      Buf : in out Buffer;
      Method : Basic_Ack
   );

   procedure Encode_Content_Header (
      Buf : in out Buffer;
      Method : Content_Header
   );

   -- Basic method decoding
   procedure Decode_Basic_Consume_Ok (
      Buf : in out Buffer;
      Method : out Basic_Consume_Ok;
      Success : out Boolean
   );

   procedure Decode_Basic_Deliver (
      Buf : in out Buffer;
      Method : out Basic_Deliver;
      Success : out Boolean
   );

   procedure Decode_Content_Header (
      Buf : in out Buffer;
      Method : out Content_Header;
      Success : out Boolean
   );

   -- Helper: Create PLAIN authentication response
   function Create_Plain_Auth_Response (
      Username : String;
      Password : String
   ) return String_Access;

end AMQP.Methods;
