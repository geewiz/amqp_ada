-- AMQP 0-9-1 Protocol Constants
package AMQP.Constants is

   pragma Pure;

   -- Protocol version
   PROTOCOL_VERSION_MAJOR : constant := 0;
   PROTOCOL_VERSION_MINOR : constant := 9;
   PROTOCOL_VERSION_REVISION : constant := 1;

   -- Frame types
   FRAME_METHOD    : constant := 1;
   FRAME_HEADER    : constant := 2;
   FRAME_BODY      : constant := 3;
   FRAME_HEARTBEAT : constant := 8;

   -- Frame constants
   FRAME_END      : constant := 16#CE#;
   FRAME_MIN_SIZE : constant := 4096;

   -- Class IDs
   CLASS_CONNECTION : constant := 10;
   CLASS_CHANNEL    : constant := 20;
   CLASS_EXCHANGE   : constant := 40;
   CLASS_QUEUE      : constant := 50;
   CLASS_BASIC      : constant := 60;
   CLASS_TX         : constant := 90;
   CLASS_CONFIRM    : constant := 85;

   -- Connection method IDs
   CONNECTION_START     : constant := 10;
   CONNECTION_START_OK  : constant := 11;
   CONNECTION_SECURE    : constant := 20;
   CONNECTION_SECURE_OK : constant := 21;
   CONNECTION_TUNE      : constant := 30;
   CONNECTION_TUNE_OK   : constant := 31;
   CONNECTION_OPEN      : constant := 40;
   CONNECTION_OPEN_OK   : constant := 41;
   CONNECTION_CLOSE     : constant := 50;
   CONNECTION_CLOSE_OK  : constant := 51;

   -- Channel method IDs
   CHANNEL_OPEN     : constant := 10;
   CHANNEL_OPEN_OK  : constant := 11;
   CHANNEL_FLOW     : constant := 20;
   CHANNEL_FLOW_OK  : constant := 21;
   CHANNEL_CLOSE    : constant := 40;
   CHANNEL_CLOSE_OK : constant := 41;

   -- Queue method IDs
   QUEUE_DECLARE    : constant := 10;
   QUEUE_DECLARE_OK : constant := 11;
   QUEUE_BIND       : constant := 20;
   QUEUE_BIND_OK    : constant := 21;
   QUEUE_UNBIND     : constant := 50;
   QUEUE_UNBIND_OK  : constant := 51;
   QUEUE_PURGE      : constant := 30;
   QUEUE_PURGE_OK   : constant := 31;
   QUEUE_DELETE     : constant := 40;
   QUEUE_DELETE_OK  : constant := 41;

   -- Basic method IDs
   BASIC_QOS        : constant := 10;
   BASIC_QOS_OK     : constant := 11;
   BASIC_CONSUME    : constant := 20;
   BASIC_CONSUME_OK : constant := 21;
   BASIC_CANCEL     : constant := 30;
   BASIC_CANCEL_OK  : constant := 31;
   BASIC_PUBLISH    : constant := 40;
   BASIC_RETURN     : constant := 50;
   BASIC_DELIVER    : constant := 60;
   BASIC_GET        : constant := 70;
   BASIC_GET_OK     : constant := 71;
   BASIC_GET_EMPTY  : constant := 72;
   BASIC_ACK        : constant := 80;
   BASIC_REJECT     : constant := 90;
   BASIC_RECOVER_ASYNC : constant := 100;
   BASIC_RECOVER    : constant := 110;
   BASIC_RECOVER_OK : constant := 111;
   BASIC_NACK       : constant := 120;

   -- Reply codes
   REPLY_SUCCESS             : constant := 200;
   CONTENT_TOO_LARGE         : constant := 311;
   NO_ROUTE                  : constant := 312;
   NO_CONSUMERS              : constant := 313;
   CONNECTION_FORCED         : constant := 320;
   INVALID_PATH              : constant := 402;
   ACCESS_REFUSED            : constant := 403;
   NOT_FOUND                 : constant := 404;
   RESOURCE_LOCKED           : constant := 405;
   PRECONDITION_FAILED       : constant := 406;
   FRAME_ERROR               : constant := 501;
   SYNTAX_ERROR              : constant := 502;
   COMMAND_INVALID           : constant := 503;
   CHANNEL_ERROR             : constant := 504;
   UNEXPECTED_FRAME          : constant := 505;
   RESOURCE_ERROR            : constant := 506;
   NOT_ALLOWED               : constant := 530;
   NOT_IMPLEMENTED           : constant := 540;
   INTERNAL_ERROR            : constant := 541;

end AMQP.Constants;
