-- AMQP URL Parsing
-- Parses AMQP connection URLs in the standard format:
--   amqp://[username:password@]host[:port][/vhost]
--
-- Note: Only plain AMQP (amqp://) is supported. TLS (amqps://) is not yet implemented.
--
-- Examples:
--   amqp://localhost
--   amqp://localhost:5672
--   amqp://guest:guest@localhost:5672/
--   amqp://user:pass@rabbitmq.example.com:5672/myvhost

with AMQP.Connection;

package AMQP.URL is

   -- Exception raised when URL parsing fails
   Parse_Error : exception;

   -- Parse an AMQP URL and return a connection configuration
   function Parse (URL : String) return AMQP.Connection.Connection_Config;

end AMQP.URL;
