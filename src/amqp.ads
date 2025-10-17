-- Root package for AMQP 0-9-1 Ada library
package AMQP is

   pragma Pure;

   -- Library version
   VERSION : constant String := "0.1.0";

   -- Exceptions
   Protocol_Error : exception;
   Connection_Error : exception;
   Channel_Error : exception;
   Frame_Error : exception;
   Encoding_Error : exception;

end AMQP;
