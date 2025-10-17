# AMQP Ada - AMQP 0-9-1 Protocol Implementation in Ada

An Ada implementation of the AMQP 0-9-1 protocol for connecting to message brokers like RabbitMQ.

## Project Status

This is an early-stage implementation with basic infrastructure in place.

### Completed
- [x] Project structure and build configuration
- [x] Basic AMQP types (Octet, Short, Long, Long_Long)
- [x] Protocol constants (frame types, class IDs, method IDs)
- [x] Basic codec (encoding/decoding primitives)
- [x] Field table structure definitions
- [x] Frame processing layer
- [x] Socket connection management (using GNAT.Sockets)

### In Progress / TODO
- [ ] Connection handshake (Start, Tune, Open)
- [ ] Channel management
- [ ] Basic.Publish implementation
- [ ] Basic.Consume implementation
- [ ] Queue/Exchange operations
- [ ] Acknowledgments and confirms
- [ ] Error handling and recovery
- [ ] Heartbeat mechanism
- [ ] TLS/SSL support

## Building

```bash
gprbuild -P amqp.gpr
```

## Running Tests

### Start RabbitMQ (for connection tests)

```bash
docker run -d --rm --name rabbitmq -p 5672:5672 rabbitmq:3
```

### Run Tests

```bash
# Basic tests (codec, frames) - run inside distrobox
distrobox enter fedora-42-dev -- /var/home/geewiz/Projects/src/geewiz/amqp_ada/bin/amqp_test

# Connection test (requires RabbitMQ running on localhost:5672)
distrobox enter fedora-42-dev -- /var/home/geewiz/Projects/src/geewiz/amqp_ada/bin/connection_test
```

### Stop RabbitMQ

```bash
docker stop rabbitmq
```

## Architecture

### Package Structure

```
AMQP                      -- Root package, version info, exceptions
├── AMQP.Constants        -- Protocol constants (frame types, method IDs)
├── AMQP.Types            -- Basic types and data structures
├── AMQP.Codec            -- Encoding/decoding operations
├── AMQP.Frames          -- Frame assembly/disassembly (TODO)
├── AMQP.Connection      -- Connection management (TODO)
├── AMQP.Channel         -- Channel operations (TODO)
└── AMQP.Methods         -- Method implementations (TODO)
```

### Implementation Phases

**Phase 1: Foundation (Current)**
- Core types and constants
- Binary encoding/decoding
- Frame structure

**Phase 2: Network Layer**
- TCP socket connection
- Frame sending/receiving
- Heartbeat handling

**Phase 3: Connection Management**
- Connection handshake (Start, Tune, Open)
- Authentication (PLAIN mechanism)
- Connection close

**Phase 4: Channel Operations**
- Channel open/close
- Channel multiplexing
- Flow control

**Phase 5: Basic Operations**
- Queue declare/bind/delete
- Exchange declare/bind/delete
- Basic.Publish
- Basic.Consume
- Basic.Ack/Nack/Reject

**Phase 6: Advanced Features**
- Transactions (Tx class)
- Publisher confirms
- Consumer cancellation
- Message properties

## Design Decisions

### Concurrency Model
 Use Ada tasks for asynchronous message delivery
 Protected objects for shared state (channels, pending confirms)
 One reader task per connection
 Separate task per consumer

### Memory Management
- Controlled types for automatic resource cleanup
- Access types for dynamic structures (field tables, strings)
- Consider using storage pools for message buffers

## Key Challenges

1. **Frame Assembly**: Managing partial frames from TCP stream
2. **Channel Multiplexing**: Thread-safe channel state management
3. **Method Dispatch**: Mapping class/method IDs to handlers
4. **Content Frames**: Handling split content (header + body frames)
5. **Error Recovery**: Proper connection/channel error handling

## Example Usage (Planned)

```ada
with AMQP.Connection;
with AMQP.Channel;

procedure Example is
   Conn : AMQP.Connection.Connection_Type;
   Chan : AMQP.Channel.Channel_Type;
begin
   -- Connect
   Conn.Connect (Host => "localhost", Port => 5672,
                 User => "guest", Password => "guest");

   -- Open channel
   Chan := Conn.Create_Channel;

   -- Declare queue
   Chan.Queue_Declare (Queue => "test_queue", Durable => True);

   -- Publish message
   Chan.Basic_Publish (
      Exchange => "",
      Routing_Key => "test_queue",
      Body => "Hello, AMQP!"
   );

   -- Consume
   Chan.Basic_Consume (
      Queue => "test_queue",
      Callback => My_Message_Handler'Access
   );

   -- Cleanup
   Chan.Close;
   Conn.Close;
end Example;
```

## Testing Strategy

1. **Unit Tests**: Test codec, frame processing independently
2. **Integration Tests**: Connect to local RabbitMQ instance
3. **Interoperability**: Test against official AMQP test suite
4. **Performance**: Benchmark throughput and latency

## Resources

- [AMQP 0-9-1 Specification](https://www.rabbitmq.com/resources/specs/amqp0-9-1.pdf)
- [RabbitMQ Protocol Tutorial](https://www.rabbitmq.com/tutorials/amqp-concepts.html)
- [Ada Reference Manual](http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-TOC.html)

## License

TBD

## Contributing

This is a work in progress. Contributions welcome!
