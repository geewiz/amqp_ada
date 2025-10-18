# AMQP Ada - AMQP 0-9-1 Protocol Implementation in Ada

An Ada implementation of the AMQP 0-9-1 protocol for connecting to message brokers like RabbitMQ.

## Project Status

Functional AMQP client library with core messaging operations implemented and tested against RabbitMQ.

### Completed
- [x] Project structure and build configuration
- [x] Basic AMQP types (Octet, Short, Long, Long_Long)
- [x] Protocol constants (frame types, class IDs, method IDs)
- [x] Binary encoding/decoding (primitives, strings, field tables)
- [x] Frame processing (assembly, disassembly, validation)
- [x] Socket connection management (using GNAT.Sockets)
- [x] Connection handshake (Start, Tune, Open)
- [x] PLAIN authentication mechanism
- [x] Channel management (open, close, multiplexing)
- [x] Queue.Declare operation
- [x] Basic.Publish implementation
- [x] Basic.Consume implementation
- [x] Basic.Get for receiving messages
- [x] Basic.Ack for message acknowledgment
- [x] Debug output system (pragma Debug with compile-time control)
- [x] Makefile for building and testing
- [x] Comprehensive test suite

### TODO
- [ ] Exchange operations (declare, bind, delete)
- [ ] Additional queue operations (bind, unbind, purge, delete)
- [ ] Basic.Nack and Basic.Reject
- [ ] QoS settings (prefetch count/size)
- [ ] Consumer cancellation (Basic.Cancel)
- [ ] Message properties (content-type, headers, etc.)
- [ ] Publisher confirms
- [ ] Transactions (Tx class)
- [ ] Heartbeat mechanism
- [ ] Error recovery and reconnection logic
- [ ] TLS/SSL support
- [ ] Memory cleanup (proper deallocation of access types)

## Building

### Using Make (Recommended)

```bash
make              # Build release version (default)
make build        # Same as above
make debug        # Build with debug output enabled
make clean        # Remove build artifacts
make clean-all    # Remove all generated files including binaries
make test         # Build and run all tests (requires RabbitMQ)
make help         # Show all available targets
```

### Using GPRBuild Directly

```bash
# Release build (no debug output)
gprbuild -P amqp.gpr

# Debug build (with verbose protocol output)
gprbuild -P amqp_debug.gpr
```

Executables are built into the `test/` directory.

## Running Tests

### Start RabbitMQ

```bash
docker run -d --rm --name rabbitmq -p 5672:5672 rabbitmq:3-management
```

### Run Individual Tests

```bash
./test/amqp_test         # Basic codec and frame tests
./test/connection_test   # Socket connection test
./test/handshake_test    # AMQP handshake test
./test/channel_test      # Channel management test
./test/pubsub_test       # Complete publish/subscribe test
```

### Run All Tests

```bash
make test
```

### Stop RabbitMQ

```bash
docker stop rabbitmq
```

## Architecture

### Package Structure

```
AMQP                      -- Root package, version info, exceptions
├── AMQP.Constants        -- Protocol constants (frame types, method IDs, reply codes)
├── AMQP.Types            -- Basic types (Octet, Short, Long, etc.) and field tables
├── AMQP.Codec            -- Binary encoding/decoding operations
├── AMQP.Frames           -- Frame assembly/disassembly and validation
├── AMQP.Connection       -- Connection management (handshake, send/receive frames)
├── AMQP.Channel          -- Channel operations (open, close, queue, basic methods)
└── AMQP.Methods          -- AMQP method structures and encoding/decoding
```

### Implementation Phases

**Phase 1: Foundation** ✅ **Complete**
- Core types and constants
- Binary encoding/decoding
- Frame structure

**Phase 2: Network Layer** ✅ **Complete**
- TCP socket connection
- Frame sending/receiving
- Buffer management

**Phase 3: Connection Management** ✅ **Complete**
- Connection handshake (Start, Tune, Open)
- Authentication (PLAIN mechanism)
- Connection close
- Parameter negotiation

**Phase 4: Channel Operations** ✅ **Complete**
- Channel open/close
- Channel multiplexing
- Basic error handling

**Phase 5: Basic Operations** ✅ **Partially Complete**
- Queue.Declare ✅
- Basic.Publish ✅
- Basic.Consume ✅
- Basic.Get ✅
- Basic.Ack ✅
- Queue bind/unbind ⏳
- Exchange operations ⏳
- Basic.Nack/Reject ⏳

**Phase 6: Advanced Features** ⏳ **Not Started**
- Transactions (Tx class)
- Publisher confirms
- Consumer cancellation
- Message properties
- Heartbeat mechanism
- Error recovery

## Design Decisions

### Synchronous API
- Currently implemented as synchronous/blocking operations
- Simple and predictable for basic use cases
- Future: Add asynchronous/task-based API for concurrent consumers

### Memory Management
- Access types for dynamic structures (field tables, strings)
- Manual cleanup required (TODO: implement Controlled types for automatic resource cleanup)
- Fixed-size buffers for frame processing (8KB default)

### Debug Output
- Uses `pragma Debug` for compile-time control
- Zero runtime overhead in release builds
- Enabled with `make debug` or `gprbuild -P amqp_debug.gpr`

## Key Challenges

1. **Frame Assembly**: Managing partial frames from TCP stream
2. **Channel Multiplexing**: Thread-safe channel state management
3. **Method Dispatch**: Mapping class/method IDs to handlers
4. **Content Frames**: Handling split content (header + body frames)
5. **Error Recovery**: Proper connection/channel error handling

## Example Usage

Complete, working examples are available in the [`examples/`](examples/) directory:

- **[`publisher.adb`](examples/publisher.adb)** - Continuously publishes messages to multiple queues
- **[`subscriber.adb`](examples/subscriber.adb)** - Subscribes to specific queues and processes messages

See [`examples/README.md`](examples/README.md) for detailed documentation on building and running the examples.

### Quick Start

```bash
# Start RabbitMQ
docker run -d --rm --name rabbitmq -p 5672:5672 rabbitmq:3-management

# Build examples
gprbuild -P examples.gpr

# Run subscriber (in one terminal)
./examples/subscriber

# Run publisher (in another terminal)
./examples/publisher
```

## Testing Strategy

1. **Unit Tests** ✅: Codec and frame processing (`amqp_test`)
2. **Integration Tests** ✅: Connection, handshake, channels against RabbitMQ
3. **End-to-End Tests** ✅: Complete publish/subscribe workflow (`pubsub_test`)
4. **Interoperability** ⏳: Test against official AMQP test suite (planned)
5. **Performance** ⏳: Benchmark throughput and latency (planned)

## Resources

- [AMQP 0-9-1 Specification](https://www.rabbitmq.com/resources/specs/amqp0-9-1.pdf)
- [RabbitMQ Protocol Tutorial](https://www.rabbitmq.com/tutorials/amqp-concepts.html)
- [Ada Reference Manual](http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-TOC.html)

## License

This project is licensed under the Apache License 2.0 - see the [LICENSE](LICENSE) file for details.

## Contributing

This is a work in progress. Contributions welcome!
