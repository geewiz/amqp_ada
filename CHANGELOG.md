# Changelog

I based the format on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

**IMPORTANT:** Until the project reaches maturity with a version 1.0 release,
expect breaking changes even in minor version bumps.

## [0.2.0] - 2025-10-18

### Changed

- **BREAKING**: Refactored Channel type to use discriminant for Connection reference
  - Channel now declared as: `Chan : aliased Channel (Conn'Access)`
  - Open procedure no longer takes Connection parameter: `Open (Chan, Channel_Number)`
  - Eliminates all `Unchecked_Access` usage in favor of safe `'Access`
  - Connection reference is now part of Channel's identity (discriminant pattern)

## [0.1.0] - 2025-10-18

### Added

- Initial AMQP 0-9-1 client library implementation
- Core protocol types and constants
- Binary encoding/decoding (Codec package)
- Frame processing (assembly, disassembly, validation)
- Socket connection management using GNAT.Sockets
- Connection handshake (Start, Tune, Open with PLAIN authentication)
- Proper AMQP connection close handshake (Connection.Close/Close-Ok)
- Channel management (open, close, multiplexing)
- Queue.Declare operation
- Basic.Publish implementation (3-frame sequence)
- Basic.Consume implementation
- Basic.Get for polling message retrieval
- Basic.Ack for message acknowledgment
- Debug output system using pragma Debug (compile-time control)
- Create_Config factory function for simplified connection setup
- Connect_And_Authenticate convenience method
- AMQP.URL package for parsing connection URLs (amqp://user:pass@host:port/vhost)
- Comprehensive test suite (connection, handshake, channel, pubsub, URL parsing)
- Example publisher and subscriber clients demonstrating selective topic subscription
- Makefile for building, cleaning, and testing
- Complete documentation in README.md and examples/README.md

### Implementation Notes

- Synchronous/blocking API
- Frame size: 8KB default
- Support for default exchange routing
- TLS (amqps://) not yet implemented
- Access types used for dynamic strings in configuration

[0.2.0]: https://github.com/your-repo/amqp_ada/releases/tag/v0.2.0
[0.1.0]: https://github.com/your-repo/amqp_ada/releases/tag/v0.1.0
