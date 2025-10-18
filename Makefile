.PHONY: all build debug clean clean-all test help

# Default target
all: build

# Build release version
build:
	gprbuild -P amqp.gpr

# Build debug version with assertions enabled
debug:
	gprbuild -P amqp_debug.gpr

# Clean build artifacts
clean:
	gprclean -P amqp.gpr
	rm -f *.stderr *.stdout
	rm -f *.o *.ali *.lexch *.bexch
	rm -f b__*.adb b__*.ads b~*.adb b~*.ads

# Clean everything including test binaries
clean-all: clean
	rm -f test/amqp_test test/connection_test test/handshake_test test/channel_test test/pubsub_test test/url_test
	rm -rf obj

# Run basic tests (requires RabbitMQ running on localhost:5672)
test: build
	@echo "Running connection test..."
	./test/connection_test
	@echo ""
	@echo "Running handshake test..."
	./test/handshake_test
	@echo ""
	@echo "Running channel test..."
	./test/channel_test
	@echo ""
	@echo "Running pubsub test..."
	./test/pubsub_test

# Help target
help:
	@echo "AMQP Ada Library - Build System"
	@echo ""
	@echo "Available targets:"
	@echo "  make build      - Build release version (default)"
	@echo "  make debug      - Build debug version with verbose output"
	@echo "  make clean      - Remove build artifacts"
	@echo "  make clean-all  - Remove all generated files including binaries"
	@echo "  make test       - Build and run all tests (requires RabbitMQ)"
	@echo "  make help       - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make build                    # Build release version"
	@echo "  make debug                    # Build with debug output"
	@echo "  make clean && make debug      # Clean rebuild in debug mode"
