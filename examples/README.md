# AMQP Ada Examples

This directory contains example programs demonstrating the use of the AMQP Ada library.

## Examples

### Publisher (`publisher.adb`)

A simple message publisher that continuously sends messages to four different queues:
- `sensor.temperature` - Temperature sensor readings
- `sensor.humidity` - Humidity sensor readings
- `log.info` - Informational log messages
- `log.error` - Error log messages

The publisher sends messages in a loop with a small delay between each message.

### Subscriber (`subscriber.adb`)

A selective message subscriber that demonstrates topic-based filtering by subscribing to only a **subset** of the queues that the publisher sends to:
- ✅ `sensor.temperature` (subscribed)
- ✅ `sensor.humidity` (subscribed)
- ❌ `log.info` (not subscribed)
- ❌ `log.error` (not subscribed)

The subscriber will only receive and display messages from the sensor queues, ignoring all log messages.

## Building

### Build both examples:
```bash
gprbuild -P examples.gpr
```

### Build individually:
```bash
gprbuild -P examples.gpr publisher.adb
gprbuild -P examples.gpr subscriber.adb
```

The executables are placed in the `examples/` directory.

## Running

### Prerequisites

Start RabbitMQ:
```bash
docker run -d --rm --name rabbitmq -p 5672:5672 rabbitmq:3-management
```

### Running the examples

**Option 1: Publisher and Subscriber together** (recommended)

In one terminal, start the subscriber:
```bash
./examples/subscriber
```

In another terminal, start the publisher:
```bash
./examples/publisher
```

You should see:
- The **publisher** sending messages to all 4 queues
- The **subscriber** receiving only messages from the 2 sensor queues

**Option 2: Publisher only**
```bash
./examples/publisher
```

Press Ctrl+C to stop.

### Expected Output

**Publisher:**
```
AMQP Publisher Example
======================
Publishing to queues:
  - sensor.temperature
  - sensor.humidity
  - log.info
  - log.error

Connecting to RabbitMQ...
Connected!

Channel opened

Publishing messages (Ctrl+C to stop)...

[sensor.temperature] Temperature reading:  1
[sensor.humidity] Humidity reading:  2
[log.info] INFO: System status:  3
[log.error] ERROR: Alert condition:  4
...
```

**Subscriber:**
```
AMQP Subscriber Example
=======================
Subscribing to topics:
  - sensor.temperature (Temperature sensor data)
  - sensor.humidity (Humidity sensor data)
(Not subscribing to log.* topics)

Connecting to RabbitMQ...
Connected!

Channel opened

Setting up subscriptions...
  Subscribed to: sensor.temperature
  Subscribed to: sensor.humidity

Waiting for messages (Ctrl+C to stop)...
------------------------------------------

[sensor.temperature] Temperature reading:  1
[sensor.humidity] Humidity reading:  2
[sensor.temperature] Temperature reading:  5
[sensor.humidity] Humidity reading:  6
...
```

Notice that the subscriber **does not** receive messages from `log.info` or `log.error` queues.

### Cleanup

Stop RabbitMQ when done:
```bash
docker stop rabbitmq
```

## Key Concepts Demonstrated

1. **Connection Management**: Establishing AMQP connections with authentication
2. **Channel Operations**: Opening channels for communication
3. **Queue Declaration**: Creating queues with specific properties
4. **Message Publishing**: Sending messages to queues via the default exchange
5. **Selective Subscription**: Subscribing to only specific queues of interest
6. **Message Consumption**: Receiving and processing messages
7. **Message Acknowledgment**: Properly acknowledging received messages
8. **Clean Shutdown**: Properly closing channels and connections

## Modifying the Examples

### To change which topics the subscriber receives:

Edit `subscriber.adb` and modify the `Subscribed_Queues` array:

```ada
Subscribed_Queues : constant array (1 .. 2) of Queue_Info := (
   (new String'("sensor.temperature"), new String'("Temperature sensor data")),
   (new String'("log.error"), new String'("Error logs"))  -- Changed!
);
```

### To add more topics to the publisher:

Edit `publisher.adb` and modify the `Topics` array:

```ada
Topics : constant array (1 .. 5) of Topic_Info := (
   (new String'("sensor.temperature"), new String'("Temperature reading: ")),
   (new String'("sensor.humidity"), new String'("Humidity reading: ")),
   (new String'("sensor.pressure"), new String'("Pressure reading: ")),  -- New!
   (new String'("log.info"), new String'("INFO: System status: ")),
   (new String'("log.error"), new String'("ERROR: Alert condition: "))
);
```

### To change message frequency:

Edit `publisher.adb` and modify the delay statements:

```ada
delay 0.5;  -- Delay between individual messages
delay 1.0;  -- Delay between rounds
```
