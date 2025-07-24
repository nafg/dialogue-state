# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A Scala library for building voice applications with Twilio and Telnyx by defining call flows as structured CallTree
programs. The library provides type-safe abstractions for Interactive Voice Response (IVR) systems using ZIO effects.

## Build System

This project uses **Bleep** (not sbt). All commands use `bleep` instead of `sbt`:

### Common Commands

- `bleep compile` - Compile all projects
- `bleep test` - Run all tests
- `bleep test [project-name]` - Run tests for a specific project
- `bleep test --only [TestClassName]` - Run specific test class

### Project Names

- `dialogue-state-core` / `dialogue-state-core-test` - Core library and tests
- `twilio` / `twilio-test` - Twilio-specific implementation
- `twilio-base` - Shared Twilio/Telnyx base classes
- `telnyx` / `telnyx-test` - Telnyx-specific implementation

### Cross-Compilation

Projects are cross-compiled for Scala 2.13.16 and 3.3.6. Specify a version with `@scala2` or `@scala3` suffix:

- `bleep test twilio-test@scala3`
- `bleep compile dialogue-state-core@scala2`

## Architecture

### Core Concepts

**CallTree**: The fundamental abstraction representing call flow logic. CallTrees are immutable, composable structures
that can be combined using the `&:` operator for sequencing.

**CallState**: Server-side state machine tracking the current call state:

- `Ready(callTree)` - Ready to execute the next CallTree
- `AwaitingDigits(gather)` - Waiting for DTMF input
- `AwaitingPayment(pay)` - Waiting for payment processing
- `AwaitingRecording(record)` - Waiting for voice recording

**CallStateServer**: Abstract base class that interprets CallTrees and manages state transitions. Provider-specific
implementations handle webhook parsing and response generation.

### CallTree Types

- **NoContinuation**: Terminal operations (Say, Play, Pause)
- **HasContinuation**: Operations requiring caller response (Gather, Pay, Record)
- **Sequence**: Combines multiple CallTrees for sequential execution

Key CallTree classes:

- `CallTree.Say` - Text-to-speech output
- `CallTree.Gather` - Collect DTMF digits with customizable prompts
- `CallTree.Pay` - Payment processing (OneTime, Reusable, PaymentMethod)
- `CallTree.Record` - Voice recording with optional transcription
- `Menu[A]` - Type-safe menu abstraction with automatic numbering

### Provider Architecture

**Base Layer** (`twilio-base`):

- `TwilioBaseCallStateServer` - Common webhook handling and XML generation
- `Node` - Provider-agnostic representation of voice operations
- `TagsBundle` - ScalaTags extensions for XML generation

**Provider Implementations**:

- `TwilioCallStateServer` - Full Twilio support including payments
- `TelnyxCallStateServer` - Telnyx support (payments not supported)

Each provider converts CallTrees to Node representations, then to provider-specific XML/TwiML.

### XML Generation Pattern

Uses ScalaTags with custom TypedTag constructors to generate valid TwiML:

```scala
val Gather = TypedTag[String]("Gather", modifiers = Nil, void = false)
```

This bypasses ScalaTags' HTML validation which rejects PascalCase XML elements like `<Gather>`.

## Testing Framework

**CallTreeTester**: Framework for unit testing CallTree programs by simulating caller interactions.

Key methods:

- `CallTreeTester(callTree)` - Create tester instance
- `tester.expect(text)` - Assert expected speech output
- `tester.sendDigits(digits)` - Simulate DTMF input
- `tester.sendRecording(url, terminator)` - Simulate recording completion
- `tester.sendTranscribedRecording(url, text, terminator)` - Simulate transcribed recording
- `tester.sendPayment(result)` - Simulate payment result
- `tester.expectEnded` - Assert call flow completion

Testing pattern:

```scala
for {
  tester <- CallTreeTester(myCallTree)
  _ <- tester.expect("Welcome message")
  _ <- tester.sendDigits("1")
  _ <- tester.expect("Option 1 selected")
  _ <- tester.expectEnded
} yield assertCompletes
```

## Key Patterns

### Error Handling

- `CallTree.Failure = Either[String, Throwable]`
- String failures become user-facing error messages
- Throwable failures are fatal errors

### Payment Processing

PaymentResult sealed trait with Success and multiple Failure types:

- `ValidationError` - Invalid payment data
- `PaymentConnectorError` - Payment processor error
- `TooManyFailedAttempts` - Exceeded retry limit
- `CallerInterruptedWithStar` / `CallerHungUp` - User abandonment

### State Management

CallStateServer maintains call state in `Ref[Map[String, CallState]]` keyed by call ID, with automatic cleanup when
calls end.

### Type Safety

- `ToText[A]` typeclass for safe text conversion
- Sealed trait hierarchies prevent invalid state transitions
- ZIO effects provide composable error handling

## Development Notes

- Use ZIO 3.x effects throughout
- All text output should go through `ToText` typeclass
- XML tag generation requires `TypedTag` constructor for non-HTML elements
- Cross-platform compatibility between Twilio and Telnyx where possible
- Payment features are Twilio-specific only
