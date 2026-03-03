# Cloud Backend Recommendation for CloudChor

## Context

CloudChor's network layer is abstracted behind a `Backend` typeclass in
`src/Choreography/Network.hs`. A new cloud backend only needs to implement
two things:

```haskell
class Backend c where
  locs       :: c -> [LocTm]
  runNetwork :: MonadIO m => c -> LocTm -> Network m a -> m a
```

`runNetwork` handles four operations:

| Operation     | Description                                      |
|---------------|--------------------------------------------------|
| `Run m`       | Local computation — trivial, no networking       |
| `Send a l`    | Deliver a `Show`-serialized string to location `l` |
| `Recv l`      | Return a `Read`-deserializable string from `l`, in order |
| `BCast a ls`  | Already implemented as sequential `Send`s        |

The critical semantic requirement is **per-sender FIFO ordering**: `Recv l`
must return messages from `l` in the order they were sent. The existing HTTP
backend satisfies this by buffering incoming messages in a
`HashMap LocTm (Chan String)` keyed by sender (see `Network/Http.hs`).

---

## Platform Comparison

### AWS SQS FIFO

- One queue per participant (their inbox).
- `Send`: `SendMessage` with `MessageGroupId = sender` (enforces per-sender
  ordering) and a `MessageAttribute` carrying the sender name.
- Background polling thread routes incoming messages to a local
  `HashMap LocTm (Chan String)` — the same structure used in `Http.hs`.
- `Recv l`: reads from `chans ! l`, identical to the HTTP backend.
- Haskell library: `amazonka-sqs` (actively maintained, well-typed).

### Google Cloud Pub/Sub

- One topic per participant.
- `Send`: `Publish` with a `sender` message attribute; ordering keys give
  FIFO per key.
- Pull subscription with a background thread routes to local `Chan`.
- Haskell library: `gogol-pubsub` is poorly maintained; would require direct
  REST calls.

### Azure Service Bus (Sessions)

- One queue per participant; `SessionId = sender`.
- Sessions natively guarantee ordered delivery per sender — the most precise
  semantic match to `Recv l`.
- No official Haskell SDK; requires raw REST calls, adding significant
  implementation overhead.

### Redis Streams (ElastiCache / Azure Cache for Redis)

- `XADD` to recipient's stream with a `sender` field; background thread does
  `XREAD`.
- Simplest possible API; `hedis` is an excellent Haskell library.
- Not cloud-native enough for a CCS real-world deployment claim.

---

## Recommendation: AWS SQS FIFO

AWS SQS FIFO is the best fit for four reasons.

**1. Semantic match.**
`MessageGroupId = sender` provides exactly the per-sender FIFO ordering that
`Recv l` requires, without any local reordering logic.

**2. Implementation mirrors the HTTP backend.**
The new backend has the same structure as `Network/Http.hs`: a background
polling thread fills `HashMap LocTm (Chan String)`, and `Recv l` reads from
`chans ! l`. The structural diff from the HTTP backend is small.

**3. Best Haskell library.**
`amazonka-sqs` covers the full SQS API with well-typed request/response
types and integrates cleanly with `MonadIO`.

**4. Error propagation story.**
SQS exposes delivery failures as structured events: visibility timeouts,
failed acknowledgements, and dead-letter queues. These are exactly the
cloud-level errors that choreographic error propagation needs to handle,
making SQS a natural motivating platform for the follow-up paper.

---

## Sketch of `SqsConfig`

```haskell
data SqsConfig = SqsConfig
  { sqsEnv     :: Amazonka.Env
  , locToQueue :: HashMap LocTm QueueUrl  -- each location's inbox queue URL
  }

mkSqsConfig :: Amazonka.Env -> [(LocTm, QueueUrl)] -> SqsConfig
mkSqsConfig env pairs = SqsConfig env (HashMap.fromList pairs)
```

`runNetworkSqs` would follow the same pattern as `runNetworkHttp`:

1. Allocate `HashMap LocTm (Chan String)` (one channel per peer).
2. Fork a `pollThread` that calls `SQS.receiveMessage` in a loop, reads the
   `sender` attribute, and writes the body to `chans ! sender`.
3. Handle `Send a l` by calling `SQS.sendMessage` to `locToQueue ! l` with
   `MessageGroupId = self` and `MessageBody = show a`.
4. Handle `Recv l` by reading from `chans ! l`.

```haskell
instance Backend SqsConfig where
  locs       = HashMap.keys . locToQueue
  runNetwork = runNetworkSqs
```
