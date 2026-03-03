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

## Hackage Library Status

Before comparing platforms, it is worth checking what Haskell libraries are
actually available and maintained, since a backend with no usable SDK requires
falling back to raw REST calls.

| Platform | Package(s) | Last upload | Versions | Status |
|---|---|---|---|---|
| Google Cloud Pub/Sub | `gogol` + `gogol-pubsub` | May 2025 | 8 | Excellent |
| AWS SQS | `amazonka` + `amazonka-sqs` | July 2023 | 60+ | Good |
| Redis Streams | `hedis` | April 2023 | 93 | Good |
| Azure Service Bus | `azure-servicebus` | May 2014 | 5 | Abandoned |

Azure is effectively ruled out: the only Haskell package is 11 years old and
wraps a deprecated REST API version. Implementing an Azure backend would
require raw HTTP calls via `wreq` or `req`, adding significant overhead with
no scientific payoff.

---

## Platform Comparison

### Google Cloud Pub/Sub

- One topic per participant (their inbox); other participants publish to it.
- `Send`: `Publish` to the recipient's topic with a `sender` message
  attribute; ordering keys enforce FIFO per sender.
- Pull subscription with a background thread routes incoming messages to a
  local `HashMap LocTm (Chan String)`.
- `Recv l`: reads from `chans ! l`, identical in structure to the HTTP backend.
- Haskell library: `gogol` + `gogol-pubsub`, last updated **May 2025** —
  the best-maintained cloud SDK available for Haskell.

### AWS SQS FIFO

- One FIFO queue per participant (their inbox).
- `Send`: `SendMessage` with `MessageGroupId = sender` (enforces per-sender
  ordering) and a `MessageAttribute` carrying the sender name.
- Background polling thread routes incoming messages to a local
  `HashMap LocTm (Chan String)` — the same structure used in `Http.hs`.
- `Recv l`: reads from `chans ! l`, identical to the HTTP backend.
- Haskell library: `amazonka` + `amazonka-sqs`, last updated **July 2023**.

### Azure Service Bus (Sessions)

- One queue per participant; `SessionId = sender`.
- Sessions natively guarantee ordered delivery per sender — the most precise
  semantic match to `Recv l`.
- No maintained Haskell SDK; would require raw REST calls via `wreq` or `req`.
- **Not recommended** due to the absence of a usable library.

### Redis Streams (ElastiCache / Azure Cache for Redis)

- `XADD` to recipient's stream with a `sender` field; background thread does
  `XREAD`.
- Simplest possible API; `hedis` is an excellent Haskell library with 93
  versions and active maintenance.
- Not cloud-native enough for a CCS real-world deployment claim.

---

## Recommendation: Google Cloud Pub/Sub

Taking library maintenance into account, **Google Cloud Pub/Sub** (`gogol` +
`gogol-pubsub`) is the strongest choice for four reasons.

**1. Best-maintained Haskell library.**
`gogol-pubsub` was updated in May 2025 — more recently than any other cloud
SDK for Haskell. `amazonka-sqs` is a solid second (July 2023) but lags behind.

**2. Semantic match.**
Pub/Sub ordering keys enforce FIFO delivery per key. Setting the ordering key
to the sender name provides exactly the per-sender ordering that `Recv l`
requires.

**3. Implementation mirrors the HTTP backend.**
The new backend has the same structure as `Network/Http.hs`: a background
pull thread fills `HashMap LocTm (Chan String)`, and `Recv l` reads from
`chans ! l`. The structural diff from the HTTP backend is small.

**4. Error propagation story.**
Pub/Sub surfaces delivery failures as structured events: unacknowledged
messages are redelivered, dead-letter topics capture undeliverable messages,
and subscription push/pull errors are typed. These are exactly the cloud-level
errors that choreographic error propagation needs to handle, making Pub/Sub a
natural motivating platform for the follow-up paper.

---

## Sketch of `PubSubConfig`

```haskell
data PubSubConfig = PubSubConfig
  { pubSubEnv   :: Gogol.Env '[]
  , locToTopic  :: HashMap LocTm TopicName  -- each location's inbox topic
  , locToSub    :: HashMap LocTm SubName    -- each location's pull subscription
  }

mkPubSubConfig :: Gogol.Env '[]
               -> [(LocTm, (TopicName, SubName))]
               -> PubSubConfig
mkPubSubConfig env pairs = PubSubConfig env topics subs
  where
    topics = HashMap.fromList [(l, t) | (l, (t, _)) <- pairs]
    subs   = HashMap.fromList [(l, s) | (l, (_, s)) <- pairs]
```

`runNetworkPubSub` follows the same pattern as `runNetworkHttp`:

1. Allocate `HashMap LocTm (Chan String)` (one channel per peer).
2. Fork a `pullThread` that calls `projects.subscriptions.pull` in a loop,
   reads the `sender` message attribute, writes the body to `chans ! sender`,
   and acknowledges the message.
3. Handle `Send a l` by calling `projects.topics.publish` to
   `locToTopic ! l` with ordering key and attribute `sender = self` and
   message data `show a`.
4. Handle `Recv l` by reading from `chans ! l`.

```haskell
instance Backend PubSubConfig where
  locs       = HashMap.keys . locToTopic
  runNetwork = runNetworkPubSub
```

### AWS SQS FIFO as a close second

If Google Cloud is not available, AWS SQS FIFO is a drop-in alternative with
the same structural pattern. Replace `projects.topics.publish` with
`SQS.sendMessage` (setting `MessageGroupId = self`) and the pull loop with
`SQS.receiveMessage` polling. The `amazonka` + `amazonka-sqs` libraries
(July 2023) are well-typed and cover the full API.
