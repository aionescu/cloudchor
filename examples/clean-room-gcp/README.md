# Data Clean Room on Google Cloud Pub/Sub

This example runs the CloudChor data clean room protocol over
[Google Cloud Pub/Sub](https://cloud.google.com/pubsub) instead of direct
HTTP connections.  Each choreographic participant communicates through a
managed GCP message queue, so all processes can run on separate machines
anywhere in the world without open ports or shared network access.

The choreography itself is identical to `examples/clean-room`.  Only the
backend (`PubSubConfig` instead of `HttpConfig`) and the `main` function
change.

---

## Prerequisites

| Tool | Install |
|------|---------|
| GHC ≥ 9.6 + Cabal ≥ 3.8 | [GHCup](https://www.haskell.org/ghcup/) |
| Google Cloud SDK (`gcloud`) | [cloud.google.com/sdk](https://cloud.google.com/sdk/docs/install) |
| A Google account | [accounts.google.com](https://accounts.google.com) |

---

## Step 1 — Create a GCP account and project

1. Go to <https://console.cloud.google.com> and sign in with your Google
   account.
2. Click **Select a project → New project**.
3. Give it a name (e.g. `cloudchor-demo`) and note the **Project ID**
   (shown below the name — it may differ from the name).
4. Click **Create**.

> **Cost**: GCP gives new accounts $300 free credit for 90 days.  Pub/Sub
> also has a permanent free tier of 10 GB/month, which is far more than a
> research demo will use.

---

## Step 2 — Authenticate the `gcloud` CLI

```bash
gcloud auth login
gcloud config set project <YOUR_PROJECT_ID>
```

---

## Step 3 — Enable the Pub/Sub API

```bash
gcloud services enable pubsub.googleapis.com
```

---

## Step 4 — Create topics and subscriptions

Each choreographic location needs one **inbox topic** and one **ordered
pull subscription** attached to it.  Run the following for every participant
in your protocol (adjust the list to match your run):

```bash
for LOC in server client1 client2; do
  gcloud pubsub topics create "$LOC"

  gcloud pubsub subscriptions create "$LOC" \
    --topic "$LOC" \
    --enable-message-ordering
done
```

> **Important**: `--enable-message-ordering` is required.  The backend uses
> the sender's location name as an ordering key to guarantee per-sender FIFO
> delivery, which is a correctness requirement for choreographic execution.

To clean up afterwards:

```bash
for LOC in server client1 client2; do
  gcloud pubsub subscriptions delete "$LOC"
  gcloud pubsub topics delete "$LOC"
done
```

---

## Step 5 — Set up Application Default Credentials

**Locally** (laptop / development machine):

```bash
gcloud auth application-default login
```

This writes credentials to
`~/.config/gcloud/application_default_credentials.json`.  The `newEnv` call
in `Main.hs` picks them up automatically.

**On GCP** (Cloud Run / GCE / GKE):  attach a service account with the
`roles/pubsub.editor` role to the VM or container.  No credential file is
needed — `newEnv` uses the instance metadata server automatically.

---

## Step 6 — Build

From the repository root:

```bash
cabal build clean-room-gcp
```

---

## Step 7 — Run

Each participant is a separate process.  They can run on the same machine
(different terminals) or on completely different machines — as long as they
all share the same GCP project and have valid credentials.

**Argument format:**

```
clean-room-gcp <self> <gcp-project-id> <server> <client1> [<client2> …]
```

**Example with two clients (three terminals):**

```bash
# Terminal 1 — server
cabal run clean-room-gcp -- server cloudchor-demo server client1 client2

# Terminal 2 — client1
cabal run clean-room-gcp -- client1 cloudchor-demo server client1 client2

# Terminal 3 — client2
cabal run clean-room-gcp -- client2 cloudchor-demo server client1 client2
```

All three processes will coordinate through GCP Pub/Sub and print the
agreed query results to stdout.

---

## How it works

```
┌──────────┐   publish to topic "server"    ┌──────────┐
│ client1  │ ──────────────────────────────▶│  server  │
│          │ ◀────────────────────────────── │          │
└──────────┘   publish to topic "client1"   └──────────┘
                                                  ▲  │
                                                  │  │ publish to
                                     pull from    │  ▼ topic "client2"
                                  subscription    │
                                   "client2"  ┌──────────┐
                                              │ client2  │
                                              └──────────┘
```

- Every location has one **topic** (its inbox) and one **subscription**
  (its reader).
- `Send a l` publishes to `l`'s topic, tagging the message with the
  sender's name as both a message attribute and an ordering key.
- A background thread long-polls each location's subscription and routes
  incoming messages to per-sender in-memory channels.
- `Recv l` reads from the in-memory channel for sender `l`, preserving
  FIFO order per sender.

---

## Differences from the HTTP version

| | `clean-room` (HTTP) | `clean-room-gcp` (Pub/Sub) |
|---|---|---|
| Backend | `HttpConfig` | `PubSubConfig` |
| Transport | Direct HTTP between participants | GCP Pub/Sub (managed queuing) |
| Addressing | `localhost:3000`, `localhost:3001`, … | GCP topic/subscription names |
| Extra arg | — | `<gcp-project-id>` |
| Requires open ports | Yes | No |
| Multi-machine | Manual networking | Built-in |
| Choreography code | unchanged | unchanged |
