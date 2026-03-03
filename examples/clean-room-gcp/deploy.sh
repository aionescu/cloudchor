#!/usr/bin/env bash
# deploy.sh — Build and run the CloudChor data clean room on Google Cloud Pub/Sub.
#
# Two modes:
#   --local  (default) All participants run on this machine; messages flow
#                      through GCP Pub/Sub.  Good for development and testing.
#   --cloud            Each participant runs on a dedicated GCE VM.
#                      Requires building on x86-64 Linux (same ABI as GCE Debian).
#
# Usage:
#   export GCP_PROJECT=my-project-id
#   ./deploy.sh [--local|--cloud] [--clients "c1 c2 c3"] [--teardown]
#
# Prerequisites:
#   - gcloud CLI authenticated  (gcloud auth login)
#   - cabal + GHC installed
#   - GCP_PROJECT env var set, or pass --project <id>

set -euo pipefail

# ── Colours ──────────────────────────────────────────────────────────────────
RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'
CYAN='\033[0;36m'; BOLD='\033[1m'; NC='\033[0m'

info()    { echo -e "${CYAN}[info]${NC}  $*"; }
ok()      { echo -e "${GREEN}[ok]${NC}    $*"; }
warn()    { echo -e "${YELLOW}[warn]${NC}  $*"; }
die()     { echo -e "${RED}[error]${NC} $*" >&2; exit 1; }
section() { echo -e "\n${BOLD}▶ $*${NC}"; }

# ── Defaults ─────────────────────────────────────────────────────────────────
PROJECT="${GCP_PROJECT:-}"
REGION="${GCP_REGION:-us-central1}"
ZONE="${GCP_ZONE:-us-central1-a}"
SERVER="server"
CLIENTS_STR="${CLIENTS:-client1 client2}"
MODE="local"
TEARDOWN=false
MACHINE_TYPE="e2-micro"          # free-tier eligible
SA_NAME="cloudchor-sa"
VM_IMAGE="debian-cloud/debian-12"

# ── Argument parsing ──────────────────────────────────────────────────────────
while [[ $# -gt 0 ]]; do
  case $1 in
    --local)    MODE="local";            shift ;;
    --cloud)    MODE="cloud";            shift ;;
    --teardown) TEARDOWN=true;           shift ;;
    --project)  PROJECT="$2";            shift 2 ;;
    --clients)  CLIENTS_STR="$2";        shift 2 ;;
    --server)   SERVER="$2";             shift 2 ;;
    --zone)     ZONE="$2";               shift 2 ;;
    --region)   REGION="$2";             shift 2 ;;
    *) die "Unknown argument: $1. Use --local, --cloud, --teardown, --project, --clients." ;;
  esac
done

read -ra CLIENTS <<< "$CLIENTS_STR"
ALL_LOCS=("$SERVER" "${CLIENTS[@]}")
ALL_LOCS_STR="${ALL_LOCS[*]}"
SA_EMAIL="$SA_NAME@${PROJECT}.iam.gserviceaccount.com"

[[ -z "$PROJECT" ]] && die "GCP project ID is required. Set GCP_PROJECT or pass --project <id>."

echo -e "${BOLD}CloudChor clean-room deploy${NC}"
echo "  project   : $PROJECT"
echo "  mode      : $MODE"
echo "  server    : $SERVER"
echo "  clients   : ${CLIENTS[*]}"
echo "  zone      : $ZONE  (cloud mode only)"
echo ""

# ── Teardown ─────────────────────────────────────────────────────────────────
teardown() {
  section "Teardown"
  for loc in "${ALL_LOCS[@]}"; do
    info "Deleting subscription $loc"
    gcloud pubsub subscriptions delete "$loc" --project "$PROJECT" --quiet 2>/dev/null || true
    info "Deleting topic $loc"
    gcloud pubsub topics delete "$loc" --project "$PROJECT" --quiet 2>/dev/null || true
  done
  if [[ "$MODE" == "cloud" ]]; then
    for loc in "${ALL_LOCS[@]}"; do
      info "Deleting VM $loc"
      gcloud compute instances delete "$loc" \
        --project "$PROJECT" --zone "$ZONE" --quiet 2>/dev/null || true
    done
  fi
  ok "Teardown complete."
}

if $TEARDOWN; then teardown; exit 0; fi

# ── Step 1: Enable APIs ───────────────────────────────────────────────────────
section "Enable GCP APIs"
APIS="pubsub.googleapis.com"
[[ "$MODE" == "cloud" ]] && APIS="$APIS compute.googleapis.com iam.googleapis.com"
gcloud services enable $APIS --project "$PROJECT"
ok "APIs enabled."

# ── Step 2: Create topics and subscriptions ───────────────────────────────────
section "Create Pub/Sub topics and subscriptions"
for loc in "${ALL_LOCS[@]}"; do
  if gcloud pubsub topics describe "$loc" --project "$PROJECT" &>/dev/null; then
    warn "Topic '$loc' already exists, skipping."
  else
    gcloud pubsub topics create "$loc" --project "$PROJECT"
    ok "Created topic '$loc'."
  fi

  if gcloud pubsub subscriptions describe "$loc" --project "$PROJECT" &>/dev/null; then
    warn "Subscription '$loc' already exists, skipping."
  else
    gcloud pubsub subscriptions create "$loc" \
      --topic "$loc" \
      --enable-message-ordering \
      --project "$PROJECT"
    ok "Created subscription '$loc' (ordered)."
  fi
done

# ── Step 3: Build binary ──────────────────────────────────────────────────────
section "Build clean-room-gcp"
(cd "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)" && cabal build clean-room-gcp)
BINARY=$(cabal list-bin clean-room-gcp)
ok "Binary: $BINARY"

# ── Step 4: Auth (local mode only) ───────────────────────────────────────────
if [[ "$MODE" == "local" ]]; then
  section "Application Default Credentials"
  if [[ -f "$HOME/.config/gcloud/application_default_credentials.json" ]]; then
    ok "ADC file found."
  else
    warn "ADC file not found. Running: gcloud auth application-default login"
    gcloud auth application-default login
  fi
fi

# ── Step 5 (cloud): Service account + VMs ────────────────────────────────────
if [[ "$MODE" == "cloud" ]]; then
  section "Service account"
  if gcloud iam service-accounts describe "$SA_EMAIL" --project "$PROJECT" &>/dev/null; then
    warn "Service account $SA_EMAIL already exists."
  else
    gcloud iam service-accounts create "$SA_NAME" \
      --display-name "CloudChor demo" \
      --project "$PROJECT"
    ok "Created service account $SA_EMAIL."
  fi
  gcloud projects add-iam-policy-binding "$PROJECT" \
    --member "serviceAccount:$SA_EMAIL" \
    --role "roles/pubsub.editor" --quiet
  ok "Granted roles/pubsub.editor."

  section "Create GCE VMs (one per participant)"
  for loc in "${ALL_LOCS[@]}"; do
    if gcloud compute instances describe "$loc" --project "$PROJECT" --zone "$ZONE" &>/dev/null; then
      warn "VM '$loc' already exists, skipping."
    else
      gcloud compute instances create "$loc" \
        --project "$PROJECT" \
        --zone "$ZONE" \
        --machine-type "$MACHINE_TYPE" \
        --image-family "$(basename "$VM_IMAGE")" \
        --image-project "$(dirname "$VM_IMAGE")" \
        --service-account "$SA_EMAIL" \
        --scopes "https://www.googleapis.com/auth/pubsub"
      ok "Created VM '$loc'."
    fi
  done

  section "Wait for VMs to be ready"
  for loc in "${ALL_LOCS[@]}"; do
    info "Waiting for SSH on '$loc'..."
    until gcloud compute ssh "$loc" \
        --project "$PROJECT" --zone "$ZONE" \
        --command "echo ok" --quiet 2>/dev/null; do
      sleep 5
    done
    ok "VM '$loc' is ready."
  done

  section "Upload binary to VMs"
  for loc in "${ALL_LOCS[@]}"; do
    gcloud compute scp "$BINARY" "$loc:/tmp/clean-room-gcp" \
      --project "$PROJECT" --zone "$ZONE" --quiet
    gcloud compute ssh "$loc" \
      --project "$PROJECT" --zone "$ZONE" \
      --command "chmod +x /tmp/clean-room-gcp" --quiet
    ok "Binary uploaded to '$loc'."
  done
fi

# ── Step 6: Run participants ──────────────────────────────────────────────────
section "Run choreography"
PIDS=()
LOG_DIR=$(mktemp -d)
info "Logs: $LOG_DIR"

cleanup_procs() {
  if [[ ${#PIDS[@]} -gt 0 ]]; then
    info "Stopping participants..."
    for pid in "${PIDS[@]}"; do kill "$pid" 2>/dev/null || true; done
  fi
}
trap cleanup_procs EXIT INT TERM

run_participant() {
  local loc="$1"
  local log="$LOG_DIR/$loc.log"

  if [[ "$MODE" == "local" ]]; then
    "$BINARY" "$loc" "$PROJECT" $ALL_LOCS_STR > "$log" 2>&1 &
    PIDS+=($!)
    info "Started $loc (pid $!), log: $log"
  else
    gcloud compute ssh "$loc" \
      --project "$PROJECT" --zone "$ZONE" \
      --command "/tmp/clean-room-gcp $loc $PROJECT $ALL_LOCS_STR > /tmp/out.log 2>&1" \
      --quiet &
    PIDS+=($!)
    info "Started $loc on GCE VM, log: $loc:/tmp/out.log"
  fi
}

for loc in "${ALL_LOCS[@]}"; do
  run_participant "$loc"
done

# ── Step 7: Wait and display output ──────────────────────────────────────────
section "Waiting for all participants to finish"
EXIT_CODE=0
for pid in "${PIDS[@]}"; do
  wait "$pid" || EXIT_CODE=$?
done

section "Results"
if [[ "$MODE" == "local" ]]; then
  for loc in "${ALL_LOCS[@]}"; do
    echo -e "${BOLD}── $loc ──${NC}"
    cat "$LOG_DIR/$loc.log"
    echo ""
  done
else
  for loc in "${ALL_LOCS[@]}"; do
    echo -e "${BOLD}── $loc ──${NC}"
    gcloud compute ssh "$loc" \
      --project "$PROJECT" --zone "$ZONE" \
      --command "cat /tmp/out.log" --quiet 2>/dev/null || true
    echo ""
  done
fi

if [[ $EXIT_CODE -eq 0 ]]; then
  ok "Choreography completed successfully."
else
  warn "One or more participants exited with a non-zero code."
fi

echo ""
info "To delete all GCP resources created by this script:"
echo "  ./deploy.sh --teardown --project $PROJECT ${MODE:+--$MODE}"
