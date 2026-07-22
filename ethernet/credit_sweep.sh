#!/usr/bin/env bash
set -euo pipefail

MIN_CREDIT=1
MAX_CREDIT=8
SLEEP_SECONDS=3
OUTPUT="credit_sweep_results.csv"
SCRIPT="udp_tsi_host.py"
ELF="../sw/hello.riscv"

usage() {
    cat <<EOF
Usage: $0 [--min-credit N] [--max-credit N] [--sleep-seconds S] [--output FILE] [--script PATH] [--elf PATH]

Runs:
  python udp_tsi_host.py --auto --credited N run ../sw/hello.riscv

for N from min-credit to max-credit inclusive, sleeps between runs, saves CSV,
and prints a timing summary.
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --min-credit)
            MIN_CREDIT="$2"
            shift 2
            ;;
        --max-credit)
            MAX_CREDIT="$2"
            shift 2
            ;;
        --sleep-seconds)
            SLEEP_SECONDS="$2"
            shift 2
            ;;
        --output)
            OUTPUT="$2"
            shift 2
            ;;
        --script)
            SCRIPT="$2"
            shift 2
            ;;
        --elf)
            ELF="$2"
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown argument: $1" >&2
            usage >&2
            exit 1
            ;;
    esac
done

if (( MIN_CREDIT < 1 )); then
    echo "ERROR: --min-credit must be >= 1" >&2
    exit 1
fi

if (( MAX_CREDIT < MIN_CREDIT )); then
    echo "ERROR: --max-credit must be >= --min-credit" >&2
    exit 1
fi

printf "credit,elapsed_seconds,returncode\n" > "$OUTPUT"

declare -a SUMMARY_LINES=()

for (( credit=MIN_CREDIT; credit<=MAX_CREDIT; credit++ )); do
    cmd=(python "$SCRIPT" --auto --credited "$credit" run "$ELF")
    echo
    echo "Running credit=$credit: ${cmd[*]}"

    start_ts="$(python -c 'import time; print(time.monotonic())')"
    set +e
    "${cmd[@]}"
    rc=$?
    set -e
    end_ts="$(python -c 'import time; print(time.monotonic())')"
    elapsed="$(python - "$start_ts" "$end_ts" <<'PY'
import sys
start = float(sys.argv[1])
end = float(sys.argv[2])
print(f"{end - start:.3f}")
PY
)"

    printf "%s,%s,%s\n" "$credit" "$elapsed" "$rc" >> "$OUTPUT"
    SUMMARY_LINES+=("credits=$credit  time=${elapsed}s  rc=$rc")
    echo "credit=$credit elapsed=${elapsed}s rc=$rc"

    if (( credit < MAX_CREDIT )); then
        sleep "$SLEEP_SECONDS"
    fi
done

echo
echo "Summary:"
for line in "${SUMMARY_LINES[@]}"; do
    echo "  $line"
done

echo
echo "Saved results to $OUTPUT"
