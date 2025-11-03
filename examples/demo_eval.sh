#!/usr/bin/env bash
set -euo pipefail
echo "=== Evaluando ejemplos ==="
for f in examples/*.lsp; do
  echo
  echo ">>> $f"
  sed 's/^;.*$//' "$f" | stack run -- eval || true
done
echo
echo "Listo."
