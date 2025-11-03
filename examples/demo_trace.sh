#!/usr/bin/env bash
set -euo pipefail
echo "=== Traza SOS de ejemplos clave ==="

for f in examples/04_let_sum.lsp examples/05_letstar.lsp examples/06_lambda_app.lsp; do
  echo
  echo ">>> $f"
  sed 's/^;.*$//' "$f" | stack run -- trace
done

echo
echo "Listo."
