# MiniLisp LPD

MiniLisp: sintaxis de superficie, desazúcar a un núcleo, evaluación big-step y traza SOS. Lexer con Alex, parser manual.

## Requisitos
- GHC/Stack
- Alex (`stack build --copy-compiler-tool alex`)
- Linux/macOS (probado con GHC 9.10.x)

## Construcción
```bash
stack build
