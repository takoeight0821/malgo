# Malgo Language Support for VS Code

Syntax highlighting for the [Malgo](https://github.com/takoeight0821/malgo) programming language.

## Features

- Syntax highlighting for `.mlg` files
- Comment toggling (`--` line comments, `{- -}` block comments)
- Bracket matching and auto-closing
- Code folding for block comments

## Syntax coverage

| Element | Example |
|---------|---------|
| Keywords | `def`, `data`, `foreign`, `import`, `module`, `type`, `class`, `impl`, `let`, `with`, `infix`, `forall`, `exists` |
| Control | `if` |
| Types / constructors | `Int32`, `List`, `Nothing`, `Just` |
| Unboxed types | `Int32#`, `Float#` |
| Comments | `-- line`, `{- block -}` |
| Strings / chars | `"hello"`, `'a'` |
| Numbers | `42`, `1i64`, `3.14`, `2.5f32`, `1#` |
| Operators | `->`, `=>`, `=`, `:`, `|`, `#|`, `|#` |
| Pragmas | `#c-style-apply` |

## Installation

### From source

1. Copy the `editors/vscode/` directory to your VS Code extensions folder:
   - macOS/Linux: `~/.vscode/extensions/malgo-language/`
   - Windows: `%USERPROFILE%\.vscode\extensions\malgo-language\`

2. Restart VS Code.

### Development

Open this directory in VS Code and press `F5` to launch an Extension Development Host with the extension loaded.
