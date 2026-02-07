# resnippets.el

**resnippets.el** is an Emacs package that defines automatically expandable snippets using regex. Unlike traditional snippet engines that rely on fixed keywords or word boundaries, resnippets.el matches the text immediately before the cursor, enabling context-aware, suffix-based expansions.

## Features

- **Regex Triggers**: Define triggers using regular expressions that match the text before the cursor.
- **Group Capture**: Reuse captured groups from your regex in the expansion.
- **Conditional Expansion**: Restrict snippets to specific major modes or arbitrary predicates (e.g., only within LaTeX fragments).
- **Cursor Placement**: Precisely control the cursor position after expansion.

## Installation

Add `resnippets.el` to your load path and require it.

```elisp
(add-to-list 'load-path "/path/to/resnippets")
(require 'resnippets)
(resnippets-global-mode 1)
```

## Usage

### Basic Snippets

Use `resnippets-add` to register a snippet. The first argument is the regex, and the second is the expansion list.

```elisp
;; Simple substitution: typing "vec" expands to "\\vec{}"
(resnippets-add "\\([a-zA-Z\\]+\\)vec" '("\\vec{" 1 "}"))

;; Regex capture: "ahat" -> "\\hat{a}"
(resnippets-add "\\([a-zA-Z\\]+\\)hat" '("\\hat{" 1 "}"))

;; Another example: "fdot" -> "\\dot{f}"
(resnippets-add "\\([a-zA-Z\\]+\\)dot" '("\\dot{" 1 "}"))
```

### Cursor Placement

Use `(resnippets-cursor)` to specify the final cursor position.

```elisp
;; "bbar" -> "\\bar{b|}"
(resnippets-add "\\([a-zA-Z\\]+\\)bar" '("\\bar{" 1 (resnippets-cursor) "}"))
```

### Conditional Snippets

You can restrict snippets using `:mode` or `:condition`.

```elisp
;; Expansion only in LaTeX-mode and org-mode within LaTeX fragments
(resnippets-add "\\([a-zA-Z\\]+\\)til" '("\\tilde{" 1 "}")
                :mode '(LaTeX-mode org-mode)
                :condition '(or (texmathp) (org-inside-LaTeX-fragment-p)))
```

### Priority

When multiple snippets match, use `:priority` to control which one wins (default: 0).

```elisp
;; Both match "foohat", but regex has higher priority
(resnippets-add "foohat" "literal" :priority 5)
(resnippets-add "\\([a-z]+\\)hat" '("\\hat{" 1 "}") :priority 10)
;; "foohat" → "\hat{foo}" (priority 10 wins)
```

### Word Boundary

Use `:word-boundary t` to match only at word boundaries (avoids matching mid-word).

```elisp
(resnippets-add "int" "\\int" :word-boundary t)
;; "int" → "\int" ✓
;; "print" → no match (int is mid-word) ✗
```

### Chained Expansions

Use `:chain t` to trigger further snippet matches after expansion.

```elisp
(resnippets-add "int" "\\int " :chain t)
(resnippets-add "\\\\int " "INTEGRAL")
;; "int" → "\int " → "INTEGRAL" (chained)
```

### Case-Preserving Substitutions

Use `:match-case t` to make the expansion match the case pattern of the input.

```elisp
;; "rapido" -> "rápido", "Rapido" -> "Rápido", "RAPIDO" -> "RÁPIDO"
(resnippets-add "rapido" "rápido" :match-case t)

;; "qubt/" -> "\\frac{qubt|}{}"
(resnippets-add "\\([a-zA-Z0-9{}_\\^\\\\]+\\)/" '("\\frac{" 1 "}{" (resnippets-cursor) "}"))
```

### Group Definitions

Use `resnippets-define` to define multiple snippets with shared properties.

```elisp
(resnippets-define "math-mode-snippets"
 '(:mode (LaTeX-mode org-mode)
   :condition (or (texmathp) (org-inside-LaTeX-fragment-p)))
 
 ("ahat" "\\hat{a}")
 ("bbar" "\\bar{b}")
 ("cdot" "\\dot{c}")
 ("ftil" "\\tilde{f}")
 )
```

### Management

```elisp
(resnippets-remove "regex-key") ;; Remove a specific snippet
(resnippets-clear)              ;; Remove all snippets

;; Export/Import
(resnippets-export "~/.emacs.d/snippets.el")  ;; Save to file
(resnippets-load "~/.emacs.d/snippets.el")    ;; Load from file
```

## Contribution

Contributions via pull requests are welcome. For significant changes, please open an issue first to discuss what you would like to change.

## License

This project is licensed under the GNU General Public License v2.0. See the `LICENSE` file for more details.
