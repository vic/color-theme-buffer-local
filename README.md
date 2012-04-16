## About

This emacs lisp package lets you set a color-theme on a per-buffer basis.

## Requirements

This package uses [Emacs Face Remapping](http://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html) to installing faces locally on the buffer by means of `buffer-face-set` and `face-remap-set-base` functions.

## Installation

```
M-x package-install color-theme-buffer-local
```

## Usage

Interactively
```
M-x color-theme-buffer-local
```

Programmatically
```lisp
(color-theme-buffer-local 'color-theme-robin-hood (current-buffer))
```

## Issues

http://github.com/vic/color-theme-buffer-local/issues
