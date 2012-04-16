## About

This emacs lisp package lets you set a color-theme on a per-buffer basis.

## Requirements

This package uses [Emacs Face Remapping](http://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html) to installing faces locally on the buffer by means of `buffer-face-set` and `face-remap-set-base` functions.

## Installation

For color-theme.el themes
```
M-x package-install color-theme-buffer-local
```

For emacs24 themes
```
M-x package-install load-theme-buffer-local
```

## Using themes made for color-theme.el

Interactively
```
M-x color-theme-buffer-local
```

Programmatically
```
(color-theme-buffer-local 'color-theme-robin-hood (current-buffer))
```

## Using themes made for emacs24 themes.

Interactively
```
M-x load-theme-buffer-local
```

Programmatically
```
(load-theme-buffer-local 'misterioso (current-buffer))
```

## Issues

http://github.com/vic/color-theme-buffer-local/issues
