# aurora-emacs
A modern and elegant emacs configuration

Add this to your `init.el` and replace `aurora-path` with the path to this repository on your computer.

```
;; Path to Aurora emacs modules (mandatory)
(defvar aurora-path "/path/to/aurora-emacs")
(add-to-list 'load-path aurora-path)
(add-to-list 'load-path ".")
(require 'aurora)
```
