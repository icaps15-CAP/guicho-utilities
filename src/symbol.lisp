
(in-package :guicho-utilities)
(use-syntax :annot)

@export
@doc "concatenate the name of the symbol with `-' delimiter and
intern that string in the current package."
(defun concatenate-symbols (sym &rest syms)
  (%concatenate-symbols syms (symbol-name sym)))

(defun %concatenate-symbols (syms str)
  (if syms
      (%concatenate-symbols
       (cdr syms)
       (concatenate 'string str "-" (symbol-name (car syms))))
      (intern str)))

@export
(defun keyword-symbol (key)
  (intern (symbol-name key)))
