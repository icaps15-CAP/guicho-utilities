
(in-package :guicho-utilities)
(use-syntax :annot)

@export
@doc "concatenate the name of the symbol with `-' delimiter and
intern that string in the current package.

Difference to alexandria:symbolicate:
  it accepts any printable object.
  numbers, symbols, keywords, strings.
"
(defun concatenate-symbols (sym &rest syms)
  (%concatenate-symbols syms (format nil "~a" sym)))

(defun %concatenate-symbols (syms str)
  (if syms
      (%concatenate-symbols
       (cdr syms)
       (concatenate 'string str "-"
                    (write-to-string (car syms) :escape nil)))
      (intern str)))

@export
(defun keyword-symbol (key)
  "intern a symbol with the same name as that of the given keyword"
  (intern (symbol-name key)))
