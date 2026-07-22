# mjr-meta-eval

The Emacs function `mjr-meta-eval' provides an easy way to evaluate an
expression in a buffer and place the results on the kill ring.

The expression may be evaluated in several different ways:

 - :int ..... Uses `mjr-meta-eval-multibase-convert'
 - :calc .... like calling `quick-calc' (C-c * q)
 - :elisp ... like calling `eval-expression' (M-:)
 - :lisp .... Uses common lisp via SLIME
 - :maxima .. Uses maxima 
 - :octave .. Uses octave 

If the string to be evaluated looks like a single integer, then :int
evaluation is always used.  This is a super handy way to convert a
number in source code to various bases when programming.

The expression can be identified by highlighting it in the buffer.
Alternately `mjr-meta-eval' will look around the point for something to
evaluate.  In lisp and lisp adjacent modes, look for a sexp at the
point.  In non-lisp modes, look a string not containing spaces.
Whenever `mjr-meta-eval' makes a guess about what to evaluate, it
always gives the user the option to edit the expression before
evaluation.
