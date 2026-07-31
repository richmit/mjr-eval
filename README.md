# mjr-meta-eval

<!-- SHELLO: ~/core/codeBits/bin/emacs_package_com_to_md.rb mjr-meta-eval.el -->
 See the README: https://github.com/richmit/mjr-meta-eval/

 The Emacs function `mjr-meta-eval` provides an easy way to evaluate an expression in a buffer and place the results on the kill ring.  My primary use case
 is the evaluation of mathematical expressions embedded in source code.

 The expression may be evaluated in several different ways:

   - :int ..... Uses `mjr-meta-eval-multibase-convert`
   - :calc .... like calling `quick-calc` (C-c * q)
   - :elisp ... like calling `eval-expression` (M-:)
   - :lisp .... Uses common lisp via SLIME
   - :maxima .. Uses maxima 
   - :octave .. Uses octave 

 The expression can be identified by highlighting it in the buffer.  Alternately `mjr-meta-eval` will look around the point for something to evaluate.  In
 lisp and lisp adjacent modes, look for a sexp at the point.  In non-lisp modes, look a string not containing spaces.  Whenever `mjr-meta-eval` makes a
 guess about what to evaluate, it always gives the user the option to edit the expression before evaluation.

 If the string to be evaluated looks like a single integer, then :int evaluation is always used.  Integers in many common programming languages can be
 recognized:

   - Unadorned, decimal integers in most programming languages.
   - C/C++
     - Integer suffixes (unsigned * long) are supported
     - Integer prefixes for HEX, BIN, & OCT are also supported
   - BOZ (BIN, OCT, & HEX) integer literals in F77 & F90.
   - LISP/ELISP read macros for (BIN, OCT, DEC, & HEX)
   - Native Emacs CALC syntax.
   - Numeric string escape sequences in C++ and Java."

 I normally bind `mjr-meta-eval` to "ESC ESC :" -- that is hit escape twice and then colon.  I think of `mjr-meta-eval` as an extended version of
 `eval-expression` that is normally bound to "ESC :".

      (keymap-global-set "ESC ESC :"   'mjr-meta-eval)
            
 The easiest way to install mjr-meta-eval is to pull it directly from github:

      (package-vc-install (list 'mjr-meta-eval
                           :url "https://github.com/richmit/mjr-meta-eval"
                           :rev 'newest))
