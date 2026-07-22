;; mjr-meta-eval --- Provide verGo.sh in Emacs. -*-coding: utf-8 lexical-binding:t; mode:emacs-lisp; fill-column:158 -*-

;; Copyright (c) 2026-2026 Mitch Richling <https://www.mitchr.me>.  All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without
;;    specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Author:      Mitch Richling
;; Version:     0.5
;; Keywords:    mjr-meta-eval
;; URL:         https://github.com/richmit/mjr-meta-eval

;; This file is not part of Emacs

;;; Install:
;; See the README: https://github.com/richmit/mjr-meta-eval/

;;; Commentary:
;; See the README: https://github.com/richmit/mjr-meta-eval/

;;; Code:

(require 'cl-lib)
(require 'maxima nil :noerror)
(require 'thingatpt)
(require 'octave nil :noerror)
(require 'slime-autoloads nil :noerror)
(require 'calc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defgroup mjr-meta-eval nil
  "mjr-meta-eval"
  :group 'convenience
  :group 'development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defcustom mjr-meta-eval-use-ido t
  "Use `ido-completing-read' if non-NIL.  Otherwise use `read-answer'.
`read-answer' provides a faster, but more terse user interface."
  :type 'boolean
  :group 'mjr-meta-eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun mjr-meta-eval-multibase-convert (in-string &optional separator)
  "If IN-STRING's contents are recognized as an unsigned integer, return a string with the integer represented in base 10, 16, 2, and 8.  Otherwise return nil.
The argument SEPARATOR is used to specify a string that will separate the base representations in the return string -- defaults to a single space.
Recognized integers:
  - Unadorned, decimal integers in most programming languages.
  - C/C++
    - Integer suffixes (unsigned * long) are supported
    - Integer prefixes for HEX, BIN, & OCT are also supported
  - BOZ (BIN, OCT, & HEX) integer literals in F77 & F90.
  - LISP/ELISP read macros for (BIN, OCT, DEC, & HEX)
  - Native Emacs CALC syntax.
  - Numeric string escape sequences in C++ and Java."
  (when-let* ((int-pattern     (cl-find-if (lambda (x) (string-match-p (cl-first x) in-string))
                                           ;;          Find Pattern                                                        Rpl Pat  Lingo
                                           (list (list "^[[:space:]]*0[xX]\\([0-9a-fA-F]+\\)[uU]?[lL]?[lL]?[[:space:]]*$"  "16#\\1" "C++"       )
                                                 (list "^[[:space:]]*0[bB]\\([01]+\\)[uU]?[lL]?[lL]?[[:space:]]*$"         "2#\\1"  "C++14"     )
                                                 (list "^[[:space:]]*0\\([0-7]+\\)[uU]?[lL]?[lL]?[[:space:]]*$"            "8#\\1"  "C++"       ) ;; before C++ DEC
                                                 (list "^[[:space:]]*\\([0-9]+\\)[uU]?[lL]?[lL]?[[:space:]]*$"             "10#\\1" "C++"       )
                                                 (list "^[[:space:]]*\\([1-9][0-9]*\\)[[:space:]]*$"                       "10#\\1" "ALL"       )
                                                 (list "^[[:space:]]*\\([0-9]+#[0-9a-fA-F]+\\)[[:space:]]*$"               "\\1"    "calc"      ) ;; Native calc format
                                                 (list "^[[:space:]]*#[xX]\\([0-9a-fA-F]+\\)[[:space:]]*$"                 "16#\\1" "LISP"      )
                                                 (list "^[[:space:]]*#[bB]\\([01]+\\)[[:space:]]*$"                        "2#\\1"  "LISP"      )
                                                 (list "^[[:space:]]*#[oO]\\([0-7]+\\)[[:space:]]*$"                       "8#\\1"  "LISP"      )
                                                 (list "^\\\\\\([0-7]\\{3\\}\\)$"                                          "8#\\1"  "C++"       )
                                                 (list "^\\\\[xX]\\([0-9a-fA-F]\\{2\\}\\)$"                                "16#\\1" "C++"       )
                                                 (list "^\\\\[uU]\\([0-9a-fA-F]\\{4\\}\\)$"                                "16#\\1" "C++/Java"  )
                                                 (list "^\\\\[uU]\\([0-9a-fA-F]\\{8\\}\\)$"                                "16#\\1" "C++"       )
                                                 (list "^[[:space:]]*[bB]'\\([01]+\\)'[[:space:]]*$"                       "2#\\1"  "F77 BOZ"   ) ;; " instead of ' is OK..
                                                 (list "^[[:space:]]*[oO]'\\([0-7]+\\)'[[:space:]]*$"                      "8#\\1"  "F77 BOZ"   )
                                                 (list "^[[:space:]]*[xX]'\\([0-9a-fA-F]+\\)'[[:space:]]*$"                "16#\\1" "F77 BOZ"   )
                                                 (list "^[[:space:]]*[zZ]'\\([0-9a-fA-F]+\\)'[[:space:]]*$"                "16#\\1" "F77 BOZ"   ))))
              (calc-int-string (replace-regexp-in-string (cl-first int-pattern) (cl-second int-pattern)in-string)))
    (mapconcat (lambda (x) (calc-eval (list calc-int-string 'calc-number-radix x))) '(10 16 2 8) (or separator " "))))

;; (let ((tmode "unit")) ;; "unit" "make"
;;   (dolist (test-case '(("0x12"        . "18 16#12 2#10010 8#22")
;;                        ("0x12L"       . "18 16#12 2#10010 8#22")
;;                        ("0x12u"       . "18 16#12 2#10010 8#22")
;;                        ("0x12UL"      . "18 16#12 2#10010 8#22")
;;                        ("0x12ULl"     . "18 16#12 2#10010 8#22")
;;                        ("0b101"       . "5 16#5 2#101 8#5")
;;                        ("0b101L"      . "5 16#5 2#101 8#5")
;;                        ("012"         . "10 16#A 2#1010 8#12")
;;                        ("012L"        . "10 16#A 2#1010 8#12")
;;                        ("12"          . "12 16#C 2#1100 8#14")
;;                        ("#x12"        . "18 16#12 2#10010 8#22")
;;                        ("#b101"       . "5 16#5 2#101 8#5")
;;                        ("#B101"       . "5 16#5 2#101 8#5")
;;                        ("#o12"        . "10 16#A 2#1010 8#12")
;;                        ("\\u00012345" . "74565 16#12345 2#10010001101000101 8#221505")
;;                        ("\\u1234"     . "4660 16#1234 2#1001000110100 8#11064")
;;                        ("\\U1234"     . "4660 16#1234 2#1001000110100 8#11064")
;;                        ("\\x12"       . "18 16#12 2#10010 8#22")
;;                        ("\\123"       . "83 16#53 2#1010011 8#123")
;;                        ("z'12'"       . "18 16#12 2#10010 8#22")
;;                        ("b'101'"      . "5 16#5 2#101 8#5")
;;                        ("o'12'"       . "10 16#A 2#1010 8#12")))
;;     (cl-destructuring-bind (test-string . test-result) test-case
;;       (let ((ret-result (mjr-meta-eval-multibase-convert test-string)))
;;         (if (string-equal tmode "unit")
;;             (if (not (string-equal ret-result test-result))
;;                 (print (format "ERROR: %S => %S Expected: %S" test-string ret-result test-result)))
;;             (princ (format "(%S . %S)\n" test-string ret-result)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun mjr-meta-eval (eval-str &optional eval-how)
  "Evaluate the region or expression near the point using maxima, calc, elisp, or lisp (via slime), put the result in the kill ring, and print it in a message.
Arguments:
 - EVAL-STR: A string to evaluate.
   In interactive mode:
    - If region is active, then it is used.
    - If the region is inactive, then look for an expression near the point.  
      - In lisp and lisp adjacent modes, look for a sexp at the point (point should be on the opening paren or just after the closing paren).
      - In non-lisp modes, look a string not containing spaces.
      - If whatever is found doesn't look like a single integer, then the user is given the opportunity to edit it before evaluation.
 - EVAL-HOW: How to evaluate EVAL-STR.  Valid values are the following 6 keyword symbols:
     - :int ..... Uses `mjr-meta-eval-multibase-convert'
     - :calc .... like calling `quick-calc' (C-c * q)
     - :elisp ... like calling `eval-expression' (M-:)
     - :lisp .... like calling slime-interactive-eval (C-:) in a slime buffer  (Available if SLIME has an active connection)
                  Note a SLIME session is *not* started automatically.
     - :maxima .. Uses maxima for evaluation (C-c C-r) in a maxima source file (Available if the maxima package is available)
                  If maxima is not already running, then a maxima session will be started.
                  Note: Sets the maxima session display2d preference to false.
     - :octave .. Uses octave for evaluation limits evaluation time to 1s      (Available if Emacs is running octave)
                  An octave session is *not* started automatically.  To start an octave session, use `run-octave'.
                  Note: Sets the octave session output format setting to compact.
   If EVAL-STR contains a single integer, as detected by `mjr-meta-eval-multibase-convert', then evaluation method is set to :int regardless of the
   value of EVAL-HOW -- in interactive mode the user is not prompted to provide a value for EVAL-HOW in this case.
   If EVAL-HOW is missing, possible in non-interactive mode, then it is set to 'calc if it is not being ignored because EVAL-STR contains a single integer.
   In interactive mode unavailable methods will not be listed."
  (interactive (let ((eval-str (or (and transient-mark-mode
                                        (region-active-p) 
                                        (mark)  
                                        (buffer-substring-no-properties (region-beginning) (region-end)))
                                   (read-string "Expression to evaluate: " (if (string-match "\\(slime\\|lisp\\)" (symbol-name major-mode))
                                                                               (thing-at-point 'sexp)
                                                                               (and (thing-at-point-looking-at "\\([^[:space:]]+\\)" 30) 
                                                                                    (match-string 0)))
                                                ""))))
                 (if (mjr-meta-eval-multibase-convert eval-str)
                     (list eval-str :int)
                     (list eval-str
                           (let ((ev-meth (append '(("calc"   ?c "Evaluate with GNU Calc")
                                                    ("elisp"  ?e "Evaluate as Emacs lisp code"))
                                                  (when (and (functionp 'slime-eval-save) (functionp 'slime-interactive-eval))
                                                    (list '("lisp"   ?l "Evaluate as Common lisp code via SLIME")))
                                                  (when (and (functionp 'maxima-single-string-wait) (functionp 'maxima-last-output-noprompt))
                                                    (list '("maxima" ?m "Evaluate in maxima session")))
                                                  (when (and (boundp 'inferior-octave-process) (process-live-p inferior-octave-process))
                                                    (list '("octave" ?o "Evaluate in octave session"))))))
                             (intern (concat ":" (if (and mjr-meta-eval-use-ido (require 'ido nil :noerror))
                                                     (ido-completing-read "Eval how: " (mapcar #'car ev-meth))
                                                     (let ((read-answer-short t))
                                                       (read-answer "Eval how: " ev-meth))))))))))
  (let* ((eval-how (or eval-how 
                       (when (mjr-meta-eval-multibase-convert eval-str) :int)
                       :calc))
         (eval-str (format "%s" eval-str))
         (eval-res (pcase eval-how
                     (:int    (mjr-meta-eval-multibase-convert eval-str))
                     (:calc   (calc-eval eval-str))
                     (:elisp  (format "%s" (eval (car (read-from-string eval-str)))))
                     (:maxima (progn (maxima-single-string-wait (concat "display2d:false$ " eval-str))
                                      (string-clean-whitespace (maxima-last-output-noprompt))))
                     (:lisp   (cl-second (slime-eval `(swank:eval-and-grab-output ,eval-str))))
                     (:octave (let ((tmp-buf (generate-new-buffer "*temp*" 't)))                                 
                                 (with-current-buffer inferior-octave-buffer
                                   (comint-redirect-send-command (concat "format compact; " eval-str) tmp-buf nil t)
                                   (sleep-for 1)
                                   (let ((res (with-current-buffer tmp-buf
                                                (buffer-string))))
                                     (kill-buffer tmp-buf)
                                     res))))
                     (_        (error "mjr-meta-eval: Unknown value for eval-how: %s" eval-how)))))
    (if (not (stringp eval-res))
        (error "mjr-meta-eval: Something went wrong during evaluation!"))
    (let ((prettyo  (string-trim-right eval-res)))
      (message "mjr-meta-eval: %s => %s%s" eval-str (if (string-match "[\n\r]" prettyo) "\n" "")  prettyo)
      (kill-new prettyo)
      prettyo)))

;; (MJR-meta-eval "inv([1,2,3;4,5,6;7,8,10])" :octave)
;; "ans =
;;   -0.6667  -1.3333   1.0000
;;   -0.6667   3.6667  -2.0000
;;    1.0000  -2.0000   1.0000"
;; 
;; (MJR-meta-eval "inv([1,2,3;4,5,6;7,8,10])" :calc)
;; "[[-0.666666666667, -1.33333333333, 1], [-0.666666666667, 3.66666666667, -2], [1, -2, 1]]"
;; 
;; (MJR-meta-eval "inv([[1,2,3],[4,5,6],[7,8,10]])" :calc)
;; "[[-0.666666666667, -1.33333333333, 1], [-0.666666666667, 3.66666666667, -2], [1, -2, 1]]"
;; 
;; (MJR-meta-eval "invert(matrix([1,2,3],[4,5,6],[7,8,10]))" :maxima)
;; "matrix([-2/3,-4/3,1],[-2/3,11/3,-2],[1,-2,1])"
;; 
;; (MJR-meta-eval "expand((x+1)^10)" :maxima)
;; "x^10+10*x^9+45*x^8+120*x^7+210*x^6+252*x^5+210*x^4+120*x^3+45*x^2+10*x+1"
;; 
;; (MJR-meta-eval "expand((x+1)^10)" :calc)
;; "x^10 + 10 x^9 + 45 x^8 + 120 x^7 + 210 x^6 + 252 x^5 + 210 x^4 + 120 x^3 + 45 x^2 + 10 x + 1"
;; 
;; (MJR-meta-eval "(+ 1 2)" :elisp)
;; "3"
;; 
;; (MJR-meta-eval "(+ 1 2)" :lisp)
;; "3"
;; 
;; (MJR-meta-eval "1+2" :calc)
;; "3"
;; 
;; (MJR-meta-eval "float(sin(1))" :maxima)
;; "0.8414709848078965"
;; 
;; (MJR-meta-eval "sin(1)" :octave)
;; "ans = 0.8415"
;; 
;; (MJR-meta-eval "sin(1)" :calc)
;; "0.0174524064373"
;; 
;; (MJR-meta-eval "(sin 1)" :elisp)
;; "0.8414709848078965"

(provide 'mjr-meta-eval)

;;; filename ends here
