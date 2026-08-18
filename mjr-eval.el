;;; mjr-eval -- evaluate mathematical expressions -*- lexical-binding:t; coding: utf-8; mode:emacs-lisp; fill-column:158 -*-

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
;; Version:     2.12
;; Keywords:    mjr-eval
;; URL:         https://github.com/richmit/mjr-eval

;; This file is not part of Emacs

;;; Commentary:
;;
;; See the README: https://github.com/richmit/mjr-eval/
;;
;; Emacs by itself provides some solid computational support in the form of Emacs Lisp and `calc'.  Emacs also provides fantastic interfaces to many external
;; computational tools (R, Octave, Matlab, Maple, Maxima, Julia, GAP, Macaulay2, etc...).  While many of these packages are quite powerful, they frequently
;; don't provide `quick-calc' functionality -- and when they do it is with wildly different user interfaces.
;;
;; In the worst cases, users are forced to use a flow something like this:
;;  - Highlight some equation in the current buffer
;;  - Switch to a computational mode
;;  - Paste the equation
;;  - Adjust the syntax to match the tool & evaluate the result
;;  - Highlight the result
;;  - Switch back to the original buffer
;;  - Paste the result
;;  
;; This package aims to replace the ghastly practice described above with an efficient and uniform user interface:
;;  - One key binding to access all the computational tools.
;;  - Helps to identify buffer contents for evaluation.
;;  - Provides a fast way for the user to select which computational tool to use (just a single key press)
;;  - Evaluates the expression and both prints the results and places them on the kill ring.
;;
;; This package uses a couple terms:
;;  - ENGINE to mean a computational tools (i.e. octave is an engine as is `calc').  
;;  - HANDLER is a function that takes a string to evaluate and a symbol identifying an ENGINE.
;;
;; This package provides four primary HANDLERs and a single dispatch function that can be used as an interface to them all.  The four HANDLERs are:
;;
;;  - `mjr-eval-org-ticker' -- Use `org-mode' babel for all computations.
;;     - Pro: Keeps a history of all computations preformed during the session
;;     - Con: Requires a working `org-mode' configuration and babel configuration for each language used
;;     - Con: Babel support for some engines may be limited.
;;    
;;  - `mjr-eval-external-session' -- Use an external tool via the author's favorite Emacs mode for that tool
;;    - Pro: Uses interactive modes that are very popular
;;    - Con: Not everybody is going to like the choice of which mode was used to provide support
;;    - Pro: For users already running sessions with these tools, this packge provides an alternate interface for interacting with sessions.
;;    - Con: Somewhat flaky support for :octave & :r
;;    
;;  - `mjr-eval-internal'-- Use internal Emacs facilities
;;    - Pro: Requires zero configuration
;;    - Pro: Lowest latency results
;;    - Con: Fewer & less powerful engines
;;    
;;  - `mjr-eval-external-one'-- Use an external tool by directly calling the binary (TODO: Not yet available)
;;    - Pro: Super simple and very robust
;;    - Con: High latency
;;    - Pro/Con: No session support (which also means no session to crash or freeze)
;;
;; While all of the HANDLERs can be used interactively, it is probably more convenient to access them via the dispatch function `mjr-eval-meta'.  This
;; function has a more sophisticated user interface and allows the user to customize what tools are available.
;;
;; I normally bind `mjr-eval-meta' to "ESC ESC :".  I think of `mjr-eval-meta' as an extended version of `eval-expression' that is normally bound to "ESC :".
;;
;;      (keymap-global-set "ESC ESC :" 'mjr-eval-meta)
;;            
;; The easiest way to install mjr-eval is to pull it directly from github:
;;
;;      (package-vc-install (list 'mjr-eval
;;                           :url "https://github.com/richmit/mjr-eval"
;;                           :rev 'newest))

;;; Code:

(require 'cl-lib)
(require 'thingatpt)
(require 'calc)

;; (require 'maxima nil :noerror)
;; (require 'octave nil :noerror)
;; (require 'slime-autoloads nil :noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr-eval-util-extract-eval-str-from-buffer ()
  "Get a string from the marked text in a buffer or ask the user for a string if nothing is marked."
  (or (and transient-mark-mode
           (region-active-p)
           (mark)
           (buffer-substring-no-properties (region-beginning) (region-end)))
      (read-string "Expression to evaluate: ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr-eval-util-read-engine (list-or-alist-of-engines)
  "Prompt the user for an ENGINE from the given list."
  (intern (downcase (ido-completing-read "Eval how: " 
                                         (if (and (car list-or-alist-of-engines)
                                                  (symbolp (car list-or-alist-of-engines)))
                                             (mapcar #'symbol-name list-or-alist-of-engines)
                                             (mapcar (lambda (x) (symbol-name (car x))) list-or-alist-of-engines))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mjr-eval-util-format-result (from-func engine eval-str result-str)
  "Clean up evaluation results and produce a nice string for a message."
  (let ((msg ""))
    (when from-func
      (setq msg (string-remove-prefix "mjr-eval-" (format "%s" from-func))))
    (when engine
      (setq msg (string-trim (concat msg " " (format "%s" engine)))))
    (setq msg (string-trim (concat msg " eval")))
    (unless (string-match-p "[^\n\r]+[\n\r]+[^\n\r]+" eval-str)
      (setq msg (string-trim (concat msg " of " eval-str))))
    (setq msg (concat msg " => "))
    (when (string-match-p "[^\n\r]+[\n\r]+[^\n\r]+" result-str)
      (setq msg (concat msg "\n")))
    (setq msg (concat msg result-str))
    msg))

;; (progn
;;   (print (mjr-eval-util-format-result 'org-ticker :calc "1+1" "2"))
;;   (print (mjr-eval-util-format-result 'org-ticker :calc "1+1" "2\n3"))
;;   (print (mjr-eval-util-format-result 'org-ticker :calc "1+\n1" "23"))
;;   (print (mjr-eval-util-format-result 'org-ticker nil "1+1" "23"))
;;   (print (mjr-eval-util-format-result nil :calc "1+1" "23"))
;;   (print (mjr-eval-util-format-result nil nil "1+1" "23"))
;;   (print (mjr-eval-util-format-result nil nil "1+\n1" "23"))
;;   (print (mjr-eval-util-format-result nil nil "1+1" "2")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defgroup mjr-eval nil
  "mjr-eval"
  :group 'convenience
  :group 'development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defcustom mjr-eval-org-ticker-engines '((:elisp          . (:language "emacs-lisp" :results "value code"      ))
                                         (:calc           . (:language "calc"       :results "value code"      ))
                                         (:r              . (:language "R"          :results "output verbatum" ))
                                         (:r-session      . (:language "R"          :results "output verbatum" ))
                                         (:octave         . (:language "octave"     :results "output verbatum" ))
                                         (:octave-session . (:language "octave"     :results "output verbatum" ))
                                         (:maxima         . (:language "maxima"     :results "output verbatum" ))
                                         (:ruby           . (:language "ruby"       :results "value code"      ))
                                         (:bash           . (:language "bash"       :results "output verbatum" ))
                                         (:lisp-session   . (:language "lisp"       :results "value"           )) ;; This uses SLIME!
                                         )
  "Engines aviable for `mjr-eval-org-ticker'."
  :type '(alist :key-type symbol :value-type (list (const :language) string (const :results) string))
  :group 'mjr-eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defcustom mjr-eval-org-ticker-buffer-name "*mjr-eval-org-ticker*"
"Name for temporary `org-mode' buffer used by `mjr-eval-org-ticker'."
  :type 'string
  :group 'mjr-eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defcustom mjr-eval-org-ticker-message-strip t
  "If NIL the message printed by `mjr-eval-org-ticker' will include the full results printed by `org-mode' for the babel block.
If non-NIL only the contents of the RESULTS block will be included."
  :type 'boolean
  :group 'mjr-eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defcustom mjr-eval-org-ticker-kill-strip t
  "If NIL the contents of the string added to the ring by `mjr-eval-org-ticker' will include the full results printed by `org-mode' for the babel block.
If non-NIL only the contents of the RESULTS block will be included."
  :type 'boolean
  :group 'mjr-eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun mjr-eval-org-ticker (eval-str engine)
  "Evaluate a EVAL-STR in the given ENGINE with `org-mode'. The result is returned as a string, printed, and placed on the kill ring.

When EVAL-STR is NIL, return a error message string if ENGINE is NOT aviable.

Engine configurations are in `mjr-eval-org-ticker-engines'.
An org buffer, named with the string in `mjr-eval-org-ticker-buffer-name' keeps a history of all calculations."
  (interactive (list (mjr-eval-util-extract-eval-str-from-buffer) 
                     (mjr-eval-util-read-engine mjr-eval-org-ticker-engines)))
  (require 'org)
  (let* ((engine-spec (cdr (assoc engine mjr-eval-org-ticker-engines)))
         (language    (plist-get engine-spec :language))
         (results     (plist-get engine-spec :results))
         (session     (if (string-match-p "-session$" (format "%s" engine)) " :session" "")))
    (if eval-str
        (if-let ((engine-err (mjr-eval-org-ticker nil engine)))
            (error engine-err)
          (with-current-buffer (get-buffer-create mjr-eval-org-ticker-buffer-name)
            (org-mode)
            (goto-char (point-max))
            (insert (format "\n\n#+NAME: oc-%d\n" (point)))
            (insert (format "#+BEGIN_SRC %s%s :results %s :exports both\n" language session results))
            (let ((loc-soc (point)))
              (insert eval-str)
              (insert "\n#+END_SRC\n")
              (let ((loc-eob (point)))
                (goto-char loc-soc)
                (mjr-org-babel-execute-src-block)
                (let* ((raw-res (buffer-substring-no-properties loc-eob (point-max)))
                       (prt-res raw-res))
                  (when mjr-eval-org-ticker-message-strip
                    (mapc (lambda (re) (setq prt-res (replace-regexp-in-string re "" prt-res)))
                          '("^#\\+RESULTS:.*$"
                            "^#\\+end_example[[:space:]]*$"
                            "^#\\+begin_example[[:space:]]*$"
                            "^#\\+end_src.*$"
                            "^#\\+begin_src.*$"
                            "^[[:blank:]]*[\n\r]+"
                            "[[:blank:]\n\r]+\\'")))
                  (message (mjr-eval-util-format-result 'mjr-eval-org-ticker engine eval-str prt-res))
                  (kill-new (if mjr-eval-org-ticker-kill-strip prt-res raw-res))
                  raw-res)))))
        (or (unless (symbolp engine)
              (format "mjr-eval-org-ticker: ENGINE (%s) is must be a symbol!" engine))
            (unless engine-spec
              (format "mjr-eval-org-ticker: ENGINE (%s) missing from mjr-eval-org-ticker-engines!" engine))
            (unless (plistp engine-spec)
              (format "mjr-eval-org-ticker: ENGINE (%s) entry in mjr-eval-org-ticker-engines malformed! Not a plist." engine))
            (unless (featurep 'org)
              (format "mjr-eval-org-ticker: ENGINE (%s) unavailable -- org-mode not loaded!" engine))
            (unless language
              (format "mjr-eval-org-ticker: ENGINE (%s) entry in mjr-eval-org-ticker-engines malformed! :language must be present and non-NIL." engine))
            (unless (stringp results)
              (format "mjr-eval-org-ticker: ENGINE (%s) entry in mjr-eval-org-ticker-engines malformed! :results must be a string." engine))
            (unless (stringp language)
              (format "mjr-eval-org-ticker: ENGINE (%s) entry in mjr-eval-org-ticker-engines malformed! :language must be a string." engine))
            (unless (featurep (intern (concat "ob-" language)))
              (format "mjr-eval-org-ticker: ENGINE (%s) unavailable -- org-mode babel support missing!" engine))))))

;; (progn
;;   (mjr-eval-org-ticker "(* 10 323)" :elisp)
;;   (mjr-eval-org-ticker "inv([1,2,3;4,5,6;7,8,10])" :calc)
;;   (mjr-eval-org-ticker "require 'matrix'; Matrix[[1,2,3],[4,5,6],[7,8,10]].inverse();" :ruby)
;;   (mjr-eval-org-ticker "solve(matrix(c(1,2,3,4,5,6,7,8,10), nrow=3));" :r)
;;   (mjr-eval-org-ticker "inv([1,2,3;4,5,6;7,8,10])" :octave)
;;   (mjr-eval-org-ticker "print(invert(matrix([1,2,3],[4,5,6],[7,8,10])));" :maxima)
;;   (mjr-eval-org-ticker "11 * 323;" :perl)
;;   (mjr-eval-org-ticker "(1+ 4)" "lisp)
;;   (mjr-eval-org-ticker "print(expand((1-x)^10));" :maxima)
;;   nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defcustom mjr-eval-external-session-octave-exec-timeout 10
  "The number of seconds `mjr-eval-external-session' waits for results to appaer.  Values less than 1 result in a 1s timeout."
  :type 'integer
  :group 'mjr-eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defcustom mjr-eval-external-session-octave-write-timeout 1
  "The number of seconds `mjr-eval-external-session' waits for Octave to print the result once printing starts. Values less than 1 result in a 1s timeout."
  :type 'integer
  :group 'mjr-eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun mjr-eval-external-session (eval-str engine)
  "Evaluate a EVAL-STR in the given ENGINE via an inferior process mode.  The result is returned as a string, printed, and placed on the kill ring.

When EVAL-STR is NIL, return a error message string if ENGINE is NOT aviable.

This function directly interacts with the a common interactive mode associated with the ENGINE.

Valid values for ENGINE:
 - :lisp-session .... Use Common LISP via `slime'
                      Very similar to `slime-interactive-eval' (C-:) in a slime buffer
                      Only available if SLIME has an active connection to a common Lisp repl (use `slime' to start one)
 - :maxima-session .. Use maxima via the `maxima-mode' shipped with maxima.
                      Only available if the maxima package is available.
                      If maxima is not already running, then a maxima session will be started automatically.
                      Dubious behavior: 
                       - Sets the maxima session display2d preference to false.
                      Undesired behavior: 
                       - When a new maxima session is started, the results are startup messages.
 - :octave-session .. Use octave via `octave-mode' shipped with octave
                      Only available if Emacs has a running inferior octave process (use `run-octave' to start one)
                      Evaluation time is limited to `mjr-eval-external-session-octave-exec-timeout' seconds.  
                      Once Octave starts I/O for the result it must complete it in `mjr-eval-external-session-octave-write-timeout' seconds.
                      Dubious behavior: 
                       - Sets the octave session output format setting to compact.
                      Undesired behavior: 
                       - The method for waiting on Octave to complete the computation is a bit of a hack.
 - :r-session ....... Use R via `ess-mode'
                      Only available if the ESS package is installed.  
                      A session will be started if one is not already running.  
                      Undesired behavior: 
                       - If multiple R sessions are running, the user is prompted to choose one for each evaluation.
                       - Invalid R syntax can lead to a badly confused ESS process.
                       - If R must be started the R buffer becomes visible."
  (interactive (list (mjr-eval-util-extract-eval-str-from-buffer) 
                     (let ((engine-list (cl-remove-if-not (lambda (x) (mjr-eval-external-session nil x))
                                                          '(:maxima-session :octave-session :r-session :lisp-session))))
                       (unless engine-list
                         (error "mjr-eval-external-session: No engines available!"))
                       (mjr-eval-util-read-engine engine-list))))
  (if eval-str
      (if-let ((engine-err (mjr-eval-external-session nil engine)))
          (error engine-err)
        (let* ((raw-res (pcase engine
                          (:lisp-session   (cl-second (slime-eval `(swank:eval-and-grab-output ,eval-str))))
                          (:maxima-session (progn (maxima-single-string-wait (concat "display2d:false$ " eval-str))
                                                  (string-clean-whitespace (maxima-last-output-noprompt))))
                          (:octave-session (let ((tmp-buf (generate-new-buffer "*temp*" 't)))
                                             (with-current-buffer inferior-octave-buffer
                                               (comint-redirect-send-command (concat "format compact; " eval-str) tmp-buf nil t)
                                               (cl-loop for i from (* mjr-eval-external-session-octave-exec-timeout 10) downto 0
                                                        when (zerop i)
                                                        do (error "mjr-eval-external-session: Evaluation failed")
                                                        until (< 1 (with-current-buffer tmp-buf (point-max)))
                                                        do (message "Waiting for Octave... %d" i)
                                                        do (sleep-for 0.1))
                                               (message "mjr-eval-external-session: Waiting for Octave... Done!")
                                               (sleep-for mjr-eval-external-session-octave-write-timeout)
                                               (prog1 (with-current-buffer tmp-buf
                                                        (buffer-substring-no-properties (point-min) (point-max)))
                                                 (kill-buffer tmp-buf)))))
                          (:r-session      (with-temp-buffer
                                             (setq-local ess-dialect        "R"
                                                         ess-eval-visibly-p nil)
                                             (ess-force-buffer-current)
                                             (ess-command eval-str (current-buffer))
                                             (buffer-substring-no-properties (point-min) (point-max)))))))
          (unless (stringp raw-res)
            (error "mjr-eval-external-session: Something went wrong during evaluation!"))
          (let ((prt-res (string-trim-right raw-res)))
            (message (mjr-eval-util-format-result 'mjr-eval-external-session engine eval-str prt-res))
            (kill-new prt-res)
            raw-res)))
      (pcase engine
        (:lisp-session   (or (unless (require 'slime nil :noerror)
                               "mjr-eval-external-session: ENGINE (:lisp-session) not available! SLIME could not be loaded.")
                             (unless (and (functionp 'slime-eval-save) (functionp 'slime-interactive-eval))
                               "mjr-eval-external-session: ENGINE (:lisp-session) not available! SLIME not loaded.")
                             (unless (slime-current-connection)
                               "mjr-eval-external-session: ENGINE (:lisp-session) not available! No SLIME connection.")))
        (:maxima-session (or (unless (require 'maxima nil :noerror)
                               "mjr-eval-external-session: ENGINE (:maxima-session) not available! maxima could not be loaded.")
                             (unless (and (functionp 'maxima-single-string-wait) (functionp 'maxima-last-output-noprompt))
                               "mjr-eval-external-session: ENGINE (:maxima-session) not available! maxima mode not loaded.")))
        (:r-session      (or (unless (require 'ess nil :noerror)
                               "mjr-eval-external-session: ENGINE (:r-session) not available! ESS could not be loaded.")
                             (unless (and (functionp 'ess-force-buffer-current) (functionp 'ess-command))
                               "mjr-eval-external-session: ENGINE (:r-session) not available! ESS not loaded.")))
        (:octave-session (or (unless (boundp 'inferior-octave-process)
                               "mjr-eval-external-session: ENGINE (:octave-session) not available! octave-mode not loaded.")
                             (unless (process-live-p inferior-octave-process)
                               "mjr-eval-external-session: ENGINE (:octave-session) not available! Inferior octave process not running")))
        (_               (format "mjr-eval-external-session: ENGINE (%s) Unsupported value!" engine)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun mjr-eval-unsigned-integer (eval-str)
  "If EVAL-STR's contents are recognized as an unsigned integer, return a string with the integer represented in various bases.  Otherwise return nil.

The return string: x (s) d h b o c
                   |  |  | | | | |
                   |  |  | | | | +--- x: The original input string
                   |  |  | | | +----- s: The programming language syntax detected
                   |  |  | | +------- d: The integer in decimal
                   |  |  | +--------- h: The integer in hexadecimal
                   |  |  +----------- b: The integer in binary
                   +----------------- c: The character for the integer

The return string is inspired by the printed results `quick-calc' puts in the mini-buffer.

Recognized programming language syntax for integers:
  - C/C++
    - Unadorned decimal integers
    - Unadorned numbers with a leading zero are interpreted as octal
    - Integer suffixes (unsigned & long) are supported
    - Integer prefixes for HEX, BIN, & OCT are also supported
  - Fortran 90 and FORTRAN 77 BOZ (BIN, OCT, & HEX) literals
    - GNU suffix syntax is also supported
    - GNU x prefix/suffix is also supported
  - LISP/ELISP read macros for (BIN, OCT, DEC, & HEX)
  - Emacs CALC syntax
  - C++ style numerical string escape sequences"
  (interactive (list (mjr-eval-util-extract-eval-str-from-buffer)))
  (when-let* ((int-pattern     (cl-find-if (lambda (x) (string-match-p (cl-first x) eval-str))
                                           ;; Find Pattern                                                        Rpl Pat  Lingo
                                           '(("^[[:space:]]*0[xX]\\([0-9a-fA-F]+\\)[uU]?[lL]?[lL]?[[:space:]]*$"  "16#\\1" "C++ Hex"                 )
                                             ("^[[:space:]]*0[bB]\\([01]+\\)[uU]?[lL]?[lL]?[[:space:]]*$"         "2#\\1"  "C++14 Bin"               )
                                             ("^[[:space:]]*0\\([0-7]+\\)[uU]?[lL]?[lL]?[[:space:]]*$"            "8#\\1"  "C++ Oct"                 ) ;; before C++ DEC
                                             ("^[[:space:]]*\\([0-9]+\\)[uU]?[lL]?[lL]?[[:space:]]*$"             "10#\\1" "C++ Dec"                 )
                                             ("^[[:space:]]*\\([0-9]+#[0-9a-fA-F]+\\)[[:space:]]*$"               "\\1"    "Emacs calc"              )
                                             ("^[[:space:]]*#[xX]\\([0-9a-fA-F]+\\)[[:space:]]*$"                 "16#\\1" "LISP Hex"                )
                                             ("^[[:space:]]*#[bB]\\([01]+\\)[[:space:]]*$"                        "2#\\1"  "LISP Bin"                )
                                             ("^[[:space:]]*#[oO]\\([0-7]+\\)[[:space:]]*$"                       "8#\\1"  "LISP Oct"                )
                                             ("^\\\\\\([0-7]\\{3\\}\\)$"                                          "8#\\1"  "C++ Oct Char*1"          )
                                             ("^\\\\[xX]\\([0-9a-fA-F]\\{2\\}\\)$"                                "16#\\1" "C++ Hex Char*1"          )
                                             ("^\\\\[uU]\\([0-9a-fA-F]\\{4\\}\\)$"                                "16#\\1" "C++/Java Unicode*4"      )
                                             ("^\\\\[uU]\\([0-9a-fA-F]\\{8\\}\\)$"                                "16#\\1" "C++ Unicode Char*8"      )
                                             ("^[[:space:]]*[bB]\\(['\"]\\)\\([01]+\\)\\1[[:space:]]*$"           "2#\\2"  "F77 BOZ Bin"             )
                                             ("^[[:space:]]*[oO]\\(['\"]\\)\\([0-7]+\\)\\1[[:space:]]*$"          "8#\\2"  "F77 BOZ Oct"             )
                                             ("^[[:space:]]*[zZ]\\(['\"]\\)\\([0-9a-fA-F]+\\)\\1[[:space:]]*$"    "16#\\2" "F77 BOZ Hex"             )
                                             ("^[[:space:]]*[xX]\\(['\"]\\)\\([0-9a-fA-F]+\\)\\1[[:space:]]*$"    "16#\\2" "GNU Fortran X BOZ Hex"   )
                                             ("^[[:space:]]*\\(['\"]\\)\\([01]+\\)\\1[bB][[:space:]]*$"           "2#\\2"  "GNU Fortran Sfx BOZ Bin" )
                                             ("^[[:space:]]*\\(['\"]\\)\\([0-7]+\\)\\1[oO][[:space:]]*$"          "8#\\2"  "GNU Fortran Sfx BOZ Oct" )
                                             ("^[[:space:]]*\\(['\"]\\)\\([0-9a-fA-F]+\\)\\1[xX][[:space:]]*$"    "16#\\2" "GNU Fortran SfX BOZ Hex" )
                                             ("^[[:space:]]*\\(['\"]\\)\\([0-9a-fA-F]+\\)\\1[zZ][[:space:]]*$"    "16#\\2" "GNU Fortran Sfx BOZ Hex" ))))
              (calc-int-string (replace-regexp-in-string (cl-first int-pattern) (cl-second int-pattern) eval-str)))
    (string-join (append (list eval-str (format "(%s):" (nth 2 int-pattern)))
                         (mapcar (lambda (x) (calc-eval (list calc-int-string 'calc-number-radix x))) '(10 16 2 8))
                         (list (char-to-string (string-to-number (calc-eval (list calc-int-string 'calc-number-radix 10))))))
                 " ")))

;; (let ((tmode "test")) ;; "test" "make"
;;   (dolist (test-case '(("0x14"        . "0x14 (C++ Hex): 20 16#14 2#10100 8#24 ")
;;                        ("0x14L"       . "0x14L (C++ Hex): 20 16#14 2#10100 8#24 ")
;;                        ("0x14u"       . "0x14u (C++ Hex): 20 16#14 2#10100 8#24 ")
;;                        ("0x14UL"      . "0x14UL (C++ Hex): 20 16#14 2#10100 8#24 ")
;;                        ("0x14ULl"     . "0x14ULl (C++ Hex): 20 16#14 2#10100 8#24 ")
;;                        ("0b101"       . "0b101 (C++14 Bin): 5 16#5 2#101 8#5 ")
;;                        ("0b101L"      . "0b101L (C++14 Bin): 5 16#5 2#101 8#5 ")
;;                        ("014"         . "014 (C++ Oct): 12 16#C 2#1100 8#14 ")
;;                        ("014L"        . "014L (C++ Oct): 12 16#C 2#1100 8#14 ")
;;                        ("14"          . "14 (C++ Dec): 14 16#E 2#1110 8#16 ")
;;                        ("#x14"        . "#x14 (LISP Hex): 20 16#14 2#10100 8#24 ")
;;                        ("#b101"       . "#b101 (LISP Bin): 5 16#5 2#101 8#5 ")
;;                        ("#B101"       . "#B101 (LISP Bin): 5 16#5 2#101 8#5 ")
;;                        ("#o14"        . "#o14 (LISP Oct): 12 16#C 2#1100 8#14 ")
;;                        ("\\u00014345" . "\\u00014345 (C++ Unicode Char*8): 82757 16#14345 2#10100001101000101 8#241505 𔍅")
;;                        ("\\u1434"     . "\\u1434 (C++/Java Unicode*4): 5172 16#1434 2#1010000110100 8#12064 ᐴ")
;;                        ("\\U1434"     . "\\U1434 (C++/Java Unicode*4): 5172 16#1434 2#1010000110100 8#12064 ᐴ")
;;                        ("\\x14"       . "\\x14 (C++ Hex Char*1): 20 16#14 2#10100 8#24 ")
;;                        ("\\143"       . "\\143 (C++ Oct Char*1): 99 16#63 2#1100011 8#143 c")
;;                        ("x'14'"       . "x'14' (GNU Fortran X BOZ Hex): 20 16#14 2#10100 8#24 ")
;;                        ("z'14'"       . "z'14' (F77 BOZ Hex): 20 16#14 2#10100 8#24 ")
;;                        ("b'101'"      . "b'101' (F77 BOZ Bin): 5 16#5 2#101 8#5 ")
;;                        ("o'14'"       . "o'14' (F77 BOZ Oct): 12 16#C 2#1100 8#14 ")
;;                        ("'14'x"       . "'14'x (GNU Fortran SfX BOZ Hex): 20 16#14 2#10100 8#24 ")
;;                        ("'14'z"       . "'14'z (GNU Fortran Sfx BOZ Hex): 20 16#14 2#10100 8#24 ")
;;                        ("'101'b"      . "'101'b (GNU Fortran Sfx BOZ Bin): 5 16#5 2#101 8#5 ")
;;                        ("'14'o"       . "'14'o (GNU Fortran Sfx BOZ Oct): 12 16#C 2#1100 8#14 ")))
;;     (cl-destructuring-bind (test-string . test-result) test-case
;;       (let ((ret-result (mjr-eval-unsigned-integer test-string :int)))
;;         (if (string-equal tmode "test")
;;             (if (not (string-equal ret-result test-result))
;;                 (print (format "ERROR: %S => %S Expected: %S" test-string ret-result test-result)))
;;             (princ (format "(%S . %S)\n" test-string ret-result)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun mjr-eval-internal (eval-str &optional engine)
  "Evaluate a EVAL-STR in the given ENGINE. The result is returned as a string, printed, and placed on the kill ring.

When EVAL-STR is NIL, return a error message string if ENGINE is NOT aviable.

Valid values for ENGINE:
 - :int ..... Use `mjr-meta-eval-multibase-convert'
 - :calc .... Like calling `quick-calc'
 - :elisp ... Like calling `eval-expression' (M-:)"
  (interactive (list (mjr-eval-util-extract-eval-str-from-buffer) 
                     (mjr-eval-util-read-engine '(:calc :elisp :int))))
  (if eval-str
      (if-let ((engine-err (mjr-eval-internal nil engine)))
          (error engine-err)
        (let* ((raw-res (pcase engine
                          (:int   (mjr-eval-unsigned-integer eval-str))
                          (:calc  (calc-eval eval-str))
                          (:elisp (format "%s" (eval (car (read-from-string eval-str))))))))
          (unless (stringp raw-res)
            (error "mjr-eval-internal: Something went wrong during evaluation!"))
          (let ((prt-res (string-trim-right raw-res)))
            (message (mjr-eval-util-format-result 'mjr-eval-external-session engine eval-str prt-res))
            (kill-new prt-res)
            prt-res)))
      (or (unless (member engine '(:calc :elisp :int))
            (format "mjr-eval-internal: ENGINE (%s) Unsupported value!" engine))
          (when (eq engine :calc)
            (unless (require 'calc nil :noerror)
              "mjr-eval-internal: ENGINE (:calc) not available! calc-mode could not be loaded.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defcustom mjr-eval-meta-engines '((:int              . (mjr-eval-internal         ?i "Integer"))
                                   (:calc             . (mjr-eval-internal         ?c "Evaluate with Emacs calc"))
                                   (:elisp            . (mjr-eval-internal         ?e "Evaluate as Emacs lisp code"))
                                   (:lisp-session     . (mjr-eval-external-session ?l "Evaluate as Common lisp code via SLIME"))
                                   (:maxima-session   . (mjr-eval-external-session ?m "Evaluate in maxima session"))
                                   (:r-session        . (mjr-eval-org-ticker       ?r "Evaluate in R session"))
                                   (:octave-session   . (mjr-eval-org-ticker       ?o "Evaluate in octave session")))
  "Engines available for `mjr-eval-meta-engines'."
  :type '(alist :key-type symbol :value-type (list symbol character string))
  :group 'mjr-eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defcustom mjr-eval-meta-use-ido nil
  "Use `ido-completing-read' if non-NIL.  Otherwise use `read-answer'.
`read-answer' provides a faster, but more terse user interface -- i.e. only one keystroke to select an evaluation method instead of two."
  :type 'boolean
  :group 'mjr-meta-eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun mjr-eval-meta (eval-str engine)
  "Evaluate the region or expression near the point using an ENGINE on `mjr-eval-meta-engines', and put the result in the kill ring.
Arguments:
 - EVAL-STR: A string to evaluate.  (it may also be NIL to check the availability of ENGINE)
   In interactive mode:
    - If region is active, then it is used.
    - If the region is inactive, then look near the point:
      - In Lisp adjacent modes, look for a sexp at the point (point should be on the opening paren or just after the closing paren).
      - In non-lisp modes, look a string not containing white-space.
      - If whatever is found doesn't look like a single integer, then the user is given the opportunity to edit it before evaluation.
 - ENGINE: One of the keys of `mjr-eval-meta-engines'
   In interactive mode, when EVAL-STR doesn't look like a single integer, the user is prompted for an engine."
  (interactive (let ((eval-str (or (and transient-mark-mode
                                        (region-active-p)
                                        (mark)
                                        (buffer-substring-no-properties (region-beginning) (region-end)))
                                   (read-string "Expression to evaluate: " (if (string-match "\\(slime\\|lisp\\)" (symbol-name major-mode))
                                                                               (thing-at-point 'sexp)
                                                                               (and (thing-at-point-looking-at "\\([^[:space:]]+\\)" 30)
                                                                                    (match-string 0)))
                                                ""))))
                 (if (mjr-eval-unsigned-integer eval-str)
                     (list eval-str :int)
                     (list eval-str
                           (if (and mjr-eval-meta-use-ido (require 'ido nil :noerror))
                               (mjr-eval-util-read-engine mjr-eval-meta-engines)
                               (let ((read-answer-short t))
                                 (intern (read-answer "Eval how: " (mapcar (lambda (x) (list (symbol-name (car x)) (nth 2 x) (nth 3 x))) mjr-eval-meta-engines)))))))))
  (let ((engine-info (cdr (assoc engine mjr-eval-meta-engines))))
    (if eval-str
        (if-let ((engine-err (mjr-eval-meta nil engine)))
            (error engine-err)
          (funcall (car engine-info) eval-str engine))
        (or (unless engine-info
              (format "mjr-eval-meta: ENGINE (%s) was not found in `mjr-eval-meta-engines'!" engine))
            (unless (listp engine-info)
              (format "mjr-eval-meta: ENGINE (%s) has malformed entry in `mjr-eval-meta-engines'! Not a list!" engine))
            (unless (= 3 (length engine-info))
              (format "mjr-eval-meta: ENGINE (%s) has malformed entry in `mjr-eval-meta-engines'! Wrong length!" engine))
            (funcall (car engine-info) nil engine)))))

;; (mjr-eval-meta "inv([1,2,3;4,5,6;7,8,10])" :octave-session)
;; "
;; #+RESULTS: oc-1
;; #+begin_example
;; ans =
;;
;;   -0.6667  -1.3333   1.0000
;;   -0.6667   3.6667  -2.0000
;;    1.0000  -2.0000   1.0000
;; #+end_example
;; "
;; (mjr-eval-meta "inv([1,2,3;4,5,6;7,8,10])" :calc)
;; "[[-0.666666666667, -1.33333333333, 1], [-0.666666666667, 3.66666666667, -2], [1, -2, 1]]"
;;
;; (mjr-eval-meta "inv([[1,2,3],[4,5,6],[7,8,10]])" :calc)
;; "[[-0.666666666667, -1.33333333333, 1], [-0.666666666667, 3.66666666667, -2], [1, -2, 1]]"
;;
;; (mjr-eval-meta "invert(matrix([1,2,3],[4,5,6],[7,8,10]))" :maxima-session)
;; "matrix([-2/3,-4/3,1],[-2/3,11/3,-2],[1,-2,1])"
;;
;; (mjr-eval-meta "expand((x+1)^10)" :maxima-session)
;;
;; (mjr-eval-meta "expand((x+1)^10)" :calc)
;; "x^10 + 10 x^9 + 45 x^8 + 120 x^7 + 210 x^6 + 252 x^5 + 210 x^4 + 120 x^3 + 45 x^2 + 10 x + 1"
;;
;; (mjr-eval-meta "(+ 1 2)" :elisp)
;; "3"
;;
;; (mjr-eval-meta "(+ 1 2)" :lisp)
;; "3"
;;
;; (mjr-eval-meta "1+2" :calc)
;; "3"
;;
;; (mjr-eval-meta "float(sin(1))" :maxima-session)
;; "0.8414709848078965"
;;
;; (mjr-eval-meta "sin(1)" :octave-session)
;; "
;; #+RESULTS: oc-257
;; #+begin_example
;; ans = 0.8415
;; #+end_example
;; "
;;
;; (mjr-eval-meta "sin(1)" :calc)
;; "0.0174524064373"
;;
;; (mjr-eval-meta "(sin 1)" :elisp)
;; "0.8414709848078965"
;;
;; (mjr-eval-meta "solve(matrix(c(1,2,3,4,5,6,7,8,10), nrow=3, byrow=TRUE))" :r-session)
;; "
;; #+RESULTS: oc-420
;; #+begin_example
;;            [,1]      [,2] [,3]
;; [1,] -0.6666667 -1.333333    1
;; [2,] -0.6666667  3.666667   -2
;; [3,]  1.0000000 -2.000000    1
;; #+end_example
;; "

;; (mjr-install-mjr-packages :reinstall :git 'mjr-eval)

(provide 'mjr-eval-meta)

;;; mjr-eval-meta.el ends here
