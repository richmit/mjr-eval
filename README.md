# mjr-eval

<!-- SHELLO: ~/core/codeBits/bin/emacs_package_com_to_md.rb mjr-eval.el -->
 See the README: https://github.com/richmit/mjr-eval/

 Emacs by itself provides some solid computational support in the form of Emacs Lisp and `calc`.  Emacs also provides fantastic interfaces to many external
 computational tools (R, Octave, Matlab, Maple, Maxima, Julia, GAP, Macaulay2, etc...).  While many of these packages are quite powerful, they frequently
 don't provide `quick-calc` functionality -- and when they do it is with wildly different user interfaces.

 In the worst cases, users are forced to use a flow something like this:
  - Highlight some equation in the current buffer
  - Switch to a computational mode
  - Paste the equation
  - Adjust the syntax to match the tool & evaluate the result
  - Highlight the result
  - Switch back to the original buffer
  - Paste the result
  
 This package aims to replace the ghastly practice described above with an efficient and uniform user interface:
  - One key binding to access all the computational tools.
  - Helps to identify buffer contents for evaluation.
  - Provides a fast way for the user to select which computational tool to use (just a single key press)
  - Evaluates the expression and both prints the results and places them on the kill ring.

 This package uses a couple terms:
  - ENGINE to mean a computational tools (i.e. octave is an engine as is `calc`).  
  - HANDLER is a function that takes a string to evaluate and a symbol identifying an ENGINE.

 This package provides four primary HANDLERs and a single dispatch function that can be used as an interface to them all.  The four HANDLERs are:

  - `mjr-eval-org-ticker` -- Use `org-mode` babel for all computations.
     - Pro: Keeps a history of all computations preformed during the session
     - Con: Requires a working `org-mode` configuration and babel configuration for each language used
     - Con: Babel support for some engines may be limited.
    
  - `mjr-eval-external-session` -- Use an external tool via the author's favorite Emacs mode for that tool
    - Pro: Uses interactive modes that are very popular
    - Con: Not everybody is going to like the choice of which mode was used to provide support
    - Pro: For users already running sessions with these tools, this packge provides an alternate interface for interacting with sessions.
    - Con: Somewhat flaky support for :octave & :r
    
  - `mjr-eval-internal`-- Use internal Emacs facilities
    - Pro: Requires zero configuration
    - Pro: Lowest latency results
    - Con: Fewer & less powerful engines
    
  - `mjr-eval-external-one`-- Use an external tool by directly calling the binary
    - Pro: Super simple and very robust
    - Con: High latency
    - Pro/Con: No session support (which also means no session to crash or freeze)

 While all of the HANDLERs can be used interactively, it is probably more convenient to access them via the dispatch function `mjr-eval-meta`.  This
 function has a more sophisticated user interface and allows the user to customize what tools are available.

 I normally bind `mjr-eval-meta` to "ESC ESC :".  I think of `mjr-eval-meta` as an extended version of `eval-expression` that is normally bound to "ESC :".

      (keymap-global-set "ESC ESC :" 'mjr-eval-meta)
            
 The easiest way to install mjr-eval is to pull it directly from github:

      (package-vc-install (list 'mjr-eval
                           :url "https://github.com/richmit/mjr-eval"
                           :rev 'newest))
