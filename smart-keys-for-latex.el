;;; latex-goodies-for emacs.el
;; Not finished at all
(require 'dash)
(require 'cdlatex)

(require 'latex-env-dep-return)
(require 'latex-change-environments)

(defconst default-math-env '("$" "$" t nil)
  "The default math environment.
     This defines the fall-back option for the math environment insertion."
  )

(defconst default-ams-env '("definition" nil nil "def")
  "The default ams-environment.
     This defines the fall-back option for ams-type-environments."
  )

(defconst default-list-env '("thmlist" nil nil nil)
  "The default list-environment.
     This defines the fall-back option for list-environments."
  )

(defconst custom-env-plist'(
                            :ams-env: (("definition" nil nil "def")
                                       ("lemma" nil nil "lem")
                                       ("corollary" nil nil "cor")
                                       ("theorem" nil nil "thm"))
                            :math: (("$" "$" t nil)
                                    ("equation" nil nil "eq")
                                    ("align" nil nil "eq")
                                    ("gather" nil nil "eq")
                                    ("equation*" nil nil nil)
                                    ("align*" nil nil nil)
                                    ("gather*" nil nil nil)
                                    )
                            :parens: (("(" ")" t nil ))
                            :lists: (("thmlist" nil nil nil)
                                     ("itemize" nil nil nil)
                                     ("enumerate" nil nil nil)
                                     ("description" nil nil)
                                     )
                            )
  "A plist with environment-type as key and a list of such environments as value.
     Every entry in such a list consists of 4 elements:
     i)  the name of the environment, e.g. `equation' `definition' or `$'.
         Even more exotic types are supported such as `\left('.
     ii) the representation of the ending delimiter of the environment.
         If the environment is regular, i.e. of `\begin{...} ... \end{...}'-type,
         its value is nil. Otherwise it could be for example`$' or `'right)'.
     iii)A boolean variable indicating whether the environment is an inline environment.
         Here t equates to `inline' and `nil'  to new-line.
     iv) A representation of the label-tag used for this environment, e.g. `eq'.
         If the environment has no label, the value is nil."
  )

(defun setup-smart-keys ()
  "Initialise all lists correctly."
  (progn
    (generate-expanded-delimiters-lists)
    (generate-all-list-environments)))

(provide 'smart-keys-for-latex)
