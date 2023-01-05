;; We define a small helper function that allows us to change the behavior of <return> depending on the environment

(setq latex-list-environements nil)

(defun generate-all-list-environments()
  "We read of all lists specified in 'custom-env-plist' under ':lists:'"
  (setq latex-list-environements (-flatten (plist-get custom-env-plist :lists:))))

(defun smart-return()
  "This function helps us to deal with the behaviour of return in various latex environments"
  (interactive)
  (if (current-line-empty)
    (if (texmathp) ;; Automatically adds "\\" in math environements.
        (save-excursion
          (previous-line)
          (end-of-line)
          (insert " ")
          (just-one-space)
          (insert "\\\\"))
      (if (member (LaTeX-current-environment) latex-list-environements) ;; Automatically adds "\item"
          (progn
            (LaTeX-indent-line)
            (insert "\\item ")
            )
        (TeX-newline)))
    (TeX-newline)))

(provide 'latex-env-dep-return)
