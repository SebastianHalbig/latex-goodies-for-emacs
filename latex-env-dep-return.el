;; We define a small helper function that allows us to change the behavior of <return> depending on the environment

(defun smart-return ()
  "This function helps us to deal with the behaviour of return in various latex environments"
  (interactive)
  (if (texmathp)
      (progn
        (if (current-line-empty)
            (progn
              (insert "\\\\")
              )
          )
        (progn
          (TeX-newline))
        )
    ;; (if = LaTeX-current-environment =
    ;; )
    (progn
      (TeX-newline)
      ))
  )

(provide 'latex-env-dep-return)
