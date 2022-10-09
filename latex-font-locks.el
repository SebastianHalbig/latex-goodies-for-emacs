(defun latex-regexp-prettify ()
  "We use an unobtrusive font for certain latex-internal formating rules such as `{...}'"
  (highlight-regexp "{" 'custom-comment)
  (highlight-regexp "}" 'custom-comment)
  (highlight-regexp "[[:space:]]*\\\\quad[[:space:]]*" 'custom-comment)
  (highlight-regexp "[[:space:]]*\\\\qquad[[:space:]]*" 'custom-comment)
  (highlight-regexp "[[:space:]]*\\\\,[[:space:]]*" 'custom-comment)
  (highlight-regexp "[[:space:]]*\\\\;[[:space:]]*" 'custom-comment)
  (highlight-regexp "[[:space:]]*\\\\enspace[[:space:]]*" 'custom-comment)
  (highlight-regexp "[[:space:]]*\\\\emspace[[:space:]]*" 'custom-comment)
  )

(provide 'latex-font-locks)
