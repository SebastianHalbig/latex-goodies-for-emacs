;;; latex-change-environments.el --- A few lines of elisp to change environments in a smart fashion.
;;
;; Copyright
;;
;; Dependencies
;;
;; Tested with emacs 29.0.50
;;
;;
;;

;; Global variables set via `generate-expanded-delimiters-lists'
(setq custom-env-starting-delimiters '())
(setq custom-env-ending-delimiters '())
(setq number-envs-of-type '())


;; Information about the environment at point are stored in these variables.
;; TODO: This should be local and transformed into an alist.
(setq env-at-point-name nil)
(setq env-at-point-start nil)


(defun cycle-texmath()
  "This function cycles through the math environments defined in custom-env-plist"
  (interactive)
  (cycle-env :math:)
  )

(defun cycle-env(type)
  (get-env-at-point type)
  (if env-number
      (if ending-pos
          (if (and (> (- ending-pos-wrapped-text starting-pos-wrapped-text) 0)
                   (looking-at-p (concat "[[:blank:]\n]*" (regexp-quote ending-del-expanded))))
              (cdlatex-tab)
            (next-environment type))
        (progn
          (save-excursion (end-env (nth 0 current-env) (nth 1 current-env) inline))
          (LaTeX-indent-line))
        )
    (progn
      (apply 'build-env  (insert-after (insert-after default-math-env 0 starting-pos-wrapped-text) 1 ending-pos-wrapped-text)) ;; TODO make this non math specific
      ))
  )

(defun next-environment (type)
  (setq next-env-number (mod (1+ env-number) (plist-get number-envs-of-type type)))
  (setq next-env (nth next-env-number (plist-get custom-env-plist type)))
  (setq label-begin (+ starting-pos (length starting-del-expanded)))

  (goto-char starting-pos)
  (delete-region starting-pos label-begin)
  (start-env (nth 0 next-env) (nth 1 next-env) (nth 2 next-env))

  (delete-region (point) (+ (point) (- starting-pos-wrapped-text label-begin)))
  (setq label-pos (generate-label (nth 3 next-env) label-text (nth 2 next-env)))
  ;; TODO globally update the label if it is changed

  (setq new-starting-pos (point))
  (setq wrap-offset (- new-starting-pos starting-pos-wrapped-text))
  (goto-char (+ ending-pos-wrapped-text wrap-offset))
  (delete-region (point) (+ ending-pos wrap-offset))
  (save-excursion (end-env (nth 0 next-env) (nth 1 next-env) (nth 2 next-env)))
  (LaTeX-indent-line)
  (if point-relative-to-start
      (goto-char (+ new-starting-pos point-relative-to-start))
    (goto-char new-starting-pos))
  )

(defun get-env-at-point (type)
  " Produces all relevant-information about the current environment."

  (is-pt-in-env type)
  (setq current-env (get-env-by-name type env-at-point-name))
  (setq starting-del-expanded (nth 0 (plist-get custom-env-starting-delimiters (nth 0 current-env))))
  (setq ending-del-expanded (nth 0 (plist-get custom-env-ending-delimiters (nth 0 current-env))))
  (setq inline (nth 2 current-env))
  (setq label-tag (nth 3 current-env))
  (setq env-number (nth 4 current-env))
  (setq starting-pos env-at-point-start)

  (if starting-pos
      (save-excursion
        (setq ending-pos (environment-balanced current-env (1+ starting-pos)));; We do not want to start at the left boarder of the env as this might cause some problems
        (goto-char (+ starting-pos (length starting-del-expanded)))
        (skip-chars-forward "[:blank:]\n")
        (if (looking-at-p "\\\\label{")
            (progn
              (skip-chars-forward "^:")
              (skip-chars-forward "[:blank:]:")
              (if (looking-at "[^}]+")
                  (progn
                    (setq label-text (match-string 0))
                    (goto-char (match-end 0)))
                (progn
                  (setq label-text nil)
                  (forward-char)))
              (skip-chars-forward "}[:blank:]\n")
              )
          (setq label-text nil))
        (setq starting-pos-wrapped-text (point))
        )
    (progn
      (setq ending-pos nil)
      (setq label-text nil)
      (if (use-region-p)
          (setq starting-pos-wrapped-text (region-beginning))
        (setq starting-pos-wrapped-text nil))
      )
    )

  (if ending-pos
      (save-excursion
        (goto-char (- ending-pos (length ending-del-expanded)))
        (skip-chars-backward "[:blank:]\n")
        (setq ending-pos-wrapped-text (max (point) starting-pos-wrapped-text))
        )
    (progn
      (if (use-region-p)
          (setq ending-pos-wrapped-text (region-end))
        (setq ending-pos-wrapped-text nil))
      )
    )
  (if (and ending-pos-wrapped-text
           (< starting-pos-wrapped-text ending-pos-wrapped-text )
           (< (point) ending-pos-wrapped-text ) )
      (setq point-relative-to-start (- (point) starting-pos-wrapped-text))
    (setq point-relative-to-start nil))
  )

(defun environment-balanced (current-env starting-pos &optional hierarchy)
  (when (and current-env starting-pos)
    (save-excursion
      (setq hierarchy (nth 1 (plist-get custom-env-starting-delimiters (car current-env))))
      (goto-char starting-pos)
      (setq ending-pos (re-search-forward (regexp-quote (nth 0 (plist-get custom-env-ending-delimiters (car current-env)))) nil  t))
      (if ending-pos
          (progn
            ;; TODO search between start and end if there is a starting delimiter of same or lower hierarchy
            ending-pos
            )
        nil)
      ))
  )

(defun get-env-by-name (type starting-del)
  (when starting-del
    (setq environments (plist-get custom-env-plist type))
    (setq position 0)
    (setq return-env (car environments))
    (while (and (> (length environments) 0)(not (string= (car return-env) starting-del)))
      (setq environments (cdr environments))
      (setq position (1+ position))
      (setq return-env (car environments))
      )
    (when return-env (insert-after return-env 3 position)))
  )

(defun build-env (starting-del &optional starting-pos-wrapped-text ending-pos-wrapped-text ending-del inline label-tag label-text)
  "This function build an environment with 'starting-del' and 'ending-del' delemiters,
     a possible label with a tag and a text and some text in the middle."

  (setq start (point))
  (setq distance 0)

  (when starting-pos-wrapped-text
    (setq start starting-pos-wrapped-text)
    (when ending-pos-wrapped-text
      (setq distance (- ending-pos-wrapped-text start)))
    )

  (goto-char start)
  (start-env starting-del ending-del inline)
  (setq pointer-pos (generate-label label-tag label-text inline))
  (forward-char distance)
  (save-excursion (end-env starting-del ending-del inline))

  (unless inline (LaTeX-indent-line))
  (when (and (= distance 0) pointer-pos)
    (goto-char pointer-pos))
  )

(defun start-env (starting-del ending-del inline)
  (deactivate-mark)
  (unless (current-line-empty -1)
    (if inline (just-one-space) (TeX-newline)))
  (insert (expand-delimiters starting-del ending-del -1))
  )

(defun generate-label (&optional label-tag label-text inline)
  (setq point-pos nil)
  (when label-tag
    (just-one-space)
    (insert (concat "\\label{" label-tag ":" label-text "}"))
    (unless label-text (setq point-pos (- (point) 1)))
    )
  (unless inline
    (save-excursion
      (forward-line)
      (setq next-line-empty (current-line-empty)))
    (unless next-line-empty (TeX-newline)))
  (or point-pos nil)
  )

(defun end-env (starting-del ending-del inline)
  (deactivate-mark)
  (unless inline
    (if (current-line-empty)
        (progn
          (save-excursion
            (previous-line)
            (setq need-line-break (current-line-empty)))
          (unless need-line-break (TeX-newline)))
      (TeX-newline))
    )
  (insert (expand-delimiters starting-del ending-del 1))
  (LaTeX-indent-line)
  (unless (current-line-empty 1)
    (if inline (just-one-space) (TeX-newline)))
  )

(defun current-line-empty (&optional direction)
  "This function checks wether the current line is empty.
     The optional variable 'direction' takes values +1 and -1.
     In the first case only the line from the point onward is considered.
     If 'direction' is -1, the line up until the point is checked:
     ,*'direction =-1' "
  (if direction
      (if (eq direction -1)
          (looking-back "^[[:blank:]]*" ) (looking-at-p "[[:blank:]]*$"))
    (progn
      (save-excursion
        (beginning-of-line)
        (looking-at-p "[[:blank:]]*$")))))

(defun insert-after (input-list index val)
  "This function produces a copy of the input list with the given value `val' at the position `index'."
  (setq output-list (copy-sequence input-list))
  (push val (cdr (nthcdr index output-list)))
  output-list)

(defun expand-delimiters (starting-del ending-del se-flag)
  "This function produces an output string of the environment determined by the starting and ending-delimiters.
  The staring delimiter is produced if se-flag= -1, if se-flag = 1 the function returns the ending delimiter."
  (if (eq se-flag -1)
      (if ending-del (progn starting-del)(progn (concat "\\begin{" starting-del "}")))
    (if ending-del (progn ending-del)(progn (concat "\\end{" starting-del "}")))
    )
  )

(defun is-pt-in-env (type)
  (cond
   ((eq type :math:)
    (if (texmathp)
        (progn
          (setq env-at-point-name (car texmathp-why))
          (setq env-at-point-start (cdr texmathp-why)))
      (progn
        (setq env-at-point-name nil)
        (setq env-at-point-start nil))))
   ((and (eq type :list:)
         (member (car(reftex-what-environment 1)) (plist-get :list:)))
    (setq env-at-point-name (car (reftex-what-environment 1)))
    (setq env-at-point-start (cdr setq (reftex-what-environment 1)))
    )
   )
  )

(provide 'latex-change-environments)
