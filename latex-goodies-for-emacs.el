;;; latex-smarter-keys.el --- Smart environment and newline handling for latex-mode.
;; Not finished at all


(defconst default-math-env '("$" "$" t nil)
  "The default math environment.
     This defines the fall-back option for the math environment insertion."
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
                            :parens:  (("(" ")" t nil ))
                            )
  "A plist with environment-type as key and a list of such environments as value.
     Every entry in such a list consists of 4 elements:
     i)  the name of the environment, e.g. `equation' or `$'.
     ii) the reprenstation of the ending delimitier of the environment.
         If the environment is bounded by `\begin{...}' and `\end{...}',
         its value is nil. Otherwise it could be for example`$'.
     iii)A boolean variable indicating wether the environment is an inline environment.
         That is t=`inline' and `nil' otherwise.
     iv) A representation of the label-tag used for this environment, e.g. `eq'.
         If the environment has no label, the value is nil."
  )

;; Global variables that allow us to
(setq custom-env-starting-delimiters '())
(setq custom-env-ending-delimiters '())
(setq number-envs-of-type '())

(setq env-at-point-name nil)
(setq env-at-point-start nil)


(defun generate-expanded-delimiters-lists()

  (dotimes (i (length custom-env-plist))
    (when(cl-oddp i)
      (progn
        (setq hierarchy (/ (- i 1) 2))
        (setq environments (nth i custom-env-plist))
        ;; Generate some statistics about the saved environments
        (add-to-list 'number-envs-of-type (nth (- i 1) custom-env-plist) t)
        (add-to-list 'number-envs-of-type (length environments) t)
        (dotimes (j (length environments))
          (progn
            (setq starting-del (nth 0 (nth j environments)))
            (setq ending-del (nth 1 (nth j environments)))
            (setq starting-del-exp (expand-delimiters starting-del ending-del -1))
            (setq ending-del-exp (expand-delimiters starting-del ending-del 1))
            (add-to-list 'custom-env-starting-delimiters starting-del t)
            (add-to-list 'custom-env-starting-delimiters (list starting-del-exp hierarchy) t)
            (add-to-list 'custom-env-ending-delimiters starting-del t)
            (add-to-list 'custom-env-ending-delimiters (list ending-del-exp hierarchy) t)
            )))))
  )

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
   )
  )

(defun smart-enter ()
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

(defun latex-regexp-prettify ()
  (highlight-regexp "{" 'custom-comment)
  (highlight-regexp "}" 'custom-comment)
  (highlight-regexp "[[:space:]]*\\\\quad[[:space:]]+" 'custom-comment)
  (highlight-regexp "[[:space:]]*\\\\qquad[[:space:]]+" 'custom-comment)
  (highlight-regexp "[[:space:]]*\\\\,[[:space:]]+" 'custom-comment)
  (highlight-regexp "[[:space:]]*\\\\;[[:space:]]+" 'custom-comment)
  (highlight-regexp "[[:space:]]*\\\\enspace[[:space:]]+" 'custom-comment)
  (highlight-regexp "[[:space:]]*\\\\emspace[[:space:]]+" 'custom-comment)
  )

(defun latex-symbols-prettify ()
  (mapc (lambda (pair) (add-to-list 'prettify-symbols-alist pair))
        ;; ARROWS
        '(("\\to"                  32 (Br . Bl) 32 (Br . Br) 57620)
          ("\\rightarrow"          32 (Br . Bl) 32 (Br . Br) 57620)
          ("\\mapsto"              8870 (cr  cl 16 0) 57620)
          ("\\longmapsto"          32 (Br . Bl)  8866 (cr . cl) 32 (Br . Bl)  32 (Br . Br)  57619)
          ("\\inj"                 5308 (tl  tl 20 0) 32 (tr  tl 0 -5) 57620)
          ("\\emb"                 5308 (tl  tl 20 0) 32 (tr  tl 0 5) 57620)
          ("\\hookrightarrow"      5354 (tl . tl) 32 (tr . tl) 57620)
          ("\\surj"                32 (Br . Bl) 32 (Br . Bl) 32 (Br . Br) 57621)
          ("\\proj"                32 (Br . Bl) 32 (Br . Bl) 32 (Br . Br) 57621)
          ("\\twoheadrightarrow"   32 (Br . Bl) 32 (Br . Bl) 32 (Br . Br) 57621)
          ("\\twoheadleftarrow"    32 (Br . Bl) 32 (Br . Bl) 32 (Br . Br) 57693)
          ("\\longrightarrow"      32 (Br . Bl) 32 (Br . Bl) 32 (Br . Br) 57619)
          ("\\rightsquigarrow"     32 (Br . Bl) 32 (Br . Br) 57703)
          ("\\leftarrow"           32 (Br . Bl) 32 (Br . Br) 57682)
          ("\\longleftarrow"       32 (Br . Bl) 32 (Br . Bl) 32 (Br . Br) 57683)
          ("\\leftrightarrow"      32 (Br . Bl) 32 (Br . Bl) 32 (Br . Br) 57684)
          ("\\leftsquigarrow"      32 (Br . Bl) 32 (Br . Br) 57696)
          ("\\Rightarrow"          32 (Br . Bl) 32 (Br . Br) 57663)
          ("\\Longrightarrow"      32 (Br . Bl) 32 (Br . Bl) 32 (Br . Br) 57662)
          ("\\implies"             32 (Br . Bl) 32 (Br . Bl) 32 (Br . Br) 57662)
          ("\\Leftarrow"           32 (Br . Bl) 32 (Br . Br) 57688)
          ("\\Longleftarrow"       32 (Br . Bl) 32 (Br . Br) 57688)
          ("\\impliedby"           32 (Br . Bl) 32 (Br . Br) 57688)
          ("\\Leftrightarrow"      32 (Br . Bl) 32 (Br . Bl) 32 (Br . Br) 57689)
          ("\\upharpoonleft"       ?↿)
          ("\\upharpoonright"      ?↾)
          ("\\downharpoonleft"     ?⇃)
          ("\\downharpoonright"    ?⇂)
          ("\\hookleftarrow"       ?↩)
          ("\\looparrowleft"       ?↫)
          ("\\looparrowright"      ?↬)
          ("\\leftrightsquigarrow" ?↭)
          ("\\leftleftarrows"      ?⇇)
          ("\\rightrightarrows"    ?⇉)
          ("\\leftrightarrows"     ?⇆)
          ("\\rightleftarrows"     ?⇄)
          ("\\Lleftarrow"         ?⇚)
          ("\\Rrightarrow"        ?⇛)
          ;; ENVIRONMENTS
          ("\\part"                ?#)
          ("\\section"             32 (Br . Bl) 32 (Br . Br) 57627)
          ("\\subsection"          32 (Br . Bl) 32 (Br . Bl) 32 (Br . Br) 57628)
          ("\\subsubsection"       32 (Br . Bl) 32 (Br . Bl) 32 (Br . Bl) 32 (Br . Br) 57629)
          ("\\begin"               32 (Br . Bl) 32 (Br . Br) 57653)
          ("\\end"                 32 (Br . Bl) 32 (Br . Br) 57677)
          ("\\item"                32 (Br . Bl) 32 (Br . Bl) 32 (Br . Br) 57686)
          ;; SPACING
          ("\\,"                  8194 (Bc . Bc) 8231)
          ("\\:"                  8194 (Bc . Bc) 8231)
          ("\\;"                  8194 (Bc . Bc) 8231)
          ("\\enspace"            8194 (Bc . Bc) 8231)
          ("\\emspace"            8195 (Bc . Bc) 8231)
          ("\\quad"               8231 (Bc . Bc) 8195 (Br . Bl) 8231 (Bc . Bc) 8195)
          ("\\qquad"              8231 (Bc . Bc) 8195 (Br . Bl) 8231 (Bc . Bc) 8195 (Br . Bl) 8231 (Bc . Bc) 8195 (Br . Bl) 8231 (Bc . Bc) 8195)
          ;; SETS
          ("\\mathbb{N}"           ?\u2115)
          ("\\mathbb{R}"           ?\u211D)
          ("\\mathbb{C}"           8450)
          ("\\mathbb{P}"           8473)
          ("\\mathcal{C}"          120018)
          ("\\cat{C}"              120018)
          ("\\mathcal{D}"          120019)
          ("\\cat{D}"              120019)
          ("\\cat{E}"              120020)
          ("\\cat{M}"              120028)
          ("\\cat{N}"              120029)
          ("\\mathfrak{C}"         120174)
          ("\\mathfrak{D}"         120175)
          ("\\ZCat"                120041)
          ;; OPERATORS
          ("\\defeq"               32 (Br . Bl) 32 (Br . Br) 57612)
          ("\\eq"                  ?=)
          ("\\neq"                 32 (Br . Bl) 32 (Br . Br) 57614)
          ("\\blank"               8212)
          ("\\from"                32 (Br . Br) 57708)
          ("\\colon"               32 (Br . Br) 57708)
          ("\\lact"                9655)
          ("\\blact"               9654)
          ("\\ract"                9665)
          ("\\bract"               9664)
          ("\\unlhd"               ?⊴)
          ("\\unrhd"               ?⊵)
          ("\\cdot "               8226)
          (" \\cdot "              8226)
          ("\\times "              215)
          (" \\times "             215)
          ("\\parallel"            ?∥)
          ("\\notni"               8260 (cc . cc) ?∋)
          ("\\sqcup"               ?⊔)
          ("\\bigsqcup"            ?⊔)
          ("\\sqcap"               ?⊓)
          ("\\bigsqcap"            ?⊓)
          ("\\biguplus"            ?⨄)
          ("\\land"                ?∧)
          ("\\lor"                 ?∨)
          ("\\odot"                ?⊙)
          ("\\bigodot"             ?⊙)
          ("\\otimes"              ?⊗)
          ("\\bigotimes"           ?⊗)
          ("\\oplus"               ?⊕)
          ("\\bigoplus"            ?⊕)
          ("\\sqrt"                ?√)
          ("\\iint"                ?∬)
          ("\\iiint"               ?∭)
          ("\\iiiint"              ?⨌)
          ("\\oiint"               ?∯)
          ("\\oiiint"              ?∰)
          ("\\ointclockwise"       ?∲)
          ("\\ointctrclockwise"    ?∳)
          ;; BETTER SUB AND SUPERSCRIPTS
          ("^0"                    8304)
          ("^1"                    185)
          ("^2"                    178)
          ("^3"                    179)
          ("^4"                    8308)
          ("^5"                    8309)
          ("^6"                    8310)
          ("^7"                    8311)
          ("^8"                    8312)
          ("^9"                    8313)
          ("^l"                    ?\u02E1)
          ("^r"                    ?\u02B3)
          ("^T"                    ?\u1d40)
          ("_0"                    8320)
          ("_1"                    8321)
          ("_2"                    8322)
          ("_3"                    8323)
          ("_4"                    8324)
          ("_5"                    8325)
          ("_6"                    8326)
          ("_7"                    8327)
          ("_8"                    8328)
          ("_9"                    8329)
          ;; GREEK UPPER CASES
          ("\\Alpha"               ?Α)
          ("\\Beta"                ?Β)
          ("\\Gamma"               ?Γ)
          ("\\Delta"               ?Δ)
          ("\\Epsilon"             ?Ε)
          ("\\Zeta"                ?Ζ)
          ("\\Eta"                 ?Η)
          ("\\Theta"               ?Θ)
          ("\\Iota"                ?Ι)
          ("\\Kappa"               ?Κ)
          ("\\Lambda"              ?Λ)
          ("\\Mu"                  ?Μ)
          ("\\Nu"                  ?Ν)
          ("\\Xi"                  ?Ξ)
          ("\\Omicron"             ?Ο)
          ("\\Pi"                  ?Π)
          ("\\Rho"                 ?Ρ)
          ("\\Sigma"               ?Σ)
          ("\\Tau"                 ?Τ)
          ("\\Upsilon"             ?Υ)
          ("\\Phi"                 ?Φ)
          ("\\Chi"                 ?Χ)
          ("\\Psi"                 ?Ψ)
          ("\\Omega"               ?Ω)
          ;; SPECIAL SYMBOLS
          ("\\ss"                  ?ß)
          ("\\aa"                  ?å)
          ("\\AA"                  ?Å)
          ("\\ae"                  ?æ)
          ("\\oe"                  ?œ)
          ("\\AE"                  ?Æ)
          ("\\OE"                  ?Œ)
          ("\\o"                   ?ø)
          ("\\O"                   ?Ø)
          ("\\l"                   ?ł)
          ("\\L"                   ?Ł)
          ("\\S"                   ?§)
          ;; ESCAPED SYMBOLS
          ("\\$"                   ?＄)
          ("\\%"                   ?％)
          ("\\#"                   ?＃)
          ("\\_"                   ?＿)
          ("\\&"                   ?& (Bc . Bc) ?|)
          ;;REFERECING
          ("\\ref"                 ?☞)
          ("\\cite"                ?†)
          ("\\footnote"            ?‡)
          ("\\label"               43 (cc . cc) ?‡)
          ("\\TeX"                 ?T (cr cl -20 -45) ?E (cr cl -20 24) ?X)
          ("\\LaTeX"               ?L (cr cl -60 35) ?A (cr cl -18 -20) ?T (cr cl -18 -60) ?E (cr cl -20 5) ?X)
          ;; PARENTHESES
          ("{"                     ?⎨)
          ("}"                     ?⎬)
          ("\\{"                   ?{)
          ("\\}"                   ?})
          ("\\lbrace"              ?{)
          ("\\rbrace"              ?})
          ("\\{"                   ?{)
          ("\\}"                   ?})
          ("\\|" .                 ?║)
          ;; Misc
          ("\\dots"                ?…)
          ("\\textbackslash"       ?＼)
          ("\\backslash"           ?＼)
          ("\\qed"                 ?□)
          ("\\lightning"           ?Ϟ)
          ("\\copyright"           ?©)
          ("\\texistregistered"    ?®)
          ("\\texttrademark"       ?™)
          ("\\euro"                8364)
          ("\\pounds"              ?£)
          ("\\lvert"               ?|)
          ("\\rvert"               ?|)
          ("\\lVert"               ?ǁ)
          ("\\rVert"               ?ǁ)
          )))

  ;;; `aas': sane auto-expanding snippets
(use-package aas
  :hook ((LaTeX-mode org-mode) . aas-activate-for-major-mode)
  :config
  (--each '(latex-mode org-mode)
    (aas-set-snippets it
      :cond #'texmathp
      " :"      "\\from "
      " **"     "\\star"
      " mm"     "\\mid "
      " ->"     "\\to "
      " mt"     "\\mapsto "
      " =>"     "\\implies "
      " ..."    "\\dots"
      " bln"    "\\blank"
      " def"    "\\defeq"
      " ~"      "\\sim"
      " =="     "\\cong"
      " ot"     "\otimes"
      " bz"     "\\mathbb{Z}"
      " bq"     "\\mathbb{Q}"
      " br"     "\\mathbb{R}"
      " bc"     "\\mathbb{C}"
      " bf"     "\\mathbb{F}"
      " bp"     "\\mathbb{P}"
      " fg"     "\\mathfrak{g}"
      " fh"     "\\mathfrak{h}"
      " cc"     "\\cat{C}"
      " cd"     "\\cat{D}"
      " cm"     "\\cat{M}"
      " zc"     "\\ZCat{C}"
      " subset" "\\subset"
      " supset" "\\supset"
      " <>"    (lambda () (interactive)
                (yas-expand-snippet "\\langle $0 \\rangle"))
      " adj"   (lambda () (interactive)
                (yas-expand-snippet "\\adj{${1:F}}{${2:U}}{${3:\\cc}}{${4:\\mm}} $0"))
      " hom"  (lambda () (interactive)
               (yas-expand-snippet "${1:\\cc}(${2:s}, ${3:t})"))
      " coend" (lambda () (interactive)
                (yas-expand-snippet "\\int^{${2:C \\in \\cc}}"))
      " end"   (lambda () (interactive)
                (yas-expand-snippet "\\int_{${2:C \\in \\cc}}"))
      " set"   (lambda () (interactive)
                (yas-expand-snippet "\\\\{$0"))
      " sum"   (lambda () (interactive)
                (yas-expand-snippet "\\sum_{${1:i=1}}^{${2:n}}$0"))
      ;;" gl"    (lambda () (interactive)
      ;;          (yas-expand-snippet "\\mathfrak{gl}_{${1:n}}$0"))
      ;;" //"    (lambda () (interactive)
      ;;          (yas-expand-snippet "\\frac{$1}{$2}$0"))
      " cat"   (lambda () (interactive)
                (yas-expand-snippet "\\cat{$1}$0")))))

(provide 'latex-goodies-for-emacs)
