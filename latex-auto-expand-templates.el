
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

(provide 'latex-auto-expand-templates)
