;; ==================================================
;; options.el
;; Options Initialization - configurations that are
;; not necessarily defined or tied to a specific mode
;;
;; Part of Knavemacs configuration
;;==================================================

;; how search works
(setq isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) "
      lazy-count-suffix-format nil
      search-whitespace-regexp ".*?")
(setq xref-search-program 'ripgrep
      grep-command "rg -nS --no-heading"
      grep-find-ignored-directories
               '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))

;; how undo works
(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))
(setq kill-do-not-save-duplicates t)

;; how tabs work
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil) ; spaces only
(setq tab-width 4)

;; additional mode switches
(electric-pair-mode 1)
(global-eldoc-mode -1)
