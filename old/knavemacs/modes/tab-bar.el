(use-package tab-bar
  :ensure nil
  :defer t
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-auto-width t)
  (tab-bar-auto-width-min '(10 4))
  (tab-bar-auto-width-max '(50 5))
  :init
  ;; HACK this is an override of the internal function so it
  ;;      shows only the hint number with some decoration.
  (defun tab-bar-tab-name-format-hints (name _tab i)
    "Show absolute numbers on tabs in the tab bar before the tab name.
  It has effect when `tab-bar-tab-hints' is non-nil."
    (if tab-bar-tab-hints (concat (format " »%d«" i) "") name)))
