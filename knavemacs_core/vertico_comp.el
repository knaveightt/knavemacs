;; vertico_comp.el - vertico completions and associated packages
;; Part of Knavemacs
;;
;; Includes:
;; - vertico
;; - savehist
;; - marginalia
;; - orderless
(use-package vertico
  :ensure t
  :bind (:map minibuffer-local-map
              ("C-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :ensure t
  :after vertico
  :init
  (savehist-mode))

(use-package marginalia
  :ensure t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

