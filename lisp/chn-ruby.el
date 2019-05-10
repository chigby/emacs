;;; chn-ruby.el --- the shimmering chainsaw

(use-package enh-ruby-mode
   :mode (("\\.rb$" . enh-ruby-mode)
         ("Rakefile$" . enh-ruby-mode)
         ("\\.rake$" . enh-ruby-mode)
         ("Gemfile$" . enh-ruby-mode)
         ("config\.ru$" . enh-ruby-mode)
         ("\\.gemspec$" . enh-ruby-mode)
         ("Vagrantfile$" . enh-ruby-mode))
   :config
   (define-key enh-ruby-mode-map (kbd "C-j") nil)
   (setq enh-ruby-deep-indent-paren nil)
   (custom-set-faces
    '(enh-ruby-string-delimiter-face ((t (:inherit font-lock-string-face :foreground nil))))))

(setq chn/ruby-version "2.6.3")

(use-package chruby
  :config
  (chruby chn/ruby-version))

(provide 'chn-ruby)
