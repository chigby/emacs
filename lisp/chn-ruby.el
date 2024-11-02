;;; chn-ruby.el --- the shimmering chainsaw

(use-package ruby-mode
  :ensure nil
  :defer t
  :mode (("\\.rb$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("\\.rake$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("config\.ru$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("Vagrantfile$" . ruby-mode))
   :config
   (setq ruby-deep-indent-paren nil))

(setq chn/ruby-version "2.6.6")

(use-package chruby
  :after ruby
  :config
  (chruby chn/ruby-version))

(provide 'chn-ruby)
