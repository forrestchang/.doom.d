;;; init.el -*- lexical-binding: t; -*-
;; Copy me to ~/.doom.d/init.el or ~/.config/doom/init.el, then edit me!

(doom! :completion
       (company
        +childframe)
       (ivy
        +fuzzy
        +childframe)

       :ui
       nav-flash
       doom
       doom-dashboard
       doom-quit
       hl-todo
       modeline
       (popup
        +all
        +defaults)
       unicode
       vc-gutter
       vi-tilde-fringe
       (window-select
        +switch-window
        +numbers)
       deft

       :editor
       (evil
        +everywhere)
       fold
       format
       snippets

       :input
       ;; chinese

       :emacs
       (dired
        +ranger
        +icons
        )
       vc

       :term
       vterm

       :tools
       eval
       lsp
       magit
       (lookup
        +dictionary
        +offline
        +docsets
        )

       :os
       macos

       :lang
       emacs-lisp
       latex
       markdown
       (org
        +attach
        +habit
        +babel
        +capture
        +export
        +present
        ;; +ipython
        +protocol
        +dragndrop
        +pomodoro
        +roam)
       ;; php
       (python
        +pyright
        +lsp)
       (go
        +lsp)
       ;; scala
       sh

       :email

       :app

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;;literate

       ;; The default module sets reasonable defaults for Emacs. It also
       ;; provides a Spacemacs-inspired keybinding scheme and a smartparens
       ;; config. Use it as a reference for your own modules.
       (default
         +bindings
         +smartparens)

       :private
       my-org
       ;; my-python
       )

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))
