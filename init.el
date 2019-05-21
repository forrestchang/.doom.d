;;; init.el -*- lexical-binding: t; -*-
;; Copy me to ~/.doom.d/init.el or ~/.config/doom/init.el, then edit me!

(doom! :completion
       (company
        +childframe)
       (ivy
        ;; +fuzzy
        +childframe)

       :ui
       deft
       doom
       doom-dashboard
       doom-quit
       ;; fill-column
       hl-todo
       ;; indent-guides
       modeline
       ;; nav-flash
       ;; neotree
       ;; ophints
       (popup
        +all
        +defaults)
       ;; pretty-code
       treemacs
       unicode
       vc-gutter
       vi-tilde-fringe
       (window-select
        +switch-window
        +numbers)
       workspaces

       :editor
       (evil
        +everywhere)
       file-templates
       fold
       format
       lispy
       multiple-cursors
       ;; rotate-text
       snippets

       :emacs
       (dired
        +ranger
        +icons)
       electric
       imenu
       vc

       :term
       eshell
       term
       vterm

       :tools
       ansible
       debugger
       docker
       editorconfig
       ein
       eval
       (flycheck
        +childframe)
       flyspell
       gist
       (lookup
        +docsets)
       lsp
       macos
       magit
       make
       pass
       pdf
       prodigy
       rgb
       terraform
       tmux
       upload
       wakatime

       :lang
       assembly
       cc
       clojure
       common-lisp
       erlang
       elixir
       elm
       emacs-lisp
       ess
       go
       haskell
       java
       javascript
       julia
       kotlin
       latex
       lua
       markdown
       (org
        +attach
        +bable
        +capture
        +export
        +present
        +ipython
        +protocol)
       php
       python
       scala
       sh
       web

       :email
       (mu4e
        +gmail)
       ;; notmuch
       ;; wanderlust

       :app
       ;; calendar
       ;; irc

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
       ;; literate
       )
