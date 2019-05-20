;;; init.el -*- lexical-binding: t; -*-
;; Copy me to ~/.doom.d/init.el or ~/.config/doom/init.el, then edit me!

(doom! :feature
       eval
       (evil +everywhere)
       file-templates
       (lookup +docsets)
       snippets
       workspaces

       :completion
       (company +auto +childframe)
       (ivy +childframe)

       :ui
       doom
       doom-dashboard
       doom-quit
       evil-goggles
       hl-todo
       modeline
       treemacs
       (popup +all +defaults)
       vc-gutter
       vi-tilde-fringe
       (window-select +switch-window +numbers)

       :editor
       fold
       format
       multiple-cursors
       rotate-text

       :emacs
       dired
       electric
       imenu
       vc

       :tools
       (flycheck +childframe)
       lsp
       macos
       magit
       rgb
       wakatime

       :lang
       (cc +irony +rtags)
       data
       emacs-lisp
       go
       javascript
       markdown
       (org +attach +babel +capture +export +present +protocol +habit)
       (python
        ;; +lsp
        )
       (sh +fish)
       web

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app

       :collab

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;;literate

       ;; The default module sets reasonable defaults for Emacs. It also
       ;; provides a Spacemacs-inspired keybinding scheme and a smartparens
       ;; config. Use it as a reference for your own modules.
       (default +bindings +smartparens))
