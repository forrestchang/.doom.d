;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(map! (:prefix-map ("b" . "buffer")
                   :desc "Previous buffer" "TAB" #'previous-buffer)
      (:prefix-map ("/" . "search")
                   :desc "Search buffer" "/" #'swiper)
      )
