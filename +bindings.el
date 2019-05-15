;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;;; Leader key


(map! :leader
      :desc "M-x"                                    "SPC" #'execute-extended-command)

(map! (:prefix-map ("b" . "buffer")
                   :desc "Previous buffer" "TAB" #'previous-buffer)
      (:prefix-map ("/" . "search")
                   :desc "Search buffer" "/" #'swiper)
      )
