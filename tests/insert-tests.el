;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

(add-to-list 'load-path "cask")
(require 'block-comment-mode)

(add-to-list 'load-path "tests")
(require 'block-comment-test-helpers)

(describe "Test normal insert block comment"

  (before-each
    (erase-buffer)
    (newline)
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  """                 Test normal inserts for common languages                  """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (it "Test c++ insert (symetrical)"
    (let(
         (expected-string (file-to-string "./tests/templates/symetrical-template.cpp"))
         (result-string "")
         )

      ;; Init c++ block comment style
      (block-comment--init-comment-style 80
                                         "/*"   " "   "*/"
                                         "/*"   "*"   "*/" )

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)
      ;; (beginning-of-buffer)
      ;; (newline)

      (setq result-string (buffer-string))
      ;; Append newline at end since the templates have this. Emacs
      ;; automatically adds this when saving
      (setq result-string (concat result-string "\n"))

      (expect result-string :to-equal expected-string)
      )
    )

  (it "Test c++ insert (non-symetrical)"
    (let(
         (expected-string (file-to-string "./tests/templates/non-symetrical-template.cpp"))
         (result-string "")
         )

      ;; Init c++ block comment style
      (block-comment--init-comment-style 80
                                         "***"   " "   "***"
                                         "/*"    "*"   "*"
                                         nil
                                         "*"     "*"   "*/")

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      (setq result-string (buffer-string))
      ;; Append newline at end since the templates have this. Emacs
      ;; automatically adds this when saving
      (setq result-string (concat result-string "\n"))

      (expect result-string :to-equal expected-string)
      )
    )

  (it "Test python insert"
    (let(
         (expected-string (file-to-string "./tests/templates/template.py"))
         (result-string "")
         )

      ;; Init python block comment style
      (block-comment--init-comment-style 80
                                         "\"\"\""   " "   "\"\"\""
                                         "#"        "#"   "#")

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      (setq result-string (buffer-string))
      ;; Append newline at end since the templates have this. Emacs
      ;; automatically adds this when saving
      (setq result-string (concat result-string "\n"))

      (expect result-string :to-equal expected-string)
      )
    )

  (it "Test elisp insert"
    (let(
         (expected-string (file-to-string "./tests/templates/template.el"))
         (result-string "")
         )

      ;; Init python block comment style
      (block-comment--init-comment-style 80
                                         "\"\"\""   " "   "\"\"\""
                                         ";;"       ";"   ";;")

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      (setq result-string (buffer-string))
      ;; Append newline at end since the templates have this. Emacs
      ;; automatically adds this when saving
      (setq result-string (concat result-string "\n"))

      (expect result-string :to-equal expected-string)
      )
    )

  (it "Test bash insert"
    (let(
         (expected-string (file-to-string "./tests/templates/template.sh"))
         (result-string "")
         )

      ;; Init python block comment style
      (block-comment--init-comment-style 80
                                         "#"   " "   "#"
                                         "#"   "#"   "#")

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      (setq result-string (buffer-string))
      ;; Append newline at end since the templates have this. Emacs
      ;; automatically adds this when saving
      (setq result-string (concat result-string "\n"))

      (expect result-string :to-equal expected-string)
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  """                              Test edge cases                              """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (it "Test insert on non-empty line"
    (let(
         (expected-string "")
         (result-string "")
         )

      (newline)
      (insert "random string")

      (setq expected-string (buffer-string))

      ;; Init python block comment style
      (block-comment--init-comment-style 80
                                         "#"   " "   "#"
                                         "#"   "#"   "#")

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      (setq result-string (buffer-string))

      (expect result-string :to-equal expected-string)
      )
    )

  (it "Test insert on line that looks like block comment, type 1"
    (let(
         (expected-string "")
         (result-string "")
         )

      (newline)
      (insert "** Almost looks like block comment  **")

      (setq expected-string (buffer-string))

      ;; Init python block comment style
      (block-comment--init-comment-style 80
                                         "#"   " "   "#"
                                         "#"   "#"   "#")

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      (setq result-string (buffer-string))

      (expect result-string :to-equal expected-string)
      )
    )

  (it "Test insert on line that looks like block comment, type 2"
    (let(
         (expected-string "")
         (result-string "")
         )

      (newline)
      (insert "**  Almost looks like block comment **")

      (setq expected-string (buffer-string))

      ;; Init python block comment style
      (block-comment--init-comment-style 80
                                         "#"   " "   "#"
                                         "#"   "#"   "#")

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      (setq result-string (buffer-string))

      (expect result-string :to-equal expected-string)
      )
    )

  )
