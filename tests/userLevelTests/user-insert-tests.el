;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

(add-to-list 'load-path "cask")
(require 'block-comment-mode)

(add-to-list 'load-path "tests")
(require 'block-comment-test-helpers)

(describe "Test user insert block comment: "

  (before-each
    (erase-buffer)
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                           Test normal insert                            """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (it "Insert non-centering"
    (let(
         (expected-string "
/*******************************************************************************/
/*                                                                             */
/*******************************************************************************/
")
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

      (setq result-string (buffer-string))

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Append newline at end since the templates have this. Emacs
      ;; automatically adds this when saving
      (setq result-string (concat result-string "\n"))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be 2)
      (expect (current-column) :to-be 4)
      )
    )

  (it "Insert centering"
    (let(
         (expected-string "
/*******************************************************************************/
/*                                                                             */
/*******************************************************************************/
")
         (result-string "")
         )

      ;; Init c++ block comment style
      (block-comment--init-comment-style 80
                                         "/*"   " "   "*/"
                                         "/*"   "*"   "*/")


      ;; Insert block comment
      (block-comment-start)
      (block-comment-toggle-centering)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      (setq result-string (buffer-string))

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Append newline at end since the templates have this. Emacs
      ;; automatically adds this when saving
      (setq result-string (concat result-string "\n"))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be 2)
      (expect (current-column) :to-be 40)
      )
    )

  (it "Insert indented"
    (let(
         (indent 2)
         (expected-string "
  /*****************************************************************************/
  /*                                                                           */
  /*****************************************************************************/
")
         (result-string "")
         )

      ;; Init c++ block comment style
      (block-comment--init-comment-style 80
                                         "/*"   " "   "*/"
                                         "/*"   "*"   "*/" )

      (insert "  ")

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      (setq result-string (buffer-string))

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Append newline at end since the templates have this. Emacs
      ;; automatically adds this when saving
      (setq result-string (concat result-string "\n"))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be 2)
      (expect (current-column) :to-be (+ 4 indent))
      )
    )

(it "Insert when just enough horizontal room"
    (let(
         (indent 61)
         (expected-string "
                                                             /******************/
                                                             /*                */
                                                             /******************/
")
         (result-string "")
         )

      ;; Init c++ block comment style
      (block-comment--init-comment-style 80
                                         "/*"   " "   "*/"
                                         "/*"   "*"   "*/" )

      (setq-default indent-tabs-mode nil)

      (insert (make-string indent (string-to-char " ")))

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      ;; Clean buffer
      (whitespace-cleanup)

      (setq result-string (buffer-string))

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Append newline at end since the templates have this. Emacs
      ;; automatically adds this when saving
      (setq result-string (concat result-string "\n"))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be 2)
      (expect (current-column) :to-be (+ 4 indent))
      )
    )

(it "Insert when not enough horizontal room"
    (let(
         (indent 62)
         (expected-string "")
         (expected-line-number nil)

         (result-string "")
         )

      ;; Init c++ block comment style
      (block-comment--init-comment-style 80
                                         "/*"   " "   "*/"
                                         "/*"   "*"   "*/" )

      (setq-default indent-tabs-mode nil)

      (insert (make-string indent (string-to-char " ")))
      (setq expected-string (buffer-string))
      (setq expected-line-number (line-number-at-pos))

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      (setq result-string (buffer-string))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be expected-line-number)
      (expect (current-column) :to-be  indent)
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                 Test normal inserts for common languages                  """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (it "Insert c++ (symetrical)"
    (let(
         (expected-string "
/*******************************************************************************/
/*                                                                             */
/*******************************************************************************/
")
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

      (setq result-string (buffer-string))

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Append newline at end since the templates have this. Emacs
      ;; automatically adds this when saving
      (setq result-string (concat result-string "\n"))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be 2)
      (expect (current-column) :to-be 4)
      )
    )

  (it "Insert c++ (non-symetrical)"
    (let(
         (expected-string "
/********************************************************************************
 ***                                                                          ***
 *******************************************************************************/
")
         (result-string "")
         )

      ;; Init c++ block comment style
      (block-comment--init-comment-style 80
                                         "***"   " "   "***"
                                         "/*"    "*"   "*"
                                         "*"     "*"   "*/")

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      (setq result-string (buffer-string))

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Append newline at end since the templates have this. Emacs
      ;; automatically adds this when saving
      (setq result-string (concat result-string "\n"))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be 2)
      (expect (current-column) :to-be 6)
      )
    )

  (it "Insert python"
    (let(
         (expected-string "
#################################################################################
\"\"\"                                                                           \"\"\"
#################################################################################
")
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

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Append newline at end since the templates have this. Emacs
      ;; automatically adds this when saving
      (setq result-string (concat result-string "\n"))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be 2)
      (expect (current-column) :to-be 5)
      )
    )

  (it "Insert elisp"
    (let(
         (expected-string "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
\"\"\"                                                                           \"\"\"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
")
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

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Append newline at end since the templates have this. Emacs
      ;; automatically adds this when saving
      (setq result-string (concat result-string "\n"))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be 2)
      (expect (current-column) :to-be 5)
      )
    )

  (it "Insert bash"
    (let(
         (expected-string "
#################################################################################
#                                                                               #
#################################################################################
")
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

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Append newline at end since the templates have this. Emacs
      ;; automatically adds this when saving
      (setq result-string (concat result-string "\n"))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be 2)
      (expect (current-column) :to-be 3)
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  """                              Test edge cases                              """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (it "Insert on non-empty line"
    (let(
         (expected-string "")
         (result-string "")
         (start-pos nil)
         )

      (newline)
      (insert "random string")

      (setq start-pos (point-marker))
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
      (expect (point-marker) :to-equal start-pos)
      )
    )
  )
