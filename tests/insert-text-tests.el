;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

(add-to-list 'load-path "cask")
(require 'block-comment-mode)

(add-to-list 'load-path "tests")
(require 'block-comment-test-helpers)

(describe "Test user insert text"

  (before-each
    (erase-buffer)
    )

  (it "Insert text no centering"
    (let(
         (user-text "User inserted this text")
         (expected-string "
/*******************************************************************************/
/*  User inserted this text <p>                                                */
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
      (insert user-text)
      (forward-char)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      ;; Check that position of point is correct
      (expect-point-at-p expected-string)

      (setq result-string (buffer-string))

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Append newline at end since the templates have this. Emacs
      ;; automatically adds this when saving
      (setq result-string (concat result-string "\n"))

      ;; Remove <p>
      (setq expected-string (replace-p expected-string "   "))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      )
    )

(it "Insert text with centering"
    (let(
         (user-text "ThisIs23CharactersLongg")
         ;; Should have (79 - 4 - 23)/2 = 28 fill characters on each side
         (expected-string "
/*******************************************************************************/
/*                           ThisIs23CharactersLongg<p>                        */
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
      ;; Enable centering
      (block-comment-toggle-centering)
      ;; Insert text
      (insert user-text)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      ;; Check that position of point is correct
      (expect-point-at-p expected-string)

      (setq result-string (buffer-string))

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Append newline at end since the templates have this. Emacs
      ;; automatically adds this when saving
      (setq result-string (concat result-string "\n"))

      ;; Remove <p>
      (setq expected-string (replace-p expected-string "   "))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      )
    )

;; (it "Insert text with no horizontal room"
;;     (let(
;;          (user-text "User inserted this text")
;;          (start-string "
;; /*******************************************************************************/
;; /*                                    User text that existed before test. <p>  */
;; /*******************************************************************************/
;; ")
;;          (expected-string "
;; /****************************************************************************************************/
;; /*                                    User text that existed before test. User inserted this text<p>*/
;; /****************************************************************************************************/
;; ")
;;          (result-string "")
;;          )

;;       (insert start-string)
;;       (jump-to-p "  ")

;;       ;; (message "start: \n%s" (buffer-string))

;;       ;; Resume block comment
;;       (insert user-text)
;;       (block-comment-start)
;;       (block-comment-abort)

;;       ;; Clean buffer and add newline at top for better error message
;;       (whitespace-cleanup)

;;       ;; Check that position of point is correct
;;       (message "res: column: %d row: %d" (current-column) (line-number-at-pos))
;;       (expect-point-at-p expected-string)

;;       (setq result-string (buffer-string))

;;       ;; Append newline at top for better error message
;;       (setq result-string (concat "\n" result-string))

;;       ;; Remove <p>
;;       (setq expected-string (replace-p expected-string "   "))

;;       ;; (message "Expected: \n%s" expected-string)
;;       ;; (message "Result: \n%s" result-string)

;;       ;; Make strings easier to read in terminal
;;       (setq expected-string (make-whitespace-readable expected-string))
;;       (setq result-string (make-whitespace-readable result-string))

;;       (expect result-string :to-equal expected-string)
;;       )
;;     )
)
