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

;; (it "Toggle centering with existing text"
;;     (let(
;;          (user-text "ThisIs23CharactersLongg")
;;          (start-string "
;; /*******************************************************************************/
;; /*  ThisIs23CharactersLongg                                                    */<p>
;; /*******************************************************************************/
;; ")
;;          ;; Should have (79 - 4 - 23)/2 = 28 fill characters on each side
;;          (expected-string "
;; /*******************************************************************************/
;; /*                           ThisIs23CharactersLongg<p>                        */
;; /*******************************************************************************/
;; ")
;;          (result-string "")
;;          )

;;       (insert start-string)
;;       (jump-to-p nil t)

;;       ;; Resume block comment
;;       (block-comment-start)

;;       ;; Toggle centering
;;       (block-comment-toggle-centering)
;;       (block-comment-abort)

;;       ;; Clean buffer and add newline at top for better error message
;;       (whitespace-cleanup)

;;       (setq result-string (buffer-string))

;;       ;; Append newline at top for better error message
;;       (setq result-string (concat "\n" result-string))

;;       ;; Append newline at end since the templates have this. Emacs
;;       ;; automatically adds this when saving
;;       (setq result-string (concat result-string "\n"))

;;       ;; Check that position of point is correct
;;       (expect-point-at-p expected-string)

;;       ;; Remove <p>
;;       (setq expected-string (replace-p expected-string "   "))

;;       ;; Make strings easier to read in terminal
;;       (setq expected-string (make-whitespace-readable expected-string))
;;       (setq result-string (make-whitespace-readable result-string))

;;       (expect result-string :to-equal (replace-p expected-string "   "))
;;       )
;;     )

)
