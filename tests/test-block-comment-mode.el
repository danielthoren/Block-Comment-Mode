;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

;; (describe "Test insert block comment"
;;           (it "Test symetrical insert (python)"
;;               :var ((expected-string
;;                      "
;; #################################################################################
;; \"\"\"                                                                           \"\"\"
;; #################################################################################
;; ")
;;                     (result-string ""))

;;               ;; Init python block comment style
;;               (block-comment--init-comment-style 80
;;                                                  "\"\"\""
;;                                                  " "
;;                                                  "\"\"\""
;;                                                  "#"
;;                                                  "#"
;;                                                  "#")

;;               ;; Insert block comment
;;               (block-comment--insert-or-resume)
;;               (block-comment-abort)



;;               (expect t :to-be t)))

(add-to-list 'load-path "cask")
(require 'block-comment-mode)

(describe "Test is functions"
  (newline)
  (newline)
  (forward-line -1)
  (it "Test empty line check"
    (expect (block-comment--is-current-line-empty)
            :to-be
            t))

  (it "Test non-empty line check"
    (insert "random string")
    (expect (block-comment--is-current-line-empty)
            :to-be
            nil))
  )