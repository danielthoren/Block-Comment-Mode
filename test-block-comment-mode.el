;; Turn lexical binding off to enable desired behaviour of the :var form

;;; -*- lexical-binding: nil; -*-

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


(describe "Test is functions"
          (it "Test empty line check")

              (expect (block-comment--is-current-line-empty) :to-be t))
