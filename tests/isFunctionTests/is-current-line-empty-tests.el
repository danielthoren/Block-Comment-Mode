;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

(add-to-list 'load-path "cask")
(require 'block-comment-mode)

(add-to-list 'load-path "tests")
(require 'block-comment-test-helpers)

(describe "Test is-current-line-empty"

  (before-each
    (erase-buffer)
    )

  (it "Line is empty"
    (expect (block-comment--is-current-line-empty)
            :to-be
            t)
    )

  (it "Line is not empty"
    (insert "random string")
    (expect (block-comment--is-current-line-empty)
            :to-be
            nil)
    )
)
