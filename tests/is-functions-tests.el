;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

(add-to-list 'load-path "cask")
(require 'block-comment-mode)

(add-to-list 'load-path "tests")
(require 'block-comment-test-helpers)

(describe "Test is functions"

  (before-each
    (erase-buffer)
    (newline)
    (newline)
    (forward-line -1))

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
