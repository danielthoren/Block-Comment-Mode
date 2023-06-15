;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

(add-to-list 'load-path "cask")
(require 'block-comment-mode)

(add-to-list 'load-path "tests")
(require 'block-comment-test-helpers)

(describe "Test is-empty-line"

  (before-each
    (erase-buffer)
    )

  (it "Blank line"
    (insert "       <p>     \n Not empty")
    (jump-to-p nil t)

    (expect (block-comment--is-empty-line)
            :to-be
            t)
    )

  (it "Not blank line"
    (insert "            \n Not empty <p>")
    (jump-to-p nil t)

    (expect (block-comment--is-empty-line)
            :to-be
            nil)
    )

  (it "Empty buffer"
    (expect (block-comment--is-empty-line)
            :to-be
            t)
    )
)
