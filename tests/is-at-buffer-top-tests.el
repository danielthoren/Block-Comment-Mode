;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

(add-to-list 'load-path "cask")
(require 'block-comment-mode)

(add-to-list 'load-path "tests")
(require 'block-comment-test-helpers)

(describe "Test is-at-buffer-top/bot"

  (before-each
    (erase-buffer)
    )

  (it "At top"
    (let (
          (text "\
Random p
test
text
")
          )
      (insert text)
      (jump-to-p nil t)

      (expect (block-comment--is-at-buffer-top)
              :to-be
              t)

      (expect (block-comment--is-at-buffer-bot)
              :to-be
              nil)
      )
    )

  (it "At bot"
    (let (
          (text "\
Random
test
text p
")
          )
      (insert text)
      (jump-to-p nil t)

      (expect (block-comment--is-at-buffer-top)
              :to-be
              nil)

      (expect (block-comment--is-at-buffer-bot)
              :to-be
              t)
      )
    )

  (it "Not at top nor bot"
    (let (
          (text "\
Random
test p
text
")
          )
      (insert text)
      (jump-to-p nil t)

      (expect (block-comment--is-at-buffer-top)
              :to-be
              nil)

      (expect (block-comment--is-at-buffer-bot)
              :to-be
              nil)
      )
    )
)
