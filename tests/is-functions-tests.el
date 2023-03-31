;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

(add-to-list 'load-path "cask")
(require 'block-comment-mode)

(add-to-list 'load-path "tests")
(require 'block-comment-test-helpers)

(describe "Test is functions"

  (before-each
    (erase-buffer)
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""  is-current-line-empty                                                    """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (it "Test is-current-line-empty: Line is empty"
    (expect (block-comment--is-current-line-empty)
            :to-be
            t))

  (it "Test is-current-line-empty: Line is not empty"
    (insert "random string")
    (expect (block-comment--is-current-line-empty)
            :to-be
            nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""  is-enclose                                                               """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (it "Test is-enclose: Has enclose"
    (let(
         (start-string "
/*******************************************************************************/p
")
         (prefix "/*")
         (postfix "*/")
         (fill "*")
         )

      ;; Insert block comment and put point at correct position
      (insert start-string)
      (jump-to-p " " t)

      (expect (block-comment--is-enclose prefix fill postfix) :to-be t)
      )
    )

  (it "Test is-enclose: Has indented enclose"
    (let(
         (start-string "
              /*****************************************/  p
")
         (prefix "/*")
         (postfix "*/")
         (fill "*")
         )

      ;; Insert block comment and put point at correct position
      (insert start-string)
      (jump-to-p " " t)

      (expect (block-comment--is-enclose prefix fill postfix) :to-be t)
      )
    )

  (it "Test is-enclose: Small enclose"
    (let(
         (start-string "
/*******/p
")
         (prefix "/*")
         (postfix "*/")
         (fill "*")
         )

      ;; Insert block comment and put point at correct position
      (insert start-string)
      (jump-to-p " " t)

      (expect (block-comment--is-enclose prefix fill postfix) :to-be t)
      )
    )

  (it "Test is-enclose: No enclose"
    (let(
         (start-string "
if (i < 10)p
")
         (prefix "/*")
         (postfix "*/")
         (fill "*")
         )

      ;; Insert block comment and put point at correct position
      (insert start-string)
      (jump-to-p " " t)

      (expect (block-comment--is-enclose prefix fill postfix) :to-be nil)
      )
    )

  (it "Test is-enclose: Enclose with gap in fill"
    (let(
         (start-string "
/************************************************** *****************************/p
")
         (prefix "/*")
         (postfix "*/")
         (fill "*")
         )

      ;; Insert block comment and put point at correct position
      (insert start-string)
      (jump-to-p " " t)

      (expect (block-comment--is-enclose prefix fill postfix) :to-be nil)
      )
    )

  (it "Test is-enclose: Enclose with deformed prefix"
    (let(
         (start-string "
*******************************************************************************/p
")
         (prefix "/*")
         (postfix "*/")
         (fill "*")
         )

      ;; Insert block comment and put point at correct position
      (insert start-string)
      (jump-to-p " " t)

      (expect (block-comment--is-enclose prefix fill postfix) :to-be nil)
      )
    )

  (it "Test is-enclose: Enclose with deformed postfix"
    (let(
         (start-string "
/******************************************************************************* /p
")
         (prefix "/*")
         (postfix "*/")
         (fill "*")
         )

      ;; Insert block comment and put point at correct position
      (insert start-string)
      (jump-to-p " " t)

      (expect (block-comment--is-enclose prefix fill postfix) :to-be nil)
      )
    )

  (it "Test is-enclose: Comment body"
    (let(
         (start-string "
/*                                                                             */p
")
         (prefix "/*")
         (postfix "*/")
         (fill "*")
         )

      ;; Insert block comment and put point at correct position
      (insert start-string)
      (jump-to-p " " t)

      (expect (block-comment--is-enclose prefix fill postfix) :to-be nil)
      )
    )
)
