;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

(add-to-list 'load-path "cask")
(require 'block-comment-mode)

(add-to-list 'load-path "tests")
(require 'block-comment-test-helpers)

(describe "Test user toggle centering"

  (before-each
    (erase-buffer)
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                                 Enabling                                  """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Enable centering"
    (let(
         (start-string "
/*******************************************************************************/
/*                                                                             */<p>
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*                                      <p>                                    */
/*******************************************************************************/
")
         (result-string "")
         )

      (insert start-string)
      (jump-to-p nil t)

      ;; Resume block comment
      (block-comment-start)

      ;; Enable centering
      (block-comment-toggle-centering)
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )

(it "Enable centering with existing text"
    (let(
         (start-string "
/*******************************************************************************/
/*  ThisIs23CharactersLongg                                                    */<p>
/*******************************************************************************/
")
         ;; Should have (79 - 4 - 23)/2 = 28 fill characters on each side
         (expected-string "
/*******************************************************************************/
/*                           ThisIs23CharactersLongg<p>                        */
/*******************************************************************************/
")
         (result-string "")
         )

      (insert start-string)
      (jump-to-p nil t)

      ;; Resume block comment
      (block-comment-start)

      ;; Enable centering
      (block-comment-toggle-centering)
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                                 Disabling                                 """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Disable centering"
    (let(
         (start-string "
/*******************************************************************************/
/*                                                                             */<p>
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*                                      <p>                                    */
/*******************************************************************************/
")
         (result-string "")
         )

      (insert start-string)
      (jump-to-p nil t)

      ;; Resume block comment
      (block-comment-start)

      ;; Enable centering
      (block-comment-toggle-centering)
      ;; Disable centering
      (block-comment-toggle-centering)

      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )

(it "Disable centering with existing text"
    (let(
         (start-string "
/*******************************************************************************/
/*                           ThisIs23CharactersLongg                           */<p>
/*******************************************************************************/
")
         (insert-string " Inserted no centering")
         (expected-string "
/*******************************************************************************/
/*                           ThisIs23CharactersLongg Inserted no centering<p>  */
/*******************************************************************************/
")
         (result-string "")
         )

      (insert start-string)
      (jump-to-p nil t)

      ;; Resume block comment
      (block-comment-start)

      ;; Enable centering
      (block-comment-toggle-centering)
      ;; Disable centering
      (block-comment-toggle-centering)

      ;; Test inserting user text, should not center
      (insert insert-string)

      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )

)
