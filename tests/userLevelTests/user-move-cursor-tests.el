;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

(add-to-list 'load-path "cask")
(require 'block-comment-mode)

(add-to-list 'load-path "tests")
(require 'block-comment-test-helpers)

(describe "Test user moves cursor"

  (before-each
    (erase-buffer)
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                        Move near rightmost boundry                        """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Move just before rightmost boundry"
    (let(
         (start-string "
/*******************************************************************************/
/*                                                                             */<p>
/*******************************************************************************/
")
         (right-before-boundry-string "
/*******************************************************************************/
/*                                                                          <p>   */
/*******************************************************************************/
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p nil t)

      ;; Resume block comment
      (block-comment-start)

      (forward-char 72)
      (run-hooks 'post-command-hook)
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did not turn off
      (expect mode-state :to-equal t)

      ;; Check for equality
      (expect-buffer-equal right-before-boundry-string "")

      )
    )

(it "Move just outside rightmost boundry"
    (let(
         (start-string "
/*******************************************************************************/
/*                                                                             */<p>
/*******************************************************************************/
")
         (right-before-boundry-string "
/*******************************************************************************/
/*                                                                           <p>  */
/*******************************************************************************/
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p nil t)

      ;; Resume block comment
      (block-comment-start)

      (forward-char 73)
      (run-hooks 'post-command-hook)
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did turn off
      (expect mode-state :to-equal t)

      ;; Check for equality
      (expect-buffer-equal right-before-boundry-string "")

      )
    )

)
