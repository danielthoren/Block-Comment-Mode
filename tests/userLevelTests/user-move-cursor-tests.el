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
"""                          Move near right boundry                          """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Move to just before right boundry"
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

(it "Move to just outside right boundry"
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
      (expect mode-state :to-equal nil)

      ;; Check for equality
      (expect-buffer-equal right-before-boundry-string "")
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                          Move near left boundry                           """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Move to just before left boundry"
    (let(
         (start-string "
/*******************************************************************************/
/*                                                                             */<p>
/*******************************************************************************/
")
         (right-before-boundry-string "
/*******************************************************************************/
/*  <p>                                                                           */
/*******************************************************************************/
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p nil t)

      ;; Resume block comment
      (block-comment-start)

      (run-hooks 'post-command-hook)
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did not turn off
      (expect mode-state :to-equal t)

      ;; Check for equality
      (expect-buffer-equal right-before-boundry-string "")
      )
    )

(it "Move to just outside left boundry"
    (let(
         (start-string "
/*******************************************************************************/
/*                                                                             */<p>
/*******************************************************************************/
")
         (right-before-boundry-string "
/*******************************************************************************/
/* <p>                                                                            */
/*******************************************************************************/
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p nil t)

      ;; Resume block comment
      (block-comment-start)

      (backward-char 1)
      (run-hooks 'post-command-hook)
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did turn off
      (expect mode-state :to-equal nil)

      ;; Check for equality
      (expect-buffer-equal right-before-boundry-string "")
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                                 Move down                                 """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Move down within comment"
    (let(
         (start-string "
/*******************************************************************************/
/*  <p>                                                                        */
/*                                                                             */
/*******************************************************************************/
")
         (right-before-boundry-string "
/*******************************************************************************/
/*                                                                             */
/*  <p>                                                                        */
/*******************************************************************************/
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p "   ")

      ;; Resume block comment
      (block-comment-start)

      (next-line 1)
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did not turn off
      (expect mode-state :to-equal t)

      ;; Check for equality
      (expect-buffer-equal right-before-boundry-string)
      )
    )

(it "Move down to postamble row"
    (let(
         (start-string "
/*******************************************************************************/
/*  <p>                                                                        */
/*******************************************************************************/
")
         (right-before-boundry-string "
/*******************************************************************************/
/*                                                                             */
/***<p>****************************************************************************/
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p "   ")

      ;; Resume block comment
      (block-comment-start)

      (next-line 1)
      (block-comment--cursor-moved)
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did not turn off
      (expect mode-state :to-equal nil)

      ;; Check for equality
      (expect-buffer-equal right-before-boundry-string "")
      )
    )

(it "Move down outside of comment (no postamble row)"
    (let(
         (start-string "
/*  <p>                                                                        */
Outside of block comment
")
         (right-before-boundry-string "
/*                                                                             */
Outs<p>ide of block comment
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p "   ")

      ;; Resume block comment
      (block-comment-start)

      (next-line 1)
      (block-comment--cursor-moved)
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did not turn off
      (expect mode-state :to-equal nil)

      ;; Check for equality
      (expect-buffer-equal right-before-boundry-string "")
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                                  Move up                                  """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Move up within comment"
    (let(
         (start-string "
/*******************************************************************************/
/*                                                                             */
/*  <p>                                                                        */
/*******************************************************************************/
")
         (right-before-boundry-string "
/*******************************************************************************/
/*  <p>                                                                        */
/*                                                                             */
/*******************************************************************************/
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p "   ")

      ;; Resume block comment
      (block-comment-start)

      (next-line -1)
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did not turn off
      (expect mode-state :to-equal t)

      ;; Check for equality
      (expect-buffer-equal right-before-boundry-string)
      )
    )

(it "Move up to postamble row"
    (let(
         (start-string "
/*******************************************************************************/
/*  <p>                                                                        */
/*******************************************************************************/
")
         (right-before-boundry-string "
/***<p>****************************************************************************/
/*                                                                             */
/*******************************************************************************/
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p "   ")

      ;; Resume block comment
      (block-comment-start)

      (next-line -1)
      (block-comment--cursor-moved)
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did not turn off
      (expect mode-state :to-equal nil)

      ;; Check for equality
      (expect-buffer-equal right-before-boundry-string "")
      )
    )

(it "Move up outside of comment (no postamble row)"
    (let(
         (start-string "
Outside of block comment
/*  <p>                                                                        */
")
         (right-before-boundry-string "
Outs<p>ide of block comment
/*                                                                             */
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p "   ")

      ;; Resume block comment
      (block-comment-start)

      (next-line -1)
      (block-comment--cursor-moved)
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did not turn off
      (expect mode-state :to-equal nil)

      ;; Check for equality
      (expect-buffer-equal right-before-boundry-string "")
      )
    )

)
