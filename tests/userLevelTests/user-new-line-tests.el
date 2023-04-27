;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

(add-to-list 'load-path "cask")
(require 'block-comment-mode)

(add-to-list 'load-path "tests")
(require 'block-comment-test-helpers)

(describe "Test user enter new line"

  (before-each
    (erase-buffer)
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                          Normal 'RET' newlines                            """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (it "RET New line empty comment start position"
    (let(
         (start-string "
/*******************************************************************************/
/*  <p>                                                                        */
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*                                                                             */
/*  <p>                                                                        */
/*******************************************************************************/
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p "   " t)

      ;; Resume block comment
      (block-comment-start)

      (execute-kbd-macro (kbd "RET"))
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did not turn off
      (expect mode-state :to-equal t)

      ;; Check for equality
      (expect-buffer-equal expected-string "   ")
      )
    )

(it "RET New line empty comment non-start position"
    (let(
         (start-string "
/*******************************************************************************/
/*              <p>                                                            */
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*                                                                             */
/*  <p>                                                                        */
/*******************************************************************************/
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p "   " t)

      ;; Resume block comment
      (block-comment-start)

      (execute-kbd-macro (kbd "RET"))
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did not turn off
      (expect mode-state :to-equal t)

      ;; Check for equality
      (expect-buffer-equal expected-string "   ")
      )
    )

(it "RET New line with user text"
    (let(
         (start-string "
/*******************************************************************************/
/*  This is user text. This text should move to next line                      */<p>
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*  This is user text. This text should move to next line                      */
/*  <p>                                                                        */
/*******************************************************************************/
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p nil t)

      ;; Resume block comment
      (block-comment-start)

      (execute-kbd-macro (kbd "RET"))
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did not turn off
      (expect mode-state :to-equal t)

      ;; Check for equality
      (expect-buffer-equal expected-string "   ")
      )
    )

(it "RET New line with user text to the right of point"
    (let(
         (start-string "
/*******************************************************************************/
/*  This is user text. This text should move to next line                      */<p>
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*  This is user text.                                                         */
/*  <p>This text should move to next line                                         */
/*******************************************************************************/
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p nil t)

      ;; Resume block comment
      (block-comment-start)

      (backward-word 7)
      (execute-kbd-macro (kbd "RET"))
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did not turn off
      (expect mode-state :to-equal t)

      ;; Check for equality
      (expect-buffer-equal expected-string "")
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""        M-j neslines: keeping the indentation of previous row              """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "M-j New line empty comment start position"
    (let(
         (start-string "
/*******************************************************************************/
/*  <p>                                                                        */
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*                                                                             */
/*  <p>                                                                        */
/*******************************************************************************/
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p "   " t)

      ;; Resume block comment
      (block-comment-start)

      (execute-kbd-macro (kbd "M-j"))
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did not turn off
      (expect mode-state :to-equal t)

      ;; Check for equality
      (expect-buffer-equal expected-string "   ")
      )
    )

(it "M-j New line empty comment non-start position"
    (let(
         (start-string "
/*******************************************************************************/
/*              <p>                                                            */
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*                                                                             */
/*  <p>                                                                        */
/*******************************************************************************/
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p "   " t)

      ;; Resume block comment
      (block-comment-start)

      (execute-kbd-macro (kbd "M-j"))
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did not turn off
      (expect mode-state :to-equal t)

      ;; Check for equality
      (expect-buffer-equal expected-string "   ")
      )
    )

(it "M-j New line with user text indented"
    (let(
         (start-string "
/*******************************************************************************/
/*          This is user text. This text should move to next line              */<p>
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*          This is user text. This text should move to next line              */
/*          <p>                                                                */
/*******************************************************************************/
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p nil t)

      ;; Resume block comment
      (block-comment-start)

      (execute-kbd-macro (kbd "M-j"))
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did not turn off
      (expect mode-state :to-equal t)

      ;; Check for equality
      (expect-buffer-equal expected-string "   ")
      )
    )

(it "M-j New line with user text indented and with text to the right of point"
    (let(
         (start-string "
/*******************************************************************************/
/*          This is user text. This text should move to next line              */<p>
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*          This is user text.                                                 */
/*          <p>This text should move to next line                                 */
/*******************************************************************************/
")
         (mode-state nil)
         )

      (insert start-string)
      (jump-to-p nil t)

      ;; Resume block comment
      (block-comment-start)

      (backward-word 7)
      (execute-kbd-macro (kbd "M-j"))
      (setq mode-state block-comment-mode)

      (block-comment-abort)

      ;; Make sure mode did not turn off
      (expect mode-state :to-equal t)

      ;; Check for equality
      (expect-buffer-equal expected-string "")
      )
    )

)
