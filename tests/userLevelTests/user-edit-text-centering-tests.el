;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

;; TODO: Finish last headings

(add-to-list 'load-path "cask")
(require 'block-comment-mode)

(add-to-list 'load-path "tests")
(require 'block-comment-test-helpers)

(describe "Test user edit text centering"

  (before-each
    (erase-buffer)
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                               Entering text                               """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Enter small text"
    (let(
         (start-string "
/*******************************************************************************/
/*                                      <p>                                    */
/*******************************************************************************/
")
         (insert-string "AA")
         ;; String is 21 characters long: Should be 28 spaces on each side
         (expected-string "
/*******************************************************************************/
/*                                     AA<p>                                   */
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
      (user-write-text insert-string)
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )

(it "Enter large text"
    (let(
         (start-string "
/*******************************************************************************/
/*                                      <p>                                    */
/*******************************************************************************/
")
         (insert-string "Inserted with centering")
         ;; String is 21 characters long: Should be 28 spaces on each side
         (expected-string "
/*******************************************************************************/
/*                           Inserted with centering<p>                        */
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
      (user-write-text insert-string)
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )

(it "Enter large text by paste"
    (let(
         (start-string "
/*******************************************************************************/
/*                                      <p>                                    */
/*******************************************************************************/
")
         (insert-string "Inserted with centering")
         ;; String is 21 characters long: Should be 28 spaces on each side
         (expected-string "
/*******************************************************************************/
/*                           Inserted with centering<p>                        */
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

      ;; Insert text
      (insert insert-string)
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                               Removing text                               """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Remove text"
    (let(
         (start-string "
/*******************************************************************************/
/*                            Inserted no centering<p>                         */
/*******************************************************************************/
")
         (remove-string " centering")
         (expected-string "
/*******************************************************************************/
/*                                 Inserted no<p>                              */
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
      (user-remove-text (length remove-string))
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )

(it "Remove text by kill"
    (let(
         (start-string "
/*******************************************************************************/
/*                            Inserted no centering<p>                         */
/*******************************************************************************/
")
         (remove-string "centering")
         (expected-string "
/*******************************************************************************/
/*                                Inserted no <p>                              */
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

      ;; Kill word
      (backward-kill-word 1)

      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                         Entering text with no room                        """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                     Removing text with expanded comment                   """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

)
