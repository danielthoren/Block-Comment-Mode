;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

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
"""                         Editing text with no room                         """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Enter text with no horizontal room"
    (let(
         (insert-string ". User inserted this text")
         (start-string "
/******************************************************************************/
/*  User text that existed before test. It is almost as wide as the comment<p>*/
/******************************************************************************/
")
         (expected-string "
/*******************************************************************************************************/
/*  User text that existed before test. It is almost as wide as the comment. User inserted this text<p>*/
/*******************************************************************************************************/
")
         (result-string "")
         )

      (insert start-string)
      (jump-to-p "  ")

      ;; Resume block comment
      (block-comment-start)
      (user-write-text insert-string)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )

(it "Remove text with no horizontal room"
    (let(
         (start-string "
/*******************************************************************************************************/
/*  User text that existed before test. It is almost as wide as the comment. User inserted this text<p>*/
/*******************************************************************************************************/
")
         (remove-string "inserted this text")
         (expected-string "
/*************************************************************************************/
/*  User text that existed before test. It is almost as wide as the comment. User <p>*/
/*************************************************************************************/
")
         (result-string "")
         )

      (insert start-string)
      (jump-to-p "  ")

      ;; Resume block comment
      (block-comment-start)
      (user-remove-text (length remove-string))
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )

(it "Remove text with no horizontal room until target width"
    (let(
         (start-string "
/*******************************************************************************************************/
/*  User text that existed before test. It is almost as wide as the comment. User inserted this text<p>*/
/*******************************************************************************************************/
")
         (remove-string ". User inserted this text")
         (expected-string "
/*******************************************************************************/
/*  User text that existed before test. It is almost as wide as the comment<p> */
/*******************************************************************************/
")
         (result-string "")
         )

      (insert start-string)
      (jump-to-p "  ")

      ;; Resume block comment
      (block-comment-start)
      (user-remove-text (length remove-string))
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                   Edit text by kill/insert with no room                   """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Enter text by paste with no horizontal room"
    (let(
         (insert-string ". User inserted this text")
         (start-string "
/******************************************************************************/
/*  User text that existed before test. It is almost as wide as the comment<p>*/
/******************************************************************************/
")
         (expected-string "
/*******************************************************************************************************/
/*  User text that existed before test. It is almost as wide as the comment. User inserted this text<p>*/
/*******************************************************************************************************/
")
         (result-string "")
         )

      (insert start-string)
      (jump-to-p "  ")

      ;; Resume block comment
      (block-comment-start)
      (insert insert-string)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )

(it "Remove text by kill with no horizontal room"
    (let(
         (start-string "
/*******************************************************************************************************/
/*  User text that existed before test. It is almost as wide as the comment. User inserted this text<p>*/
/*******************************************************************************************************/
")
         (remove-string "inserted this text")
         (expected-string "
/*************************************************************************************/
/*  User text that existed before test. It is almost as wide as the comment. User <p>*/
/*************************************************************************************/
")
         (result-string "")
         )

      (insert start-string)
      (jump-to-p "  ")

      ;; Resume block comment
      (block-comment-start)

      ;; Kill word
      (backward-kill-word 3)

      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )

(it "Remove text by kill with no horizontal room until target width"
    (let(
         (start-string "
/*******************************************************************************************************/
/*  User text that existed before test. It is almost as wide as the comment. User inserted this text<p>*/
/*******************************************************************************************************/
")
         (remove-string ". User inserted this text")
         (expected-string "
/*******************************************************************************/
/*  User text that existed before test. It is almost as wide as the <p>        */
/*******************************************************************************/
")
         (result-string "")
         )

      (insert start-string)
      (jump-to-p "  ")

      ;; Resume block comment
      (block-comment-start)

      ;; Kill word
      (backward-kill-word 5)

      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )
)
