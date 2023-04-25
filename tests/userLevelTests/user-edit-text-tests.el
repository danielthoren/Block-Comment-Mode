;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

;; TODO: Finish last headings

(add-to-list 'load-path "cask")
(require 'block-comment-mode)

(add-to-list 'load-path "tests")
(require 'block-comment-test-helpers)

(describe "Test user edit text"

  (before-each
    (erase-buffer)
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                               Entering text                               """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (it "Enter text"
    (let(
         (start-string "
/*******************************************************************************/
/*                                                                             */<p>
/*******************************************************************************/
")
         (insert-string "User inserted this text ")
         (expected-string "
/*******************************************************************************/
/*  User inserted this text <p>                                                */
/*******************************************************************************/
")
         (result-string "")
         )

      (insert start-string)
      (jump-to-p nil t)

      ;; Resume block comment
      (block-comment-start)
      (user-write-text insert-string)
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )

(it "Enter text by paste"
    (let(
         (start-string "
/*******************************************************************************/
/*                                                                             */<p>
/*******************************************************************************/
")
         (insert-string "User inserted this text ")
         (expected-string "
/*******************************************************************************/
/*  User inserted this text <p>                                                */
/*******************************************************************************/
")
         (result-string "")
         )

      (insert start-string)
      (jump-to-p nil t)

      ;; Resume block comment
      (block-comment-start)

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
/*  User inserted this text <p>                                                */
/*******************************************************************************/
")
         (remove-string "inserted this text")
         (expected-string "
/*******************************************************************************/
/*  User <p>                                                                   */
/*******************************************************************************/
")
         (result-string "")
         )

      (insert start-string)
      (jump-to-p nil t)

      ;; Resume block comment
      (block-comment-start)
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
/*  User inserted this text <p>                                                */
/*******************************************************************************/
")
         (remove-string "inserted this text")
         (expected-string "
/*******************************************************************************/
/*  User <p>                                                                   */
/*******************************************************************************/
")
         (result-string "")
         )

      (insert start-string)
      (jump-to-p nil t)

      ;; Resume block comment
      (block-comment-start)

      ;; Kill word
      (backward-kill-word 3)

      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                         Entering text with no room                        """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Enter text with no horizontal room"
    (let(
         (insert-string " User inserted this text")
         (start-string "
/******************************************************************************/
/*                                    User text that existed before test. <p> */
/******************************************************************************/
")
         (expected-string "
/****************************************************************************************************/
/*                                    User text that existed before test. User inserted this text<p>*/
/****************************************************************************************************/
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

(it "Enter text by paste with no horizontal room"
    (let(
         (insert-string " User inserted this text")
         (start-string "
/******************************************************************************/
/*                                    User text that existed before test. <p> */
/******************************************************************************/
")
         (expected-string "
/****************************************************************************************************/
/*                                    User text that existed before test. User inserted this text<p>*/
/****************************************************************************************************/
")
         (result-string "")
         )

      (insert start-string)
      (jump-to-p "  ")

      ;; Resume block comment
      (block-comment-start)

      ;; Insert text all at once
      (insert insert-string)

      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string)
      )
    )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                     Removing text with expanded comment                   """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

)
