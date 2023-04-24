;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

(add-to-list 'load-path "cask")
(require 'block-comment-mode)

(add-to-list 'load-path "tests")
(require 'block-comment-test-helpers)

(describe "Test user resume block comment"

  (before-each
    (erase-buffer)
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  """                     Test normal resume functionality                      """
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Resume non-centering"
    (let(
         (expected-string "
/*******************************************************************************/
/*                                                                             */<p>
/*******************************************************************************/
")
         (result-string "")
         )

      ;; Insert block comment and put point at correct position
      (insert expected-string)
      (jump-to-p nil t)
      (setq expected-string (remove-p expected-string))

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      (setq result-string (buffer-string))

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be 2)
      (expect (current-column) :to-be 4)
      )
    )

(it "Resume centering"
    (let(
         (expected-string "
/*******************************************************************************/
/*                                                                             */<p>
/*******************************************************************************/
")
         (result-string "")
         )

      ;; Insert block comment and put point at correct position
      (insert expected-string)
      (jump-to-p nil t)
      (setq expected-string (remove-p expected-string))

      ;; Insert block comment
      (block-comment-start)

      ;; Set centering to t
      (block-comment-toggle-centering)

      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      (setq result-string (buffer-string))

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-equal 2)
      (expect (current-column) :to-equal 40)
      )
    )

(it "Resume with comment text"
    (let(
         (expected-string "
/*******************************************************************************/
/*                 has comment text                                            */<p>
/*******************************************************************************/
")
         (result-string "")
         )

      ;; Insert block comment and put point at correct position
      (insert expected-string)
      (jump-to-p nil t)
      (setq expected-string (remove-p expected-string))

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      (setq result-string (buffer-string))

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be 2)
      (expect (current-column) :to-be 35)
      )
    )

(it "Resume with width alignment"
    (let(
         (expected-string "
/*******************************************************************************/
/*                                                                             */
/*******************************************************************************/
")
         (start-string "
/**************************************************/
/*                                                             */<p>
/*******************************************************************/
")
         (result-string "")
         )

      ;; Insert block comment and put point at correct position
      (insert start-string)
      (jump-to-p nil t)

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)
      (setq result-string (buffer-string))

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be 2)
      (expect (current-column) :to-be 4)
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                           Test Resume positions                           """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Resume from inside comment"
    (let(
         (expected-string "
/*******************************************************************************/
/*                                      <p>                                    */
/*******************************************************************************/
")
         (result-string "")
         )

      ;; Insert block comment and put point at correct position
      (insert expected-string)
      (jump-to-p "   " t)
      (setq expected-string (replace-p expected-string "   "))

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      (setq result-string (buffer-string))

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be 2)
      (expect (current-column) :to-be 4)
      )
    )

(it "Resume from beginning-of-line"
    (let(
         (expected-string "
/*******************************************************************************/
<p>/*                                                                             */
/*******************************************************************************/
")
         (result-string "")
         )

      ;; Insert block comment and put point at correct position
      (insert expected-string)
      (jump-to-p nil t)
      (setq expected-string (remove-p expected-string))

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      (setq result-string (buffer-string))

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be 2)
      (expect (current-column) :to-be 4)
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                           Resume pre/post amble                           """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Resume on preamble row no space"
    (let(
         (expected-string "\
/*******************************************************************************/<p>
/*                                                                             */
/*******************************************************************************/
")
         )

      ;; Insert block comment and put point at correct position
      (insert expected-string)
      (jump-to-p nil t)
      (setq expected-string (remove-p expected-string))

      ;; Insert block comment
      (block-comment-start)
      (expect block-comment-mode :to-be nil)
      )
    )

(it "Resume on postamble row no space"
    (let(
         (expected-string "
/*******************************************************************************/
/*                                                                             */
/*******************************************************************************/<p>")
         )

      ;; Insert block comment and put point at correct position
      (insert expected-string)
      (jump-to-p nil t)
      (setq expected-string (remove-p expected-string))

      ;; Insert block comment
      (block-comment-start)
      (expect block-comment-mode :to-be nil)
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                      Test space constrained comments                      """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Resume no space beneath"
    (let(
         (expected-string "
/*******************************************************************************/
/*                                                                             */<p>
/*******************************************************************************/")
         (result-string "")
         )

      ;; Insert block comment and put point at correct position
      (insert expected-string)
      (jump-to-p nil t)
      (setq expected-string (remove-p expected-string))

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      (setq result-string (buffer-string))

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be 2)
      (expect (current-column) :to-be 4)
      )
    )

(it "Resume no space above"
    (let(
         (expected-string "\
/*******************************************************************************/
/*                                                                             */<p>
/*******************************************************************************/
")
         (result-string "")
         )

      ;; Insert block comment and put point at correct position
      (insert expected-string)
      (jump-to-p nil t)
      (setq expected-string (remove-p expected-string))

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      (setq result-string (buffer-string))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be 2)
      (expect (current-column) :to-be 4)
      )
    )

(it "Resume at top of buffer with no pre/postfix"
    (let(
         (expected-string "
/*                                                                             */
")
         (start-string "
/*                                                      */<p>
")
         (result-string "")
         )

      ;; Insert block comment and put point at correct position
      (insert start-string)
      (jump-to-p nil t)

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      ;; Clean buffer and add newline at top for better error message
      (whitespace-cleanup)

      (setq result-string (buffer-string))

      ;; Append newline at top for better error message
      (setq result-string (concat "\n" result-string))

      ;; Make strings easier to read in terminal
      (setq expected-string (make-whitespace-readable expected-string))
      (setq result-string (make-whitespace-readable result-string))

      (expect result-string :to-equal expected-string)
      (expect (line-number-at-pos) :to-be 1)
      (expect (current-column) :to-be 4)
      )
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  """                              Test edge cases                              """
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (it "Resume on line that looks almost like block comment 1"
    (let(
         (expected-string "")
         (result-string "")
         (start-pos nil)
         )

      (newline)
      (insert "** Almost looks like block comment  **<p>")
      (jump-to-p nil t)

      (setq start-pos (point-marker))
      (setq expected-string (buffer-string))

      ;; Init python block comment style
      (block-comment--init-comment-style 80
                                         "#"   " "   "#"
                                         "#"   "#"   "#")

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      (setq result-string (buffer-string))

      (expect result-string :to-equal expected-string)
      (expect (point-marker) :to-equal start-pos)
      )
    )

  (it "Resume on line that looks almost like block comment 2"
    (let(
         (expected-string "")
         (result-string "")
         (start-pos nil)
         )

      (newline)
      (insert "**  Almost looks almost like block comment **<p>")
      (jump-to-p nil t)

      (setq start-pos (point-marker))
      (setq expected-string (buffer-string))

      ;; Init python block comment style
      (block-comment--init-comment-style 80
                                         "#"   " "   "#"
                                         "#"   "#"   "#")

      ;; Insert block comment
      (block-comment-start)
      (block-comment-abort)

      (setq result-string (buffer-string))

      (expect result-string :to-equal expected-string)
      (expect (point-marker) :to-equal start-pos)
      )
    )
  )
