;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

(add-to-list 'load-path "cask")
(require 'block-comment-mode)

(add-to-list 'load-path "tests")
(require 'block-comment-test-helpers)

(describe "Test resume block comment"

  (before-each
    (erase-buffer)
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  """                     Test normal resume functionality                      """
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Test resume non-centering"
    (let(
         (expected-string "
/*******************************************************************************/
/*                                                                             */
/*******************************************************************************/
")
         (result-string "")
         )

      ;; Insert block comment and put point at correct position
      (insert expected-string)
      (forward-line -2)

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

(it "Test resume centering"
    (let(
         (expected-string "
/*******************************************************************************/
/*                                                                             */
/*******************************************************************************/
")
         (result-string "")
         )

      ;; Insert block comment and put point at correct position
      (insert expected-string)
      (forward-line -2)

      ;; Set centering to t
      (block-comment-toggle-centering)
      (expect block-comment-centering-enabled :to-be t)

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
      (expect (line-number-at-pos) :to-equal 2)
      (expect (current-column) :to-equal 40)

      (block-comment-toggle-centering)
      )
    )

(it "Test resume with comment text"
    (let(
         (expected-string "
/*******************************************************************************/
/*                 has comment text                                            */
/*******************************************************************************/
")
         (result-string "")
         )

      ;; Insert block comment and put point at correct position
      (insert expected-string)
      (forward-line -2)

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

(it "Test resume with width alignment"
    (let(
         (expected-string "
/*******************************************************************************/
/*                                                                             */
/*******************************************************************************/
")
         (start-string "
/**************************************************/
/*                                                             */
/*******************************************************************/
")
         (result-string "")
         )

      ;; Insert block comment and put point at correct position
      (insert start-string)
      (forward-line -2)

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
"""  Test space constrained comments                                          """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (it "Test resume no space beneath"
;;     (let(
;;          (expected-string "
;; /*******************************************************************************/
;; /*                                                                             */
;; /*******************************************************************************/")
;;          (result-string "")
;;          )

;;       ;; Insert block comment and put point at correct position
;;       (insert expected-string)
;;       (forward-line -1)

;;       ;; Insert block comment
;;       (block-comment-start)
;;       (block-comment-abort)

;;       ;; Clean buffer and add newline at top for better error message
;;       (whitespace-cleanup)

;;       (setq result-string (buffer-string))

;;       ;; Append newline at top for better error message
;;       (setq result-string (concat "\n" result-string))

;;       ;; Make strings easier to read in terminal
;;       (setq expected-string (make-whitespace-readable expected-string))
;;       (setq result-string (make-whitespace-readable result-string))

;;       (expect result-string :to-equal expected-string)
;;       (expect (line-number-at-pos) :to-be 2)
;;       (expect (current-column) :to-be 4)
;;       )
;;     )

(it "Test resume no space above"
    (let(
         (expected-string "\
/*******************************************************************************/
/*                                                                             */
/*******************************************************************************/
")
         (result-string "")
         )

      ;; Insert block comment and put point at correct position
      (insert expected-string)
      (forward-line -2)

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

(it "Test resume at top of buffer with no pre/postfix"
    (let(
         (expected-string "
/*                                                                             */
")
         (start-string "
/*                                                      */
")
         (result-string "")
         )

      ;; Insert block comment and put point at correct position
      (insert expected-string)
      (forward-line -1)

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

  (it "Test insert on line that looks almost like block comment, type 1"
    (let(
         (expected-string "")
         (result-string "")
         (start-pos nil)
         )

      (newline)
      (insert "** Almost looks like block comment  **")

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

  (it "Test insert on line that looks like block comment, type 2"
    (let(
         (expected-string "")
         (result-string "")
         (start-pos nil)
         )

      (newline)
      (insert "**  Almost looks almost like block comment **")

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
