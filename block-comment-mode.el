;;; block-comment-mode.el --- Generate and format block comments  -*- lexical-binding: t; -*-

;; Author: Daniel Thoren <danne_thoren@hotmail.com>
;; Maintainer: Daniel Thoren <danne_thoren@hotmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; Version: 1.0

;;; Commentary:
;;
;; Block comment mode is a utility to make inserting and editing block
;; comments easier. A block comment is any text area that follows the
;; valid block comment format seen below. The PreAmble & PostAmble
;; rows are optional when resuming a block comment.
;;
;; <enclose-prefix>         <enclose-fill * X>         <enclose-postfix>  (PreAmble line)
;; <prefix>                 <fill * X>                 <postfix>          (Comment line)
;; <enclose-prefix>         <enclose-fill * X>         <enclose-postfix>  (PostAmble line)
;;
;; The following features are supported:
;; * It can insert a block comment according to the specified style
;;   with the specified width.
;; * It can resume a block comment of any style given that it follows the
;;   expected format of a block comment. When a comment is resumed,
;;   the mode automatically formats it:
;;   - Setting it to  the target width (if the user text fits within that width).
;;   - Aligns all comment lines to have equal width.
;; * It automatically extends the width of the comment when the user text
;;   within reaches the end of the comment body.
;; * It supports a 'centering' mode, where the entered user text is
;;   automatically centered.
;; * It can align the start of the user text on the current comment line
;;   to one of the following alignments:
;;   - Left aligned
;;   - Center aligned
;;   - Right aligned
;;   - Start of previous line:s user text (if previous line contains text)
;;   - End of previous line:s user text (if previous line contains text)
;;
;;;; Glossary
;;
;; The following list contains defenitions of words used througout this module:
;; * "body": The part of the block comment where the user may enter text.
;;           This may also be referred to as the "active region".
;; * "body start": The horizontal start of the body.
;; * "body end"  : The horizontal end of the body.
;;
;; * "comment"      : The whole block comment.
;; * "comment start": The horizontal start of the comment, meaning the
;;                    position where the prefix starts.
;; * "comment end"  : The horizontal end of the comment, meaning the
;;                    position where the postfix ends.
;; * "comment line" : A normal block comment line containing a body where
;;                    the user can enter text.
;;
;; * "prefix" : The set of characters at the start of a block comment line.
;; * "postfix": The set of characters at the end of a block comment line.
;; * "enclose": A specially formatted line above and/or below the normal
;;              comment lines, thus enclosing the block comment body.
;;              By default, a complete block comment has one enclose at the
;;              top (above), and one at the bot (below) of the comment.
;;              It consists of a prefix, fill characters, postfix.
;;              The enclosing lines are not mandatory, and block comments
;;              without them can be resumed normally.
;;
;; * "user text": The text that a user has entered into the block comment body.
;;                May also be referred to as "text".
;;
;;;; Unit tests
;;
;; This mode has near full test coverage on all top level features, meaning
;; the functionality that the user may interact with. The tests have been
;; written using the "Buttercup" testing framework. They can be run using
;; "Cask".

;;; Code:


(add-to-list 'load-path (expand-file-name "Block-Comment-Mode/src" user-emacs-directory))

(require 'block-comment-util)            ;; General, uncategorized, utilities
(require 'block-comment-get)             ;; Getter functions, extracting information from the buffer
(require 'block-comment-is)              ;; Boolean checks based on buffer contents
(require 'block-comment-jump-to)         ;; Jump to different positions of the block comment

(require 'block-comment-text-position)   ;; Adjust user text position as it is inserted
(require 'block-comment-text-alignment)  ;; Move user text between different alignments within the block comment body
(require 'block-comment-width-alignment) ;; Adjust the width of the block comment rows
(require 'block-comment-style-detection) ;; Detect the style of a existing block comment
(require 'block-comment-insert)          ;; Insert parts of, or whole block comment
(require 'block-comment-startup-shutdown);; Handle startup/shutdown logic

(defvar block-comment-keymap nil
  "The keymap to use for the mode.

Main goal of the keymap is to bind the following functions:
- Bind `block-comment-abort' to terminate editing (default 'C-g').
- Bind `block-comment-newline' to insert a new block comment line (default '<ret>')
- Bind `block-comment-newline-indent' to insert a new block comment line with the same
                                      user text indent as the previous line (default 'M-j')
- Bind `block-comment-toggle-centering' to toggle centering mode (default 'C-c C-c')
- Bind `block-comment-align-next' to cycle through alignments (default '<tab>')")

(unless block-comment-keymap
  (setq block-comment-keymap (make-sparse-keymap))
  (define-key block-comment-keymap (kbd "C-g") 'block-comment-abort)
  (define-key block-comment-keymap (kbd "RET") 'block-comment-newline)
  (define-key block-comment-keymap (kbd "M-j") 'block-comment-newline-indent)
  (define-key block-comment-keymap (kbd "C-c C-c") 'block-comment-toggle-centering)
  (define-key block-comment-keymap (kbd "TAB") 'block-comment-align-next)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                          Buffer local variables                           """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local block-comment-width 80
  "The target width the block comment.")
(defvar-local block-comment-prefix nil
  "The prefix string of the block comment line.")
(defvar-local block-comment-fill nil
  "The fill character of the block comment line.")
(defvar-local block-comment-postfix nil
  "The postfix string of the block comment line.")

(defvar-local block-comment-enclose-prefix-top nil
  "The prefix string of the top enclose line.")
(defvar-local block-comment-enclose-fill-top nil
  "The fill character of the top enclose line.")
(defvar-local block-comment-enclose-postfix-top nil
  "The postfix string of the top enclose line.")

(defvar-local block-comment-enclose-prefix-bot nil
  "The prefix string of the bot enclose line.")
(defvar-local block-comment-enclose-fill-bot nil
  "The fill character of the bot enclose line.")
(defvar-local block-comment-enclose-postfix-bot nil
  "The postfix string of the bot enclose line.")

(defvar-local block-comment-edge-offset 2
  "The offset between the prefix/postfix and the user text region.")

(defvar-local block-comment-body-start-boundry nil
  "Saves the start boundry of the current body.")
(defvar-local block-comment-body-end-boundry nil
  "Saves the end boundry of the current body.")
(defvar-local block-comment-centering--order 1
  "Keeps track of which side of the user text that should get the larger step (if not divisible by 2)

When the change in user text is not divisible by 2, a uneven amount of  space needs to be added/removed.
This variable keeps track of which side of the user text that should get the extra addition/subtraction.
The side is alternated every time to keep the text centered.")

(defvar-local block-comment-centering--enabled nil
  "Keeps track of the centering mode state. t if centering is enabled, else nil")

(defvar-local block-comment-has-hooks nil
  "Keeps track of the hook status. t if the hooks are active, else nil")
(defvar-local block-comment--force-no-hooks nil
  "Used to signal that hooks should not be added even though the add-hooks function is invoked")

;; Default values
(defvar-local block-comment-prefix-default nil
  "Default value for the prefix string.")
(defvar-local block-comment-fill-default " "
  "Default value for the fill character.")
(defvar-local block-comment-postfix-default nil
  "Default value for the postfix string.")

(defvar-local block-comment-enclose-prefix-top-default nil
  "Default value for the top enclose prefix string.")
(defvar-local block-comment-enclose-fill-top-default nil
  "Default value for the top enclose fill character.")
(defvar-local block-comment-enclose-postfix-top-default nil
  "Default value for the top enclose postfix string.")

(defvar-local block-comment-enclose-prefix-bot-default nil
  "Default value for the bot enclose prefix string.")
(defvar-local block-comment-enclose-fill-bot-default nil
  "Default value for the bot enclose fill character.")
(defvar-local block-comment-enclose-postfix-bot-default nil
  "Default value for the bot enclose postfix string.")

;;;###autoload
(define-minor-mode block-comment-mode
  "Block comment mode is a utility to make inserting and editing block comments easier.

If the current line is empty, the mode will insert a block comment according
to the stored style.

If the line is not empty, the mode will check if the line looks like a block
comment. If it does, the mode will automatically detect the style
of the block comment and resume it using that style. If the line does not
look like a block comment, the mode will not activate."
  :init-value nil
  :lighter "[Block-comment-mode]"
  :keymap block-comment-keymap
  (progn
    (when block-comment-mode
      (block-comment--default-init-variables)
      (if (block-comment--insert-or-resume)
          ;; Add hooks, starting mode
          (block-comment--add-hooks)
        ;; Disable mode if insertion failed
        (setq block-comment-mode nil))
      )

    (unless block-comment-mode
      (block-comment--remove-hooks))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                         Interactive functions                            """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun block-comment-start ()
  "Starts the `block-comment-mode', inserting or resuming at `point'.

If the current line is empty, the mode will insert a block comment according
to the stored style.

If the line is not empty, the mode will check if the line looks like a block
comment. If it does, the mode will automatically detect the style
of the block comment and resume it using that style. If the line does not
look like a block comment, the mode will not activate."
  (interactive)
  (block-comment-mode 1)
  )

;;;###autoload
(defun block-comment-abort ()
  "Turns off the `block-comment-mode', removing hooks and setting mode to nil."
  (interactive)
  (block-comment-mode 0)
  )

;;;###autoload
(defun block-comment-newline ()
  "Inserts a new comment line.

The text to the right of `point' is moved down to the new comment
line, just as it would with a normal text block."
  (interactive)
  (block-comment--insert-newline)
  )

;;;###autoload
(defun block-comment-newline-indent ()
  "Inserts a new comment line and indents `point' to the same column as the previous line.

The text to the right of `point' is moved down to the new line and indented, just as it
would with a normal comment when using `M-j`."
  (interactive)
  (let (
        (has-prev-comment (block-comment--is-user-comment))
        )
    (block-comment--insert-newline)
    (when has-prev-comment
      (block-comment--align :prev-start)
      )
    )
  )

;;;###autoload
(defun block-comment-toggle-centering ()
  "Toggles centering mode.

When centering is enabled, the user text on the current line is centered
automatically. When editing the text, it is kept centered while this mode
is active.

When centering is disabled, the user text is left untouched."
  (interactive)
  (if block-comment-centering--enabled
      (progn
        (setq-local block-comment-centering--enabled nil)
        (block-comment--message "BC: Centering disabled")
        )
    (progn
      (setq-local block-comment-centering--enabled t)
      (setq-local block-comment-centering--order 1)  ; Set order to right side (end of comment)
      (block-comment--align :center)
      (block-comment--jump-to-starting-pos)
      (block-comment--message "BC: Centering enabled")
      )
    )
  )

;;;###autoload
(defun block-comment-align-next ()
  "Moves the block comment text to the next alignment.

Each time this function is called, the start of the user text on the current
line is aligned to the next alignment (determined by distance).

The following alignments are available:
  `start'      : Aligns the text to the start of the comment.
  `prev-start' : Aligns the text with the beginning of the previous line's text block.
  `prev-end'   : Aligns the text with the end of the previous line's text block.
  `end'        : Aligns the text with the end of the comment body.
  `center'     : Aligns the text to the center of the comment body."
  (interactive)
  (block-comment--align (block-comment--align-get-next))
  )


(provide 'block-comment-mode)

;;; block-comment-mode.el ends here
