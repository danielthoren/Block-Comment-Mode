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
        (has-prev-comment (block-comment--has-comment))
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
line is aligned to the next alignment (determined by distance). The following
alignments are available:
- Left aligned
- Center aligned
- Right aligned
- Start of previous line:s user text (if previous line contains text)
- End of previous line:s user text (if previous line contains text)"
  (interactive)
  (block-comment--align (block-comment--align-get-next))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                          Startup/shutdown logic                          """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--insert-or-resume ()
  "This function creates or resumes a block comment on the current line.

If the current line is empty, a new block comment is inserted and
the `block-comment-mode' is activated.

If the current line is not empty, and if it looks like a block comment,
the function tries to detect the style used and resume the comment using
that style.

When this fails, a message is printed, telling the user that the mode has failed."
  (let (
        (inserted nil)
        )

    (if (block-comment--detect-style)
        ;; If on block comment line, resume block comment
        (progn
          ;; Force hooks off
          (block-comment--remove-hooks)
          (block-comment--force-no-hooks)

          ;; Jump to starting position to prevent width alignment from using
          ;; rightmost cursor position when calculating target width
          (block-comment--jump-to-starting-pos)

          ;; Align width of each line in the comment
          (block-comment--align-width)
          (block-comment--jump-to-starting-pos)

          (setq inserted t)
          )
      ;; Else try to insert new comment if the current line is empty
      (if (block-comment--is-current-line-empty)
          (setq inserted (block-comment--insert-block-comment))
        ;; If not empty, print error message
        (block-comment--message "Line is not empty!")
        )
      )

    (when inserted
      (block-comment--init-line-boundries)
      (block-comment--allow-hooks)
      )

    ;; Return if insertion was successful or not
    inserted
    )
  )

(defun block-comment--default-init-variables ()
  "Init all session variables to their default values.

All variables that are local to a mode session are default initialized when
the `block-comment-mode' is started."

  (block-comment--reset-style-if-incomplete)

  (setq-local block-comment-body-start-boundry nil)
  (setq-local block-comment-body-end-boundry nil)
  (setq-local block-comment-centering--order 1)
  (setq-local block-comment-centering--enabled nil)
  (setq-local block-comment-has-hooks nil)
  (setq-local block-comment--force-no-hooks nil)
  )

(defun block-comment--init-line-boundries ()
  "Init comment boundries for the current line.

Init variables used to keep track of line boundries:
`block-comment-body-start-boundry'
`block-comment-body-end-boundry'
"
  ;; Set comment body start pos
  (save-excursion
    (block-comment--jump-to-body-start)
    (setq block-comment-body-start-boundry (point-marker))
    )

  ;; Set comment body end pos
  (save-excursion
    (block-comment--jump-to-body-end)
    (setq block-comment-body-end-boundry (point-marker))
    )
  )

(defun block-comment--set-comment-style (width
                                         prefix
                                         fill
                                         postfix
                                         enclose-prefix
                                         enclose-fill
                                         enclose-postfix
                                         &optional enclose-prefix-bot
                                         enclose-fill-bot
                                         enclose-postfix-bot)
  ;; TODO: Update doc string when sticky style has been introduced
  "Sets the comment style of the mode.

This style will be used when inserting a block comment in the
current buffer. By default, the style is variable, meaning that
the most recently resumed style will be used on the next
insertion. Thus, the style set here will only be applicable until
a pre-existing comment has been resumed.

The optional bottom enclose style parameters can be used to set a
different style for the top/bottom encloses. If not set, bot the
top and bottom encloses use the same parameters."

  (unless enclose-prefix-bot
    (setq enclose-prefix-bot enclose-prefix)
    (setq enclose-fill-bot enclose-fill)
    (setq enclose-postfix-bot enclose-postfix)
    )

  (setq-local block-comment-width width)

  (setq-local block-comment-prefix prefix)
  (setq-local block-comment-fill fill)
  (setq-local block-comment-postfix postfix)

  (setq-local block-comment-enclose-prefix-top enclose-prefix)
  (setq-local block-comment-enclose-fill-top enclose-fill)
  (setq-local block-comment-enclose-postfix-top enclose-postfix)

  (setq-local block-comment-enclose-prefix-bot enclose-prefix-bot)
  (setq-local block-comment-enclose-fill-bot enclose-fill-bot)
  (setq-local block-comment-enclose-postfix-bot enclose-postfix-bot)

  ;; Default parameters
  (setq-local block-comment-prefix-default prefix)
  (setq-local block-comment-fill-default fill)
  (setq-local block-comment-postfix-default postfix)

  (setq-local block-comment-enclose-prefix-top-default enclose-prefix)
  (setq-local block-comment-enclose-fill-top-default enclose-fill)
  (setq-local block-comment-enclose-postfix-top-default enclose-postfix)

  (setq-local block-comment-enclose-prefix-bot-default enclose-prefix-bot)
  (setq-local block-comment-enclose-fill-bot-default enclose-fill-bot)
  (setq-local block-comment-enclose-postfix-bot-default enclose-postfix-bot)

  ;; Sets the target spacing between pre/postfix and user comment
  (setq-local block-comment-edge-offset 2)
  )

(defun block-comment--force-no-hooks ()
  "Removes hooks and enables the hook override.

The hook override ensures that no hooks are re-added when calling
the function `block-comment--add-hooks'. Use the function
`block-comment--allow-hooks' to reset the override."

  (block-comment--remove-hooks)
  (setq block-comment--force-no-hooks t)
  )

(defun block-comment--allow-hooks ()
  "Reset the hook override.

Sets the hook override to nil, enabling the function
`block-comment--add-hooks' to add the hooks."

  (setq block-comment--force-no-hooks nil)
  )

(defun block-comment--remove-hooks ()
  "Removes the hooks if they are active.

If the following hooks are active, they are removed:
`post-command-hook'
`after-change-functions'
"
  (when block-comment-has-hooks
    (setq post-command-hook
          (delete #'block-comment--cursor-moved post-command-hook))
    (setq after-change-functions
          (delete #'block-comment--user-edit after-change-functions))

    ;; Keep track of hook status
    (setq block-comment-has-hooks nil)
    )
  )

(defun block-comment--add-hooks ()
  "Adds the hooks if they, and the override, are inactive.

Adds necessary hooks so that the mode can react to changes in the
buffer. This behaviour can be overridden by the function
`block-comment--force-no-hooks'. In which case, the hooks will be
forcibly disabled until the corresponding
`block-comment--allow-hooks' is called.

The following hooks are added:
`post-command-hook'
`after-change-functions'
"
  (when (and (not block-comment-has-hooks) (not block-comment--force-no-hooks))
    ;; Keep track of the cursors position.
    ;; Disable the mode if it leaves the active region.
    (add-to-list 'post-command-hook #'block-comment--cursor-moved)

    ;; Add a hook that is called everytime the buffer is modified
    (add-to-list 'after-change-functions #'block-comment--user-edit)

    ;; Keep track of hook status
    (setq block-comment-has-hooks t)
    )
  )

(defun block-comment--jump-to-starting-pos ()
  "Jump to the starting position of the current comment line.

This function assumes that the current line holds a block comment.
If there is a user text inside the block, it will always jump to the end of
the text. Otherwise, the behaviour will depend on wether centering mode is
enabled or not. If it is, then it will jump to the center of the comment body,
else to the beginning of the comment body."

  (if (block-comment--has-comment)
      (block-comment--jump-to-last-char-in-body)
    (if block-comment-centering--enabled
        (block-comment--jump-to-body-center)
      (block-comment--jump-to-body-start)
      )
    )
  )

(defun block-comment--cursor-moved ()
  "Disable mode if `point' moves outside of active boundry.

This function is triggered by `post-command-hook' every time
`point' has moved. Used to detect when `point' leaves the current
line boundry. If it does, and it is within the boundries of
another comment line, reinitialize boundries and continue the
mode. If the new position is not within a comment, disable mode."

  (let* (
         (start (marker-position block-comment-body-start-boundry))
         (end (marker-position block-comment-body-end-boundry))
         (cur (marker-position (point-marker)))
         )

    (if (or (< cur start) (< end cur))  ; If outside of line boundry
        (if (block-comment--is-body t)  ; If still in a block comment body
            (progn                      ; Set up variables for new line

              (block-comment--init-line-boundries)
              )
          (block-comment-mode 0)        ; If not on block comment body, exit centering
          )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                             Insert functions                             """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--insert-block-comment ()
  "Inserts a new block comment and puts `point' at starting pos

Inserts block comment on the following format:
enclose top: <enclose-prefix>    <enclose fill>    <enclose-postfix>
body       : <prefix> <`point'>  <fill>            <postfix>
enclose bot: <enclose-prefix>    <enclose fill>    <enclose-postfix>"

  (block-comment--reset-style-if-incomplete)

  (let (
        (max-viable-column (- block-comment-width
                              (+
                               (string-width block-comment-prefix)
                               (string-width block-comment-postfix)
                               (* block-comment-edge-offset 2)
                               (/ block-comment-width 8)
                               ))
                           )
        )

    ;; Only insert comment if there is enough horizontal room
    (if (> max-viable-column (current-column))
        (progn

          ;; Insert top enclose
          (block-comment--insert-enclose-line block-comment-enclose-prefix-top
                                         block-comment-enclose-fill-top
                                         block-comment-enclose-postfix-top)

          (newline)

          ;; Insert bot enclose
          (block-comment--indent-accoring-to-previous-comment-line)
          (block-comment--insert-enclose-line block-comment-enclose-prefix-bot
                                         block-comment-enclose-fill-bot
                                         block-comment-enclose-postfix-bot)

          (beginning-of-line)
          (newline)
          (forward-line -1)

          ;; Insert body
          (block-comment--indent-accoring-to-previous-comment-line)
          (block-comment--insert-comment-line (- block-comment-width (current-column)))

          ;; Put point at start pos
          (block-comment--jump-to-starting-pos)

          t  ; return t
          )
      (progn
        (block-comment--message "Not enough room to insert comment!")
        nil  ; return nil
        )
      ) ;; end if
    )
  )

(defun block-comment--insert-newline (&optional target-width)
  "Inserts a new comment line, moving text to the right of `point' down.

Inserts a new comment line below the current line and moves the
text to the right of `point' down to the new line. `point' is
left at the body start position (beginning of comment line,
before text). The comment line boundries are initialized."
  (let (
        (remain-text-start (point-marker))
        (remain-text-end nil)
        (remain-text nil)
        )

    (block-comment--remove-hooks)

    ;; Get current block-comment width
    (unless target-width (setq target-width (block-comment--get-comment-width)))

    ;; If there is text to the right of comment
    (when (not (block-comment--is-point-right-of-comment))
      (block-comment--jump-to-last-char-in-body)
      (setq remain-text-end (point-marker))

      ;; Delete remaining text between point and end of body
      (setq remain-text (delete-and-extract-region remain-text-start
                                                   remain-text-end))

      ;; Insert the same amount of fill characters that we just removed to keep
      ;; alignment
      (insert (make-string (string-width remain-text)
                           (string-to-char block-comment-fill)))
      )

    (end-of-line)
    (insert "\n")
    (block-comment--indent-accoring-to-previous-comment-line)

    (block-comment--insert-comment-line target-width)
    (block-comment--init-line-boundries)
    (block-comment--add-hooks)
    (block-comment--jump-to-starting-pos)

    ;; If there is text to the right of `point', reinsert the deleted text
    (when remain-text
      (insert remain-text)
      (block-comment--jump-to-first-char-in-body)
      )
    )
  )

(defun block-comment--insert-comment-line (width)
"Inserts a new block comment line at `point' that is WIDTH wide."
  (let* (
         (fill-count (+ 1 (- width
                             (+ (string-width block-comment-prefix)
                                (string-width block-comment-postfix)
                                )
                             )
                        )
                     )
         )

    (save-excursion
      ;; Insert the comment body
      (insert block-comment-prefix)
      (insert (make-string fill-count (string-to-char block-comment-fill)))
      (insert block-comment-postfix)
      ) ;; End excursion
    )
  )

(defun block-comment--insert-enclose-line (prefix fill postfix)
"Inserts an enclosing line at `point'.

An enclosing line is a line inserted before and after the block comment body.

Parameters:
    * PREFIX: A string representing the prefix of the enclosing line.
    * FILL: A character representing the fill of the enclosing line.
    * POSTFIX: A string representing the postfix of the enclosing line."
  (let* (
         (target-width (+ 1 (- block-comment-width (current-column))))
         (padding-length (- target-width
                            (+ (string-width prefix)
                               (string-width postfix))))
         )

    (insert prefix)
    (insert (make-string padding-length
                         (string-to-char fill)))
    (insert postfix)
    ) ;; End let
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                             Style detection                               """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--detect-style ()
"Attempts to detect the block comment style at `point'.

This is done by first checking if the current line matches the
block comment format (see top comment).
If it does, then the prefix & postfix are detected and saved in
the following buffer-local variables:
* `block-comment-prefix'
* `block-comment-postfix'

Then the function checks for the enclosing lines by moving
up/down until the line no longer matches the detected comment
format. If both the top/bottom lines match the format of a
enclosing line, their format is detected and saved. If one, or
neither of the enclosing lines match the required format, both
are set to the empty string. The following buffer-local variables
are used to store the format:
* `block-comment-enclose-prefix-top'
* `block-comment-enclose-fill-top'
* `block-comment-enclose-postfix-top'

* `block-comment-enclose-prefix-bot'
* `block-comment-enclose-fill-bot'
* `block-comment-enclose-postfix-bot'

Returns t if a block comment body style is found, nil otherwise."
  (let (
        (body-found nil)
        (enclose-top-found nil)
        (enclose-bot-found nil)
        (lines-in-buffer (block-comment--get-line-count-in-buffer))
        (at-bottom nil)
        )

    ;;-------------------------- Detect body style ------------------------------

    ;; Detect block comment body style and set global symbols
    (setq body-found (block-comment--detect-body-style 'block-comment-prefix
                                                       'block-comment-postfix))

    ;; Only try to find enclose if there is a block comment body
    (when body-found
      ;;-------------------------- Detect enclose top style -----------------------

      (save-excursion
        ;; Move to line above block comment body
        (while (progn
                 ;; Move up one line
                 (forward-line -1)

                 ;; Continue if still in block comment body and not at top of buffer
                 (and (not (block-comment--is-at-buffer-top))
                      (block-comment--is-body nil))
                 )
          )

        ;; Detect style
        (setq enclose-top-found (block-comment--detect-enclose-style
                                 'block-comment-enclose-prefix-top
                                 'block-comment-enclose-fill-top
                                 'block-comment-enclose-postfix-top))
        )

      ;;-------------------------- Detect enclose top style -----------------------

      (save-excursion
        ;; Move to line above block comment body
        (while (progn
                 ;; Move up one line
                 (forward-line 1)

                 ;; Check if at bottom of buffer
                 (setq at-bottom (block-comment--is-at-buffer-bot lines-in-buffer))

                 ;; Continue if still in block comment body and not at bottom of buffer
                 (and (not at-bottom)
                      (block-comment--is-body nil))
                 )
          )

        ;; Detect style
        (setq enclose-bot-found (block-comment--detect-enclose-style
                                 'block-comment-enclose-prefix-bot
                                 'block-comment-enclose-fill-bot
                                 'block-comment-enclose-postfix-bot))
        )

      ;; If either enclose was not found, set both to non-existent
      (unless (and enclose-top-found
                   enclose-bot-found)

        (setq-local block-comment-enclose-prefix-top "")
        (setq-local block-comment-enclose-fill-top "")
        (setq-local block-comment-enclose-postfix-top "")

        (setq-local block-comment-enclose-prefix-bot "")
        (setq-local block-comment-enclose-fill-bot "")
        (setq-local block-comment-enclose-postfix-bot "")
        )
      )

    ;; Return t if style found, else nil
    body-found
    )
  )

(defun block-comment--detect-body-style (body-prefix-symbol
                                         body-postfix-symbol)
"Auto-detects the body style used for block comments on the current line.

The given symbols are updated with the new values if they are found.
If they are not found, they are set to an empty string.

Parameters:
    BODY-PREFIX-SYMBOL : Symbol to which the new prefix will be written.
    BODY-POSTFIX-SYMBOL: Symbol to which the new postfix will be written.

Return: t if the style is found, else nil.

Note: It is assumed that the current line contains a block comment;
behavior is undefined if it does not!

Note: Assumes that there is at least one space between the prefix/postfix
and the body"
  (let* (
         (start-pos nil)
         (end-pos nil)
         (fill-margin-pos nil)
         (prefix "")
         (prefix-fill "")
         (postfix "")
         (postfix-fill "")
         (encountered-error nil)
         )

    ;; Only try to detect if the line is not blank
    (unless (block-comment--is-blank-line)

      ;; Find postfix
      (save-excursion
        (end-of-line)
        (skip-syntax-backward " " (line-beginning-position))

        (unless (= (point) (line-beginning-position))
          (setq end-pos (point-marker))
          (skip-syntax-backward "^ " (line-beginning-position))
          (setq start-pos (point-marker))

          ;; If there is enough space remaining
          (if (< block-comment-edge-offset (- (marker-position (point-marker))
                                              (point-min)))
              (progn
                (backward-char block-comment-edge-offset)
                (setq fill-margin-pos (point-marker))

                (setq postfix (buffer-substring start-pos end-pos))
                (setq postfix-fill (buffer-substring start-pos fill-margin-pos))
                )
            (progn
              (setq encountered-error t)
              )
            )
          )
        )

      ;; Find prefix
      (save-excursion
        (beginning-of-line)
        (skip-syntax-forward " " (line-end-position))

        (unless (= (point) (line-end-position))
          (setq start-pos (point-marker))
          (skip-syntax-forward "^ " (line-end-position))
          (setq end-pos (point-marker))

          ;; If there is enough space remaining
          (if (< block-comment-edge-offset (- (point-max)
                                              (marker-position (point-marker))))
              (progn
                (forward-char block-comment-edge-offset)
                (setq fill-margin-pos (point-marker))

                (setq prefix (buffer-substring start-pos end-pos))
                (setq prefix-fill (buffer-substring end-pos fill-margin-pos))
                )
            (progn
              (setq encountered-error t)
              )
            )

          )
        )
      )

    ;; Only modify when prefix/postfix was found
    (if (and (not encountered-error)
             (> (string-width prefix) 0)
             (> (string-width postfix) 0)
             (string= prefix-fill postfix-fill))
        ;; If found, modify the given symbols and return t
        (progn
          (set body-prefix-symbol prefix)
          (set body-postfix-symbol postfix)
          t
          )
      ;; If not found, return nil
      nil
      )
    )
  )

(defun block-comment--detect-enclose-style (prefix-symbol
                                            fill-symbol
                                            postfix-symbol)
"Auto-detects the enclose style used for the enclosing line of a block comment.

Detects style using helper functions, then checks if the current line adheres
to the block comment enclose format, meaning that no non-fill characters may
 exist between the prefix/postix:
<Prefix><x * Fill><Postfix

If the style is found and adheres to the format, the given symbols are updated
with the detected values. If they are not found, they are set to an empty string.

Parameters:
    PREFIX-SYMBOL : Symbol to which the new prefix will be written.
    FILL-SYMBOL   : Symbol to which the new fill will be written.
    POSTFIX-SYMBOL: Symbol to which the new postfix will be written.

Return: t if enclose style is found, else nil.

Note: The `point' must be on the enclosing line before calling this function!"
  (let* (
         (enclose-prefix nil)
         (enclose-fill nil)
         (enclose-postfix nil)
         (enclose-found nil)
         )

    ;;-------------------------- Find fill ----------------------------------
    (setq enclose-fill (block-comment--detect-enclose-fill))

    (when enclose-fill
      ;;-------------------------- Find prefix ----------------------------------
      (setq enclose-prefix (block-comment--detect-enclose-prefix enclose-fill))

      ;;-------------------------- Find postfix ----------------------------------
      (setq enclose-postfix (block-comment--detect-enclose-postfix enclose-fill))

      ;;-------------------------- sanity check ----------------------------------

      ;; If all components were found, and is enclose returns true, set given
      ;; symbols to the found values
      (when (and enclose-prefix
                 enclose-postfix
                 (block-comment--is-enclose enclose-prefix
                                            enclose-fill
                                            enclose-postfix))

        (set prefix-symbol enclose-prefix)
        (set fill-symbol enclose-fill)
        (set postfix-symbol enclose-postfix)
        (setq enclose-found t)
        )
      )

    ;; Return t if found, else nil
    enclose-found
    )
  )

(defun block-comment--detect-enclose-fill ()
  "Detects and returns the enclose fill of the current line.

Finds and returns the char in the center of the text on the current row.
Does not check if the format of the current row matches that of a
enclose. If the current row contains no text, this function returns nil.

Return: The fill string if found, else nil"
  (let (
        (block-start nil)
        (block-end nil)
        (body-start nil)
        (body-end nil)
        (block-middle nil)
        (enclose-fill nil)
        )

    ;; Find block end
    (end-of-line)
    (skip-syntax-backward " " (line-beginning-position))
    (setq block-end (current-column))

    ;; Find block start
    (beginning-of-line)
    (skip-syntax-forward " " (line-end-position))
    (setq block-start (current-column))

    ;; If block end is less than block start, then the line is empty
    (when (and block-end block-start (> block-end block-start))
      ;; Jump to middle
      (setq block-middle (/ (- block-end block-start) 2))
      (forward-char block-middle)
      (setq enclose-fill (string (char-after)))
      )

    ;; Return fill string
    enclose-fill
    )
  )

(defun block-comment--detect-enclose-prefix (enclose-fill)
  "Detect and returns the enclose prefix of the current line.

Detects the prefix by starting in the center of the text on the
current line and skipping backwards until the first non-fill
character is found. The prefix is then set to the string between
this position and the first non-space character of the line.
If the only characters found between the center and
`beginning-of-line', then it is assumed that the prefix consists
of a single fill character. If the line is empty, then the prefix
is not found and the function returns nil.

Parameters:
    ENCLOSE-FILL: The fill string used in the enclosing line.

Return: The prefix string if found, else nil.

Note: The function assumes that the `point' is on the enclosing line before calling it."
  (let (
        (block-start nil)
        (block-end nil)
        (enclose-prefix nil)
        )
    (save-excursion
      (beginning-of-line)
      ;; Skip to the first non blank character and define this as the start of
      ;; the prefix
      (skip-syntax-forward " " (line-end-position))
      (setq block-start (point-marker))
      )
    (save-excursion
      ;; Skip backward from the middle until first non fill character was found
      (skip-chars-backward enclose-fill (line-beginning-position))
      (setq block-end (point-marker))
      )

    (if (<= block-end block-start)
        ;; If all characters from the middle to the start are the same, then
        ;; prefix is the same character as fill
        (setq enclose-prefix enclose-fill)
      ;; If not, then capture the prefix
      (progn
        (setq enclose-prefix (buffer-substring block-start block-end))
        ;; Sanity check, prefix should not be longer than 5
        (when (> (string-width enclose-prefix) 5)
          (setq enclose-prefix nil)
          )
        )
      )
    ;; Return the prefix
    enclose-prefix
    )
  )

(defun block-comment--detect-enclose-postfix (enclose-fill)
"Detects and returns the enclose postfix of the current line.

Detects the postfix by starting in the center of the text on the
current line and skipping forward until the first non-fill
character is found. The postfix is then set to the string between
this position and the first non-space character of the line.
If the only characters found between the center and
`beginning-of-line', then it is assumed that the prefix consists
of a single fill character. If the line is empty, then the prefix
is not found and the function returns nil.

Parameters:
    ENCLOSE-FILL: The fill string used in the enclosing line.

Return: The prefix string if found, else nil.

Note: The function assumes that the `point' is on the enclosing line before calling it."
  (let (
        (block-start nil)
        (block-end nil)
        (enclose-postfix nil)
        )
    (save-excursion
      (end-of-line)
      ;; Skip to the first non blank character and define this as the end of
      ;; the postfix
      (skip-syntax-backward " " (line-beginning-position))
      (setq block-end (point-marker))
      )
    (save-excursion
      ;; Skip backward from the middle until first non fill character was found
      (skip-chars-forward enclose-fill (line-end-position))
      (setq block-start (point-marker))
      )

    (if (<= block-end block-start)
        ;; If all characters from the middle to the start are the same, then
        ;; prefix is the same character as fill
        (setq enclose-postfix enclose-fill)
      ;; If not, then capture the prefix
      (progn
        (setq enclose-postfix (buffer-substring block-start block-end))
        ;; Sanity check, prefix should not be longer than 5
        (when (> (string-width enclose-postfix) 5)
          (setq enclose-postfix nil)
          )
        )
      )
    ;; Return postfix string
    enclose-postfix
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                             Centering logic                              """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--user-edit (begin end length)
  "Adds or removes fill characters to keep the commend width constant.

This function is triggered by the hook `after-change-functions'
every time the user has inserted/removed characters. The BEGIN &
END are used to see if characters have been added or removed.

;; Details

If characters are inserted:
It removes fill characters on the appropriate side of the user
text to keep the width of the comment constant. If centering is
disabled, characters are only removed from the right side of the
text. Otherwise, the removal alternates between the left/right
side to keep the text centered. If there is no space left in the
active region, the comment width is increased to make room fo the
text.

If characters are removed:
It adds fill characters on the appropriate side of the user text
to keep the width of the comment constant. If centering is
disabled, characters are only added from the right side of the
text. Otherwise, the addition alternates between the left/right
side to keep the text centered. If the comment is extended, its
width is decreased such that the text fits inside the active
region. The width only decrease until the target width has been
reached.

Parameters (Provided by the hook):
    BEGIN : The start of the region where the change occured.
    END   : The end of the region where the change occured.
    LENGTH: The length in chars of the pre-change text replaced by that range."
  (let* (
         (step (- (- end begin) length))
         (min-step (/ step 2))
         (max-step (- step min-step))

         (left  (if (= block-comment-centering--order 0) max-step min-step))
         (right (if (= block-comment-centering--order 0) min-step max-step))
         )

    (if (< step 0)
        (block-comment--user-removed-chars block-comment-centering--order
                                                block-comment-centering--enabled)
      (progn
        ;; If centering is not enabled, only remove from right side
        ;; of user comment
        (unless block-comment-centering--enabled
          (setq left 0)
          (setq right step)
          )
        (block-comment--user-inserted-chars left right))
      )

    ;; Alternate between putting larger step on left/right side
    ;; if centering is enabled
    (when block-comment-centering--enabled
      (setq-local block-comment-centering--order
                  (- 1 block-comment-centering--order))
      )
    )

  ;; Re-align point if it is outside of boundry
  (block-comment--align-width)
  (block-comment--align-point)
  )

(defun block-comment--user-removed-chars (curr-side centering)
  "Handle width and centering when characters has been removed.

If the comment width is below the target width (`block-comment-width'),
then fill characters are inserted until the target width is restored.

If centering is enabled, then the insertion of fill characters is
alternated between the beginning and end of the body. If not, all
fill characters are inserted at the end of the body.

Parameters:
    CURR-SIDE: The side that should receive the largest fill count:
               0: Left side (start of body)
               1: Right side (end of body)
    CENTERING: A boolean value indicating whether the block comment is centered or not.

Note: The function assumes that the `point' is inside the block
      comment before calling it."
  (save-excursion

    (let* (
           ;; Get position of right hand side of comment
           (comment-end-pos (progn
                              (block-comment--jump-to-comment-end 0)
                              (current-column)))
           ;; Get the removed width
           (removed-width (- block-comment-width
                             comment-end-pos))
           )

      (while (> removed-width 0)
        ;; Alternate between right and left side
        (if (= curr-side 0)
            (block-comment--jump-to-body-start 0)
          (block-comment--jump-to-body-end 0)
          )
        ;; Insert the fill and substract fill from removed-width
        (insert block-comment-fill)
        (setq removed-width (- removed-width
                               (string-width block-comment-fill)
                               )
              )
        ;; Only alternate if centering is enabled
        (when centering
          (setq curr-side (- 1 curr-side))
          )
        )
      )
    )
  )

(defun block-comment--user-inserted-chars (left right)
  "Handle width and centering when characters has been inserted.

The function removes fill characters to keep the width of the
comment constant. If centering is enabled, the removal is
alternated between the beginning & end of the body. Else, all
characters are removed from the end.

If the user's comment grows larger than the target width, it
stops removing characters. This makes the width of the comment
increase to make room for the text.

Parameters:
  LEFT : The number of characters to remove from the left side.
  RIGHT: The number of characters to remove from the right side.

Note: This function assumes that the `point' is inside the block
      comment before calling it."
  (let (
        (remain-space-left 0)
        (remain-space-right 0)
        (edge-offset block-comment-edge-offset)
        )

    ;; Make edge offset 1 char larger when centering to make it easier to
    ;; differentiate between the modes
    (when block-comment-centering--enabled
      (setq edge-offset (+ edge-offset 1))
      )

    (save-excursion

      ;; Get space remaining on right
      (save-excursion
        (setq remain-space-right
              (block-comment--jump-to-last-char-in-body)
              )
        )

      ;; Get space remaining on left
      (save-excursion
        (setq remain-space-left
              (block-comment--jump-to-first-char-in-body)
              )
        )

      ;;;;;;;;;;;;;;;;;;;;;;;;;;; Left side ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; If no space on left side, perform operation on right side instead
      (when (<= remain-space-left left)
        (setq right (+ right left))
        (setq left 0)
        )

      ;; Remove characters at beginning of line
      (block-comment--jump-to-body-start 0)
      (delete-char left)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;; Right side ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Remove/add characters at end of line
      (block-comment--jump-to-body-end 0)

      (if (<= remain-space-right right)
          ;; If there is no space remaining, make more space
          (progn
            (insert (make-string right
                                 (string-to-char block-comment-fill))
                    )
            ;; Update end of block comment to avoid aborting block comment mode
            (setq block-comment-body-end-boundry (point-marker))
            )
        ;; If there is space left, remove the right portion
        (delete-char (- right))
        )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                         Text alignment functions                         """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--align (next-alignment)
"Aligns the text on the current row to the NEXT-ALIGNMENT.

Alignment is performed by adjusting the left-most position of the
text within the comment. The position of `point' relative to the
text is preserved during alignment.

The following alignments are valid (accepted symbol on the left):
  `start'      : Aligns the text to the start of the comment.
  `prev-start' : Aligns the text with the beginning of the previous line's text block.
  `prev-end'   : Aligns the text with the end of the previous line's text block.
  `end'        : Aligns the text with the end of the comment body.
  `center'     : Aligns the text to the center of the comment body.

Parameters:
  NEXT-ALIGNMENT: The next alignment to move the comment text to.

Note: This function assumes that the `point' is inside the block
      comment before calling it."
  (block-comment--remove-hooks)
  (let (
        (comment-text-start nil)        ; Start of comment text
        (comment-text-end nil)          ; End of comment text
        (point-start-pos (point-marker))

        (relative-text-position nil)    ; Points relative position inside comment text from the left
        (comment-text nil)
        (comment-text-width nil)
        (remain-fill nil)
        )

    (when (block-comment--has-comment)  ; Align text if there is any

      (block-comment--jump-to-last-char-in-body)
      (setq comment-text-end (point-marker))

      (block-comment--jump-to-first-char-in-body)
      (setq comment-text-start (point-marker))

      ;; Save relative position
      (setq relative-text-position (- point-start-pos (point-marker)))

      ;; Extract text body
      (setq comment-text (delete-and-extract-region comment-text-start
                                                    comment-text-end))
      )

    (cond ((equal :start next-alignment)
           (block-comment--jump-to-body-start))
          ((equal :prev-start next-alignment)
           (block-comment--jump-to-previous-text-column))
          ((equal :prev-end next-alignment)
           (block-comment--jump-to-previous-text-column t))
          ((equal :end next-alignment)
           (block-comment--jump-to-body-end))
          ((equal :center next-alignment)
           (block-comment--jump-to-body-center)))

    (when comment-text
      (insert comment-text)

      ;; Restore relative position if there is any
      (when relative-text-position

        ;; Make sure the relative text position does not put point
        ;; outside of the block comment. If it does, change the position
        (if (> relative-text-position 0)
            (progn ; Position positive (to the right
              (setq comment-text-width (string-width comment-text))
              (setq remain-fill (- (+ (block-comment--jump-to-last-char-in-body)
                                      comment-text-width)
                                   block-comment-edge-offset))
              ;; If there is not enough space left, reduce relative position
              (when (> relative-text-position remain-fill)
                (setq relative-text-position remain-fill))
              )
          (progn ; Position negative (to the left)
            (setq remain-fill (- (block-comment--jump-to-first-char-in-body)
                                 block-comment-edge-offset))
            ;; When there is not enough space left, reduce relative position
            (when (> (abs relative-text-position) remain-fill)
              (setq relative-text-position remain-fill))
            )
          )
        )

      (block-comment--jump-to-first-char-in-body)
      (forward-char relative-text-position)
      )
    )
  (block-comment--add-hooks)
  )

  ;; Variables used by function 'block-comment--align-get-next' that
  ;; needs to be dynamically bound since they are inserted into a list
  ;; then sorted using a lambda. Lexical scoping makes this necessary
  ;; when passing symbols into the lambda.
  (defvar-local body-start-distance nil
    "The distance from the start of the block comment body and the start of the user text")
  (defvar-local prev-text-start-distance nil
    "The distance from the start of the user text on the previous line, and the current line")
  (defvar-local prev-text-end-distance nil
    "The distance from the end of the user text on the previous line, and the current line")
  (defvar-local body-end-distance nil
    "The distance from the end of the block comment body and the start of the user text")
  (defvar-local body-center-distance nil
    "The distance from the center of the block comment body and the start of the user text")

(defun block-comment--align-get-next ()
  "Gets the next alignment on the current comment row.

The following alignments are available:
  `start'      : Aligns the text to the start of the comment.
  `prev-start' : Aligns the text with the beginning of the previous line's text block.
  `prev-end'   : Aligns the text with the end of the previous line's text block.
  `end'        : Aligns the text with the end of the comment body.
  `center'     : Aligns the text to the center of the comment body.

Return: One of the symbols defined above"

  (let (
        (text-start nil)         ; Start of comment text
        (text-end nil)           ; End of comment text
        (body-start nil)         ; The body start position
        (prev-text-start nil)    ; Start of the comment text on the previous line
        (body-center nil)        ; Body center position
        (prev-text-end nil)      ; End of the comment text on the previous line
        (body-end nil)           ; Body end position
        (text-center nil)        ; The center of the comment text
        )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;; Set positions ;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Set current line text boundries
    (let ((boundry (block-comment--get-text-boundry)))
      (setq text-start (if (car boundry) (car boundry) (current-column)))
      (setq text-end (if (cdr boundry) (cdr boundry) (current-column)))
      )

    ;; Set prev line text boundries
    (save-excursion
      (forward-line -1)
      (let ((prev-boundry (block-comment--get-text-boundry)))

        (when (setq prev-text-start (car (block-comment--get-text-boundry)))
          (setq-local prev-text-start-distance (- prev-text-start text-start)))

        (when (setq prev-text-end (cdr (block-comment--get-text-boundry)))
          (setq-local prev-text-end-distance (- prev-text-end text-start)))
        )
      )

    ;; Set body start/center/end positions
    (setq body-start
          (block-comment--get-column-from-marker
           block-comment-body-start-boundry))

    (setq body-end
          (block-comment--get-column-from-marker
           block-comment-body-end-boundry))

    (save-excursion
      (block-comment--jump-to-body-center)
      (setq body-center (current-column))
      )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;; Set distances ;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (setq-local body-start-distance (- body-start text-start))
    (setq-local body-end-distance (- body-end text-start))

    ;; The offset from 'text-start' to the center of the comment text
    (setq text-center (if (= text-start text-end)
                          0
                        (ceiling (- text-end text-start) 2)))

    ;; Distance from text center to body center
    (setq-local body-center-distance (- body-center
                                        (+ text-start
                                           text-center)))

    ;;;;;;;;;;;;;;;;;;;;; Calculate next alignment ;;;;;;;;;;;;;;;;;;;;;

    (let* (
           (distances-list (list (cons 'body-start-distance :start)
                                 (cons 'body-center-distance :center)
                                 (cons 'body-end-distance :end)))
           (curr-elem 0)
           )

      ;; Only add elements depending on previous line if there is a previous comment line
      (when (and prev-text-start-distance prev-text-end-distance)
        (push (cons 'prev-text-start-distance :prev-start) distances-list)
        (push (cons 'prev-text-end-distance :prev-end) distances-list))

      ;; Sort by distance
      (setq distances-list (sort distances-list
                                 (lambda (a b)
                                   (< (symbol-value (car a)) (symbol-value (car b))))))

      ;; Iterate until first distance larger than 0 that can fit
      ;; inside of the body is found, or until the end of the list.
      (while (and (< curr-elem (- (length distances-list) 1))
                  (or (>= 0 (symbol-value (car (nth curr-elem distances-list))))
                      (< (- body-end (+ text-end (symbol-value (car (nth curr-elem distances-list))))) 0 )
                      )
                  )
        (setq curr-elem (+ curr-elem 1))
        )

      ;; When text is end-aligned, wrap around to start aligned
      (when (and (= curr-elem (- (length distances-list) 1))
                 (>= text-end body-end))
        (setq curr-elem 0)
        )

      ;; If no text on current line, when reaching end position the
      ;; curr elem will be nil. Wrap around fixed here
      (unless (nth curr-elem distances-list)
        (setq curr-elem 0)
        )

      (cdr (nth curr-elem distances-list))
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                       Width alignment functions                          """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--align-width ()
"Adjusts the width of each comment line to match the widest comment line.

The function ensures that the width is not less than the target
width minus the indentation. If it is, the target width is
adjusted accordingly."
  (let* (
         (indentation (block-comment--get-indent-level))
         (rightmost-text-column (block-comment--get-rightmost-comment-text-column))
         (point-column (current-column))
         (rightmost-column (max point-column rightmost-text-column))

         (target-width (+ (- rightmost-column indentation)
                          block-comment-edge-offset
                          (string-width block-comment-prefix))
                       )
         )

    ;; Dont make width less than target minus indentation
    (when (< target-width (- block-comment-width indentation))
      (setq target-width (- block-comment-width indentation))
      )

    ;; Remove hooks to disable centering when adjusting width
    (block-comment--remove-hooks)

    (save-excursion
      (block-comment--jump-to-last-comment-line)
      (block-comment--adjust-lines-above-to-target-width target-width)
      )

    ;; Re-enable hooks
    (block-comment--add-hooks)
    )
  )

(defun block-comment--adjust-lines-above-to-target-width (target-width)
"Aligns width of all block comment lines from the current row upwards.

Aligns width of all block comment lines from the current row
upwards to the given TARGET-WIDTH. The function continues the
alignment process until it reaches the top of the buffer or
encounters a line that is not part of the comment body or
enclose.

Parameters:
    TARGET-WIDTH: The width to align the rows to."
  (let (
        (curr-width 0)
        (width-diff 0)
        (is-body nil)
        (is-enclose-top nil)
        (is-enclose-bot nil)
        (at-top nil)
        )

    ;; Align all block comment lines above
    (while (progn

             ;; Check if this is body or enclose
             (setq is-body (block-comment--is-body nil))
             (setq is-enclose-top (block-comment--is-enclose-top nil))
             (setq is-enclose-bot (block-comment--is-enclose-bot nil))

             ;; Exit if not in body or if at top of buffer
             (and (not at-top)
                  (or is-body is-enclose-top is-enclose-bot))
             )

      (setq curr-width (block-comment--get-comment-width))
      (setq width-diff (- target-width curr-width))

      ;; When normal block comment line
      (save-excursion
        (cond (is-body
               (block-comment--align-body-width width-diff
                                                block-comment-fill))

              ;; else if: enclose-top
              (is-enclose-top
               (block-comment--align-enclose-width width-diff
                                                   block-comment-enclose-fill-top))

              ;; else if: enclose-bot
              (is-enclose-bot
               (block-comment--align-enclose-width width-diff
                                                   block-comment-enclose-fill-bot))
              ))

      ;; Check if at top after performing logic in order to process the top
      ;; line before breaking the while loop
      (setq at-top (block-comment--is-at-buffer-top))

      ;; Move up one line
      (block-comment--move-line -1)

      ) ;; end while
    )
  )

(defun block-comment--align-body-width (width-diff fill)
  """  Changes the block comment width  'width-diff' characters, inserting     """
  """  if the diff is positive and removing if it is positive.                 """
  """  If inserting, then inserts half at beginning, and half at the end of    """
  """  comment body. Takes the centering mode indo consideration.              """
  """  Param 'width-diff': How much the width should change, increases if      """
  """                      positive, decreases if negative                     """
  """  Param 'fill'      : The char to fill with                               """
  """  OBS: This function assumes that the block comment body fits inside the  """
  """  new boundry!                                                            """

  (if (< width-diff 0)
      (block-comment--align-body-width-decrease width-diff)
    (block-comment--align-body-width-increase width-diff fill)
    )
  )

(defun block-comment--align-body-width-increase (increase fill)
  """  Increases the block comment body width with 'increase' number of       """
  """   fill characters.                                                      """
  """  Param 'increase': How much the width should change, increases if       """
  """                    positive, decreases if negative                      """
  """  Param 'fill'    : The char to fill with                                """

  (let* (
         (step (abs increase))
         (min-step (/ step 2))
         (max-step (- step min-step))

         (left  (if (= block-comment-centering--order 0) max-step min-step))
         (right (if (= block-comment-centering--order 0) min-step max-step))
         )

    ;; When not centering, only add to the right
    (unless (block-comment--is-centering-line)
      (setq right (+ left right))
      (setq left 0)
      ) ;; End unless

    (block-comment--jump-to-body-start 0)
    (insert (make-string left
                         (string-to-char fill)))

    (block-comment--jump-to-body-end 0)
    (insert (make-string right
                         (string-to-char fill)))
    )
  )

(defun block-comment--align-body-width-decrease (decrease)
  """  Decrease the block comment body width with 'decrease' amount           """
  """  Param 'decrease': How much the width should change, increases if      """
  """                    positive, decreases if negative                      """
  """  Param 'fill'    : The char to fill with                                """

  (let* (
         (step (abs decrease))
         (min-step (/ step 2))
         (max-step (- step min-step))

         (left  0)
         (right 0)
         )

    (if (block-comment--is-centering-line)
        (progn
          ;; When centering, move text to center to avoid truncating text
          (block-comment--align :center)
          ;; Remove hooks again since function above adds them
          (block-comment--remove-hooks)
          ;; Take centering order into consideration
          (setq left  (if (= block-comment-centering--order 1) max-step min-step))
          (setq right (if (= block-comment-centering--order 1) min-step max-step))
          )
      ;; When not centering, only remove from the right if possible
      (let (
            (remain-right (block-comment--jump-to-last-char-in-body 0))
            )
        ;; Try to remove as many characters as
        ;; possible from the right side to keep the formatting
        (setq right (if (> remain-right step) step remain-right))
        (setq left (- step right))
        )
      )

    (block-comment--jump-to-body-start 0)
    (delete-char left)

    (block-comment--jump-to-body-end 0)
    (delete-char (- right))
    )
  )

(defun block-comment--align-enclose-width (width-diff fill)
  """  Changes the block comment width  'width-diff' characters, inserting     """
  """  if the diff is positive and removing if it is positive.                 """
  """  Param 'width-diff': How much the width should change, increases if      """
  """                      positive, decreases if negative                     """
  """  Param 'fill'      : The char to fill with                               """
  """  OBS: This function assumes that the block comment body fits inside the  """
  """  new boundry!                                                            """
  (let* (
         (step (abs width-diff))
         (min-step (/ step 2))
         (max-step (- step min-step))
         )
    (block-comment--jump-to-body-center)

    ;; If width should increase
    (when (> width-diff 0)
      (insert (make-string step
                           (string-to-char fill)
                           )
              )
      ) ;; End when width-diff positive

    (when (< width-diff 0)
      (delete-char min-step)
      (delete-char (- max-step))
      )
    ) ;; End when width-diff negative
  )

;; TODO: Add user tests for this function
;; NOTE: Changed recently, but not tested. Reset using git if new
;; version does not work
(defun block-comment--align-point ()
  """  If point is outside of the comment bounds after width alignment, put  """
  """  point inside of the bounds                                            """
  (let (
        (curr-pos (point-marker))
        )

    (when (> curr-pos block-comment-body-end-boundry)
      (backward-char (- curr-pos block-comment-body-end-boundry)))
    (when (< curr-pos block-comment-body-start-boundry)
      (forward-char (- block-comment-body-start-boundry curr-pos)))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                             Helper functions                             """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--reset-style-if-incomplete ()
  """  Resets style to default if any of the style parameters is missing      """

  (when (or (string= block-comment-prefix "")
            (not block-comment-prefix)
            (string= block-comment-fill "")
            (not block-comment-fill)
            (string= block-comment-postfix "")
            (not block-comment-postfix)
            (string= block-comment-enclose-prefix-top "")
            (not block-comment-enclose-prefix-top)
            (string= block-comment-enclose-fill-top "")
            (not block-comment-enclose-fill-top)
            (string= block-comment-enclose-postfix-top "")
            (not block-comment-enclose-postfix-top)
            (string= block-comment-enclose-prefix-bot "")
            (not block-comment-enclose-prefix-bot)
            (string= block-comment-enclose-fill-bot "")
            (not block-comment-enclose-fill-bot)
            (string= block-comment-enclose-postfix-bot "")
            (not block-comment-enclose-postfix-bot)
            )
    (block-comment--reset-style)
    )
  )

(defun block-comment--reset-style ()
    (setq-local block-comment-prefix block-comment-prefix-default)
    (setq-local block-comment-fill block-comment-fill-default)
    (setq-local block-comment-postfix block-comment-postfix-default)
    (setq-local block-comment-enclose-prefix-top block-comment-enclose-prefix-top-default)
    (setq-local block-comment-enclose-fill-top block-comment-enclose-fill-top-default)
    (setq-local block-comment-enclose-postfix-top block-comment-enclose-postfix-top-default)
    (setq-local block-comment-enclose-prefix-bot block-comment-enclose-prefix-bot-default)
    (setq-local block-comment-enclose-fill-bot block-comment-enclose-fill-bot-default)
    (setq-local block-comment-enclose-postfix-bot block-comment-enclose-postfix-bot-default)
  )

(defun block-comment--indent-accoring-to-previous-comment-line ()
  """  Indent current line in accordance with the block comment line on           """
  """  the previous line. Auto detects if previous line is body or enclose-top  """
  """  -> return: Current indentation level                                     """
  (block-comment--move-line -1)

  (let* (
         (prefix (block-comment--get-line-prefix))
         (indent-level (block-comment--get-indent-level prefix))
         )

    (block-comment--move-line 1)

    (beginning-of-line)
    (insert (make-string indent-level
                         (string-to-char " ")
                         )
            )

    ;; Return indent level
    indent-level
    )
  )

(defun block-comment--get-indent-level (&optional prefix)
  """  Get the indentation level (int) of the current comment line                       """
  """  Param 'prefix' : The prefix to look for                                  """
  """                   Default: block-comment-prefix                           """
  """           return: Current indentation level                               """
  (unless prefix (setq prefix block-comment-prefix))

  (save-excursion
    (block-comment--jump-to-comment-start prefix)
    (current-column))
  )


(defun block-comment--is-centering-line (&optional tolerance)
  """  Checks if the current block comment line is centering or non-centering.    """
  """  If the left margin is larger than (edge-offset + 1) and the diff          """
  """  between the margins is less than tolerance,                               """
  """  then is centering                                                         """
  """  Param 'tolerance': How much off center the text is allowed to be          """
  """                     -> Default = 2                                         """
  """  -> Return: t if text is centered, else nil                                """
  (unless tolerance (setq tolerance 2))

  (save-excursion
    (let (
          (begin-width (block-comment--jump-to-first-char-in-body))
          (end-width (block-comment--jump-to-last-char-in-body 0))
          )
      ;; If diff between begin/end width is smaller than x, then assume
      ;; that we are in centering mode
      (and (> begin-width (+ block-comment-edge-offset 1))
           (> tolerance
              (abs (- begin-width end-width)))
           )
      )
    )
  )

(defun block-comment--is-point-right-of-comment ()
  """ Returns t if current point if right of block comment text               """
  (save-excursion
    (let (
          (current-pos (current-column))
          (text-end (progn
                      (block-comment--jump-to-last-char-in-body)
                      (current-column)
                      ))
          )
      (>= current-pos text-end)
      )
    )
  )

(defun block-comment--move-line (count)
  """  Moves point 'count' lines up/down, keeping the column position.         """
  """  Param 'count':                                                          """
  """                 +x -> move point x lines down                            """
  """                 -x -> move point x lines up                              """
  (let (
        (column (current-column))
        )
    (forward-line count)
    (move-to-column column)
    )
  )

(defun block-comment--has-comment ()
  """ Checks if the block-comment-body at point contains a user comment """
  """ If it does, then return t, else nil                               """
  (let (
        (body-end nil)
        )
    (save-excursion
      (setq body-end (block-comment--jump-to-body-end))
      (block-comment--jump-to-body-start)
      (skip-syntax-forward " " body-end)
      (not (equal (point-marker)
                  body-end
                  )
           )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                            Get functions                                  """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--get-line-count-in-buffer ()
  """  Gets the number of lines in the current buffer                        """
  """  -> Return: The number of lines in the current buffer                  """
  (count-lines (point-min) (point-max))
  )

(defun block-comment--get-widest-comment-text ()
  """  Finds the width of the widest block comment text above point and        """
  """  returns said width. The block comment text is the actual user text      """
  """  inside the block comment body.                                          """
  (let (
        (widest-width 0)
        (curr-width 0)
        (at-top nil)
        )

    (save-excursion
      ;; Jump to the last non-postfix line in the block comment
      (block-comment--jump-to-last-comment-line 0 t)

      ;; Get widest comment text
      (while (and (not at-top)
                  (block-comment--is-body nil))

        (setq curr-width (block-comment--get-comment-text-width))
        (when (> curr-width widest-width)
          (setq widest-width curr-width)
          )

        ;; Check if at top after performing logic in order to process the top
        ;; line before breaking the while loop
        (setq at-top (block-comment--is-at-buffer-top))

        ;; Move up one line
        (forward-line -1)
        )
      )

    ;; Return widest width
    widest-width
    ) ;; End let
  )

(defun block-comment--get-rightmost-comment-text-column ()
  """  Gets the column of the non-fill character farthest to the right in  """
  """  the current block comment.                                          """
  (let (
        (rightmost-column 0)
        (curr-column 0)
        (at-top nil)
        )

    (save-excursion
      ;; Jump to the last non-postfix line in the block comment
      (block-comment--jump-to-last-comment-line 0 t)

      ;; Get right most text column position
      (while (and (not at-top)
                  (block-comment--is-body nil))

        (block-comment--jump-to-last-char-in-body 0)
        (setq curr-column (current-column))
        (when (> curr-column rightmost-column)
          (setq rightmost-column curr-column)
          )

        ;; Check if at top after performing logic in order to process the top
        ;; line before breaking the while loop
        (setq at-top (block-comment--is-at-buffer-top))

        ;; Move up one line
        (forward-line -1)
        )
      )

    ;; Return rightmost column
    rightmost-column
    ) ;; End let
  )

(defun block-comment--get-comment-width ()
  """  Returns the width of the block comment line at point, meaning the        """
  """  entire width of the block comment, from first char of prefix, to last   """
  """  char of postfix. This function works on both a comment line, and         """
  """  a pre/post amble line                                                    """

  (let* (
         (comment-start 0)
         (comment-end 0)
         )

    (save-excursion
      (setq comment-start (block-comment--jump-to-comment-start))
      (setq comment-end (block-comment--jump-to-comment-end 0))
      )

    (- comment-end comment-start)
    )
  )

(defun block-comment--get-body-width (&optional prefix postfix)
  """  Returns the width of the block comment body at point, meaning the       """
  """  width between the pre/post fix                                          """
  """  Param 'prefix' : The prefix to look for                                 """
  """                   Default: block-comment-prefix                          """
  """  Param 'postfix' : The postfix to look for                               """
  """                    Default: block-comment-postfix                        """

  (unless prefix (setq prefix block-comment-prefix))
  (unless postfix (setq postfix block-comment-postfix))

  (let (
        (comment-start 0)
        (comment-end 0)
        )

    (save-excursion
      (setq comment-start (block-comment--jump-to-body-start block-comment-edge-offset
                                                             prefix))
      (setq comment-end (block-comment--jump-to-body-end block-comment-edge-offset
                                                         postfix))
      )
    (- comment-end comment-start)
    )
  )

(defun block-comment--get-comment-text-width ()
  """  Gets the width of the actual text within the block comment              """
  (let (
        (text-start 0)
        (text-end 0)
        )

    (save-excursion
      ;; Jump to first text column position
      (block-comment--jump-to-first-char-in-body)
      (setq text-start (current-column))

      ;; Jump to last text column position, no offset
      (block-comment--jump-to-last-char-in-body 0)
      (setq text-end (current-column))
      ) ;; End save-excursion

    ;; Return text width
    (- text-end text-start)
    ) ;; End let
  )

(defun block-comment--get-line-prefix-postfix ()
  """  Gets the prefix & postfix based on the line type at point.             """
  """  Ret: The (prefix, postfix) of the line type at point as a cons-cell    """
  (let (
        (prefix nil)
        (postfix nil)
        )
    ;; Select the current lines pre/postfix
    (cond ((block-comment--is-body)
           (progn
             (setq prefix block-comment-prefix)
             (setq postfix block-comment-postfix))
           )
          ((block-comment--is-enclose-top)
           (progn
             (setq prefix block-comment-enclose-prefix-top)
             (setq postfix block-comment-enclose-postfix-top))
           )
          ((block-comment--is-enclose-bot)
           (progn
             (setq prefix block-comment-enclose-prefix-bot)
             (setq postfix block-comment-enclose-postfix-bot))
           )
          )

    (cons prefix postfix)
    )
  )

(defun block-comment--get-line-prefix ()
  (car (block-comment--get-line-prefix-postfix))
  )

(defun block-comment--get-line-postfix ()
  (cdr (block-comment--get-line-prefix-postfix))
  )

(defun block-comment--get-column-from-marker (marker)
  (if (markerp marker)
    (save-excursion
      (goto-char marker)
      (current-column)
      )
    (error
     "block-comment--get-column-from-marker: Invalid argument, must be marker!")
    )
  )

(defun block-comment--get-text-boundry ()
  "Gets the start & end position (column) of the text on the current line.

Return: Cons cell containing the start & end column positions"
  (let (
        (text-start nil)
        (text-end nil)
        )
    (if (and (block-comment--is-body)
             (block-comment--has-comment))
        (progn
          (save-excursion
            (block-comment--jump-to-last-char-in-body)
            (setq text-end (current-column))

            (block-comment--jump-to-first-char-in-body)
            (setq text-start (current-column))))
      )

    (cons text-start text-end)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                              Is x functions                               """
"""     -> Functions that check if current line is block comment of type x    """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--is-at-buffer-top ()
  """  Checks if point is at the first line in the current buffer            """
  """  -> Return: t if current pos is at the first line, else nil            """
  (equal (line-number-at-pos) 1)
  )

(defun block-comment--is-at-buffer-bot (&optional lines-in-buffer)
  """  Checks if point is at the last line in the current buffer             """
  """  Param 'lines-in-buffer' : The number of lines in the current buffer.  """
  """                            Can be sent as a parameter to minimize the  """
  """                            the number of times this value needs to be  """
  """                            calculated.                                 """
  """  -> Return: t if current pos is at the last line, else nil             """
  (unless lines-in-buffer (setq lines-in-buffer (block-comment--get-line-count-in-buffer)))
  (equal (line-number-at-pos) lines-in-buffer)
  )

(defun block-comment--is-current-line-empty ()
  """ Checks if current line contains any non ' ' characters                 """
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:blank:]]*$")))

(defun block-comment--is-blank-line (&optional pos)
  """  Checks if line at pos/point is emtpy, returns t if so, else nil        """
  """  Param 'pos' : The marker position to check                             """
  """                default: (point)                                         """
  """  -> Return: t if line is blank, else nil                                """
  (save-excursion
    (goto-char (or pos (point)))
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")
    )
  )

(defun block-comment--is-enclose-top (&optional inside-body)
  """  Checks if the current line follows the format of a block comment        """
  """  top enclose                                                            """
  """  Param 'inside': specifies if point is required to be inside of the     """
  """                body or not:                                             """
  """                t   -> Point must be inside the body                     """
  """                nil -> Point must be on the same line as body             """
  """                Default: nil                                             """

  (let (
        (match-signature nil)
        (has-body-beneath nil)
        )

    (setq match-signature (block-comment--is-enclose block-comment-enclose-prefix-top
                                                     block-comment-enclose-fill-top
                                                     block-comment-enclose-postfix-top
                                                     inside-body))
    (save-excursion
      (forward-line 1)
      (setq has-body-beneath (block-comment--is-body))
      )

    (and match-signature has-body-beneath)
    )
  )

(defun block-comment--is-enclose-bot (&optional inside-body)
  """  Checks if the current line follows the format of a block comment        """
  """  bottom enclose                                                         """
  """  Param 'inside': specifies if point is required to be inside of the     """
  """                body or not:                                             """
  """                t   -> Point must be inside the body                     """
  """                nil -> Point must be on the same line as body             """
  """                Default: nil                                             """
  (let (
        (match-signature nil)
        (has-body-beneath nil)
        (has-body-above nil)
        )
    (setq match-signature (block-comment--is-enclose block-comment-enclose-prefix-bot
                                                     block-comment-enclose-fill-bot
                                                     block-comment-enclose-postfix-bot
                                                     inside-body))

    (save-excursion
      (forward-line 1)
      (setq has-body-beneath (block-comment--is-body))
      )

    (save-excursion
      (forward-line -1)
      (setq has-body-above (block-comment--is-body))
      )

    (and match-signature has-body-above (not has-body-beneath))
    )
  )

(defun block-comment--is-body (&optional inside-body)
  """ Checks if the current line follows the format of a block comment body    """
  """  Param 'inside': specifies if point is required to be inside of the     """
  """                body or not:                                             """
  """                t   -> Point must be inside the body                     """
  """                nil -> Point must be on the same line as body             """
  """                Default: nil                                             """
  (unless inside-body (setq inside-body nil))

  (block-comment--is-comment block-comment-prefix
                             block-comment-fill
                             block-comment-postfix
                             inside-body))

(defun block-comment--is-enclose (prefix fill postfix &optional inside-body)
  """  Checks if the current line follows the format of a enclose                  """
  """  with the given prefix, fill and postfix.                                   """
  """  Param 'prefix' : The prefix to look for                                    """
  """  Param 'fill' : The fill to use                                             """
  """  Param 'postfix' : The postfix to look for                                  """

  (let (
        (is-comment (block-comment--is-comment prefix fill postfix inside-body))
        (is-enclose nil)
        (block-start nil)
        (block-end nil)
        (enclose-body nil)
        (enclose-body-template nil)
        (encountered-error nil)
        )
    (when is-comment
      (condition-case nil
          (save-excursion
            ;; Make sure there is only fill characters in-between prefix/postfix
            (beginning-of-line)
            (skip-syntax-forward " " (line-end-position))
            (forward-char (+ 1 (string-width prefix)))
            (setq block-start (point-marker))

            (end-of-line)
            (skip-syntax-backward " " (line-beginning-position))
            (backward-char (+ 2 (string-width postfix)))
            (setq block-end (point-marker))
            )
        ((end-of-buffer beginning-of-buffer)
         (setq encountered-error t)
         (block-comment--error "block-comment--is-enclose: Encountered end-of-buffer" "BC: end-of-buffer")
         )
        )

      (unless encountered-error
        (setq enclose-body (buffer-substring block-start block-end))
        (setq enclose-body-template (make-string (string-width enclose-body)
                                                 (string-to-char fill)))

        ;; If the entire enclose body contains the fill character,
        ;; the current line containes enclose, return t, else nil
        (when (string= enclose-body-template enclose-body)
          (setq is-enclose t)
          )
        )
      )
    ;; Return if this is enclose
    is-enclose
    )
  )

(defun block-comment--is-comment (prefix fill postfix &optional inside)
  """  Checks if the current line follows the format of a block comment body    """
  """  with the given prefix, fill and postfix.                                """
  """  Param 'prefix' : The prefix to look for                                 """
  """  Param 'fill' : The fill to use                                          """
  """  Param 'postfix' : The postfix to look for                               """
  """  Param 'inside' specifies if point is required to be inside of the       """
  """                body or not:                                              """
  """       t   -> Point must be inside the body                               """
  """       nil -> Point must be on the same line as body                       """
  (unless inside (setq inside nil))

  (let (
        (read-prefix-pos nil)   ;; Position of current line:s prefix
        (read-postfix-pos nil)  ;; Position of current line:s postfix
        (point-in-body t)       ;; If point is inside body.
        )
    ;; Check if prefix is present on this line
    (save-excursion
      (beginning-of-line)
      (setq read-prefix-pos
            (search-forward
             (concat prefix fill)
             (line-end-position)
             t)))

    ;; Check if postfix is present on this line
    (save-excursion
      (end-of-line)
      (setq read-postfix-pos
            (search-backward
             (concat fill postfix)
             (line-beginning-position)
             t)))

    ;; If inside-body is true, check if point is inside body
    (when (and
           inside
           read-prefix-pos
           read-postfix-pos)

      (setq point-in-body (and
                           (> (point-marker) (+ read-prefix-pos block-comment-edge-offset))
                           (< (point-marker) (- read-postfix-pos block-comment-edge-offset))
                           )))

    ;; Return value, t if in block comment line, else nil
    (and read-prefix-pos
         read-postfix-pos
         point-in-body)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                         Jump to functions                                """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--jump-to-previous-text-column (&optional end)
  """  Jump to the same column as the text block in the previous block        """
  """  comment line. If param end is set to t, then jump to same column        """
  """  as the end of the text block in the previous line.                      """
  """    Param 'end': If t, jump to same column as end of text block in       """
  """                 previous line, else the start                            """

  (let* (
         (prev-block-start nil)
         (prev-comment-column nil)
         (prev-text-offset nil)
         )

    ;; Get previous text block offset from comment start
    (save-excursion
      (forward-line -1)
      (block-comment--jump-to-comment-start)
      (setq prev-block-start (current-column))

      (if end
          (block-comment--jump-to-last-char-in-body)
        (block-comment--jump-to-first-char-in-body))

      (setq prev-comment-column (current-column))
      (setq prev-text-offset (- prev-comment-column prev-block-start))
      )

    ;; Move to same position on current line
    (block-comment--jump-to-comment-start)
    (forward-char prev-text-offset)
    )
  )

(defun block-comment--jump-to-comment-start (&optional prefix)
  """  Jump to block comment start, the first char of the prefix               """
  """  Param 'prefix' : The prefix to look for                                 """
  """                   Default: block-comment-prefix                          """
  """  Ret: The position of the comment start                                  """

  (unless prefix (setq prefix (block-comment--get-line-prefix)))
  (block-comment--jump-to-body-start (- 0 (string-width prefix)) prefix)
  (point-marker)
  )

(defun block-comment--jump-to-comment-end (&optional offset postfix)
  """  Jump to block comment end, the char directly after after the postfix.    """
  """  Param 'offset': Offset can be used to move the position from the         """
  """                  default position                                         """
  """                  Default: 1                                               """
  """  Param 'postfix' : The postfix to look for                                """
  """                    Default: block-comment-postfix                         """
  """  Return: point-marker                                                     """

  (unless offset (setq offset 1))
  (unless postfix (setq postfix (block-comment--get-line-postfix)))

  (block-comment--jump-to-body-end (- 0 (+ (string-width postfix) offset)) postfix)
  (point-marker)
  )

(defun block-comment--jump-to-body-center ()
  """  Jumps to the center of the block comment body and returns the end      """
  """  final column position                                                  """
  (let (
        (start-point 0)
        (end-point 0)
        (line-width 0)
        (middle-point 0)
        )

    ;; Set line width for this line
    (save-excursion

      (block-comment--jump-to-comment-start)
      (setq start-point (current-column))

      (block-comment--jump-to-comment-end)
      (setq end-point (current-column))
      )
    (setq line-width (- end-point start-point))
    (setq middle-point (/ line-width 2))

    (when (> middle-point 0)
      (block-comment--jump-to-comment-start)
      (forward-char middle-point)
      )

    (current-column)
    )
  )

;; TODO: Use var: block-comment-body-start-boundry instead of using regex
;;       each time? Make new function that finds the boundry and use boundry
;;       here (optimization)
(defun block-comment--jump-to-body-start (&optional edge-offset prefix)
  """  Jumps to the start of block comment body                               """
  """  Param 'edge-offset': The offset from the block comment prefix          """
  """                       Default: block-comment-edge-offset                """
  """  Param 'prefix' : The prefix to look for                                """
  """                   Default: block-comment-prefix                         """
  """  Ret : The position of the body start                                   """
  (unless edge-offset (setq edge-offset block-comment-edge-offset))
  (unless prefix (setq prefix (block-comment--get-line-prefix)))

  (let (
        (start-pos (point-marker))
        (line-end (line-end-position))
        )
    (beginning-of-line)

    ;; Place point at end of prefix if a prefix is found
    (if (search-forward prefix
                        line-end
                        t)
        (forward-char edge-offset)
      (goto-char start-pos)
      )
    )
  (point-marker)
  )

(defun block-comment--jump-to-body-end (&optional edge-offset postfix)
  """  Jumps to the end of block comment body, meaning the inside of the        """
  """  block comment, excluding the pre/postfix and the edge offset.            """
  """  Param 'edge-offset': Sets a custom edge offset, meaning the distance     """
  """                       to the postfix.                                     """
  """                       Default: block-comment-edge-offset                  """
  """  Param 'postfix' : The postfix to look for                                """
  """                    Default: block-comment-postfix                         """
  """  Ret: The position of point                                               """
  (unless edge-offset (setq edge-offset block-comment-edge-offset))
  (unless postfix (setq postfix (block-comment--get-line-postfix)))

  (let (
        (start-pos (point-marker))
        (line-start (line-beginning-position))
        )
    (end-of-line)

    ;; Place point at start of postfix if a postfix is found
    (if (search-backward postfix
                         line-start
                         t)
        (backward-char (+ edge-offset 1))
      (goto-char start-pos)
      )
    )
  (point-marker)
  )

(defun block-comment--jump-to-first-char-in-body (&optional offset)
  """   Jumps to the first char in the comment body text                       """
  """   Beginning means the first non-fill character in the body               """
  """   Param: 'offset': The offset can be used to change where to jump:       """
  """                    +x -> Jump closer to postfix                          """
  """                    -x -> Jump closer to prefix                           """
  """   Ret: the number of fill characters remaining on the left side          """

  (unless offset
    (setq offset 0)
    )

  (let (
        (body-start-pos nil)   ;; Start of block-comment body
        (comment-start-pos nil);; Start of user comment
        )

    (beginning-of-line)
    ;; Find start position in block comment
    (block-comment--jump-to-body-start 0)

    ;; Set start of block-comment body
    (setq body-start-pos (current-column))

    (skip-syntax-forward " " (line-end-position))

    ;; Set start of user comment
    (setq comment-start-pos (current-column))

    (forward-char offset)

    ;; Return remaining space between user comment and start of
    ;; block-comment body
    (- comment-start-pos body-start-pos)
    )
  )

(defun block-comment--jump-to-last-char-in-body (&optional offset)
  """  jumps to end of comment in body at point End means the place right     """
  """  after the last non-fill character in the body                          """
  """  Param: 'offset': Jumps to last char in body + this offset. Default = 1 """
  """  Ret: the number of fill characters remaining on the right side         """
  (let (
        (body-end-pos nil)   ;; End of block-comment body
        (comment-end-pos nil);; End of user comment
        )
    ;; Set default value
    (unless offset
      (setq offset 1)
      )

    (end-of-line)
    ;; Find end position in block comment
    (block-comment--jump-to-body-end 0)

    ;; Set end of block-comment body
    (setq body-end-pos (current-column))

    ;; Jump back to character pos right after last char in body
    (skip-syntax-backward " " (line-beginning-position))
    ;; Jump back one more to stand on last char in body
    (backward-char 1)
    ;; Jump forward by offset
    (forward-char offset)

    ;; Set end of user comment
    (setq comment-end-pos (current-column))

    ;; Return remaining space between user comment and end of block-comment body
    (- body-end-pos comment-end-pos)
    )
  )

(defun block-comment--jump-to-last-comment-line (&optional offset stop-before-postfix)
  """  Moves point down to last block comment line.                           """
  """  Param 'offset': The offset can be used to tweak the relative          """
  """                  position that point ends on:                          """
  """                      +x -> Move point x lines further down             """
  """                      -x -> Move point x lines further up               """
  """                  Default: 0                                            """
  """  Param 'stop-before-postfix': if t, stop on last block comment line     """
  """                               (before postfix)                         """
  """                   Default: nil                                         """
  (unless offset (setq offset 0))

  (let (
        (is-body nil)
        (is-enclose-top nil)
        (is-enclose-bot nil)
        (lines-in-buffer (block-comment--get-line-count-in-buffer))
        (at-bottom nil)
        )
    ;; Move to line below bottom of block comment
    (while (progn
             ;; Move down one line
             (forward-line 1)

             ;; Check if this is body or enclose
             (setq is-body (block-comment--is-body nil))
             (setq is-enclose-top (block-comment--is-enclose-top nil))
             (unless stop-before-postfix
               (setq is-enclose-bot (block-comment--is-enclose-bot nil)))

             (setq at-bottom (block-comment--is-at-buffer-bot lines-in-buffer))

             ;; Exit if not in comment or if at bottom of buffer
             (and (not at-bottom)
                  (or is-body is-enclose-top is-enclose-bot))
             )
      )

    ;; Move back up to last comment line if necessary
    (unless at-bottom
      (forward-line -1)
      )

    )

  ;; Move according to offset
  (block-comment--move-line offset)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                               General Util                                """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--unit-tests-running ()
  (and (boundp 'block-comment--unit-tests)
       block-comment--unit-tests)
  )

(defun block-comment--message (messageStr)
  (when (not (block-comment--unit-tests-running))
    (message messageStr)
    )
  )

(defun block-comment--error (errMsg &optional errEcho)
  (when (not (block-comment--unit-tests-running))

    (unless errEcho (setq errEcho errMsg))

    ;; Format strings
    (setq errMsg (concat "Error: " errMsg))
    (setq errMsg (propertize errMsg 'face 'error))
    (setq errEcho (propertize errEcho 'face 'error))

    ;; Print echo message to Echo area in red
    (let ((message-log-max nil))
      (message errEcho)
      )

    ;; Print message to Messages buffer in red
    (with-current-buffer (messages-buffer)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert errMsg)
        )
      )
    )
  )

(provide 'block-comment-mode)

;;; block-comment-mode.el ends here
