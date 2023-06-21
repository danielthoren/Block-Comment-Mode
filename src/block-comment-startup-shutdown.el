;;; block-comment-startup-shutdown.el --- Startup, shutdown and hook logic   -*- lexical-binding: t; -*-

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
;; Hansle startup and shutdown logic for the mode. This includes
;; management of hooks used to run the mode.
;;
;; When the mode is started, a check is performed to see if the
;; current position in the buffer contain a block comment already.
;; If it does, then that comment is resumed. Otherwise a new block
;; comment is inserted.
;;
;; A hook is added on startup that is called each time the user
;; adds/removes text to/from the buffer. This hook is used to
;; manage the user text within the buffer, and to check if the cursor
;; moves out of the block kcomment. If the cursor leaves the comment,
;; the mode is disabled.
;;

;;; Code:

(defun block-comment--insert-or-resume ()
  "This function creates or resumes a block comment on the current line.

If the current line is empty, a new block comment is inserted and
the `block-comment-mode' is activated.

If the current line is not empty, and if it looks like a block comment,
the function tries to detect the style used and resume the comment using
that style.

When this fails, a message is printed, telling the user that the mode has failed.

-> Return t if insertion was successful, else nil"
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
      (if (block-comment--is-empty-line)
          (setq inserted (block-comment--insert-block-comment))
        ;; If not empty, print error message
        (block-comment--message "Line is not empty!")
        )
      )

    (when inserted
      (block-comment--init-line-boundries)
      (block-comment--allow-hooks)
      )

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
`block-comment-body-end-boundry'"

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
`after-change-functions'"

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
`after-change-functions'"

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

(provide 'block-comment-startup-shutdown)