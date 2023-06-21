;;; block-comment-style-detection.el --- Detect the style of a existing block comment  -*- lexical-binding: t; -*-

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
;; Funcitons used to detect the style of a existing block comment. If
;; the current block at `point' follows the block comment style, the
;; preamble, comment and postamble styles are detected if present. If
;; the detection is successfull, the style variables are set to the
;; detected style.
;;

;;; Code:

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

-> Return t if a block comment body style is found, nil otherwise."
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

Notes:
  * Assumes that the current line contains a block comment;
    behavior is undefined if it does not!
  * Assumes that there is at least one space between the prefix/postfix
    and the body

-> Return t if the style is found, else nil."
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
    (unless (block-comment--is-empty-line)

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

Notes:
  * The `point' must be on the enclosing line before calling this function!

-> Return t if enclose style is found, else nil."
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

Notes:
  * The function assumes that the `point' is on the enclosing line
    before calling it.

-> Return the prefix string if found, else nil."
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

Notes:
  * The function assumes that the `point' is on the enclosing line
    before calling it.

-> Return the prefix string if found, else nil."
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

(defun block-comment--reset-style-if-incomplete ()
  "Resets style to default if any of the style parameters is missing."

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
  "Resets style to default."
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


(provide 'block-comment-style-detection)
