;;; block-comment-insert.el --- Insert parts of, or whole block comment   -*- lexical-binding: t; -*-

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
;; Functions that insert parts of, or whole block comment.
;;

;;; Code:

(defun block-comment--insert-block-comment ()
  "Inserts a new block comment and puts `point' at starting pos

Inserts block comment on the following format:
enclose top: <enclose-prefix>    <enclose fill>    <enclose-postfix>
body       : <prefix> <`point'>  <fill>            <postfix>
enclose bot: <enclose-prefix>    <enclose fill>    <enclose-postfix>

Fails if there is insufficient room to insert the comment.

-> Return t if successful, else nil"

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
before text). The comment line boundries are initialized.

-> Return nil"
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
  nil
  )

(defun block-comment--insert-comment-line (width)
"Inserts a new block comment line at `point' that is WIDTH wide.

Parameters:
  WIDTH: The width of the inserted comment line."
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

(defun block-comment--indent-accoring-to-previous-comment-line ()
"Indents the current line based on the previous comment line.

This function automatically detects whether the previous line is
a body or an enclosure-top line of the block comment. It then
indents the current line accordingly, using the same indentation
level as the previous line.

-> Returns: The current indentation level."
  (block-comment--move-line -1)

  (let* (
         (prefix (block-comment--get-line-prefix))
         (indent-level (block-comment--get-indent-level prefix))
         )

    (block-comment--move-line 1)

    (beginning-of-line)
    (insert (make-string indent-level
                         (string-to-char " ")))

    ;; Return indent level
    indent-level
    )
  )

(provide 'block-comment-insert)
