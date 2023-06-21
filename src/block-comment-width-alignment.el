;;; block-comment-width-alignment.el --- Adjust the width of the block comment rows   -*- lexical-binding: t; -*-

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
;; Util functions used by block comment mode.
;;

;;; Code:

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
"Changes the block comment width by WIDTH-DIFF characters.

If WIDTH-DIFF is positive, FILL characters are inserted to
increase the width of the block comment. If WIDTH-DIFF is
negative, FILL characters are removed to decrease the width.

The function takes centering mode into consideration,
adding/removing characters from both sides if enabled.

Parameters:
  WIDTH-DIFF: The amount by which the width should change.
              Positive for increasing width, negative for decreasing width.
  FILL      : The character to use for filling.

Notes:
  * This function assumes that the block comment body fits
    inside the new boundary."

  (if (< width-diff 0)
      (block-comment--align-body-width-decrease width-diff)
    (block-comment--align-body-width-increase width-diff fill)
    )
  )

(defun block-comment--align-body-width-increase (increase fill)
"Increases the width of the block comment by INCREASE number of FILL characters.

Function increases the current block comment line with INCREASE
number of FILL characters. It takes centering mode into
consideration, inserting on both sides of the user text if
enabled.

Parameters:
  INCREASE: The amount by which the width should increase. Positive value.
  FILL    : The character to fill the added width with."
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
      )

    (block-comment--jump-to-body-start 0)
    (insert (make-string left
                         (string-to-char fill)))

    (block-comment--jump-to-body-end 0)
    (insert (make-string right
                         (string-to-char fill)))
    )
  )

(defun block-comment--align-body-width-decrease (decrease)
"Decreases the width of the block comment by DECREASE number of characters.

Function decreases the current block comment line with DECREASE
number of characters. It takes centering mode into
consideration, removing on both sides of the user text if
enabled.

Parameters:
  DECREASE: The amount by which the width should decrease. Positive value.
  FILL    : The character to fill the added width with."
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
"Adjusts the width of the block comment enclosure by WIDTH-DIFF characters.

This function modifies the width of the block comment enclosure
by inserting characters if WIDTH-DIFF is positive and removing
characters if it is negative.

Parameters:
  WIDTH-DIFF: The change in width, a positive value increases the width,
              a negative value decreases it.
  FILL      : Fill character used by the enclose

Notes:
  * This function assumes that the block comment body fits within the new
     boundary after the width adjustment."
  (let* (
         (step (abs width-diff))
         (min-step (/ step 2))
         (max-step (- step min-step))
         )
    (block-comment--jump-to-body-center)

    ;; If width should increase
    (when (> width-diff 0)
      (insert (make-string step
                           (string-to-char fill))))

    (when (< width-diff 0)
      (delete-char min-step)
      (delete-char (- max-step)))
    )
  )

(defun block-comment--align-point ()
  " Moves `point' inside block comment boundry.

If `point' is outside of the comment bounds after width
alignment, puts `point' inside of the bounds."
  (let (
        (curr-pos (point-marker))
        )

    (when (> curr-pos block-comment-body-end-boundry)
      (backward-char (- curr-pos block-comment-body-end-boundry)))
    (when (< curr-pos block-comment-body-start-boundry)
      (forward-char (- block-comment-body-start-boundry curr-pos)))
    )
  )

(provide 'block-comment-width-alignment)
