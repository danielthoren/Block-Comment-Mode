;;; block-comment-text-position.el --- Adjust user text position as it is inserted -*- lexical-binding: t; -*-

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
;; Contains logic that adjusts the block comment spacing when new user
;; text is inserted. There are two modes, normal and centering. In
;; normal mode, the user text is kept left aligned. This is done by
;; deleting fill characters to the right of the user text, thus
;; keeping the position of the postfix constant. In centering mode,
;; the user text is centered within the block comment body. This is
;; done by alternating which side of the user text that fill
;; characters are removed from.
;;
;; When toggling between centering and normal mode, the text is moved
;; accordingly (centered or left aligned).
;;

;;; Code:

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

Notes:
  * The function assumes that the `point' is inside the block
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

Notes:
  * This function assumes that the `point' is inside the block
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

(provide 'block-comment-text-position)
