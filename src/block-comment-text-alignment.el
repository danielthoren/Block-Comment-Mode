;;; block-comment-text-alignment.el --- Move user text between different alignments within the block comment body   -*- lexical-binding: t; -*-

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
;; Functions that align the user text within the block comment body.
;; The following alignments are valid:
;;   `start'      : Aligns the text to the start of the comment.
;;   `prev-start' : Aligns the text with the beginning of the previous line's text block.
;;   `prev-end'   : Aligns the text with the end of the previous line's text block.
;;   `end'        : Aligns the text with the end of the comment body.
;;   `center'     : Aligns the text to the center of the comment body.
;;

;;; Code:

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

Notes:
  * This function assumes that the `point' is inside the block
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

    (when (block-comment--is-user-comment)  ; Align text if there is any

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

-> Return one of the alignment symbols defined above"

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

(provide 'block-comment-text-alignment)
