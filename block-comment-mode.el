(provide 'block-comment-mode)

;; lägg detta i ditt mode:
;; (block-comment--init "/*" " " "*/" 80)


(defun block-comment--init (width prefix fill postfix enclose-prefix enclose-fill enclose-postfix)
  (interactive)
  (set (make-local-variable 'block-comment-width) width)

  (set (make-local-variable 'block-comment-prefix) prefix)
  (set (make-local-variable 'block-comment-fill) fill)
  (set (make-local-variable 'block-comment-postfix) postfix)

  (set (make-local-variable 'block-comment-enclose-prefix) enclose-prefix)
  (set (make-local-variable 'block-comment-enclose-fill) enclose-fill)
  (set (make-local-variable 'block-comment-enclose-postfix) enclose-postfix)

  )

(defun block-comment-centering-abort ()
  (interactive)
  (block-comment-centering-mode 0))

(defun block-comment-centering-newline ()
  (interactive)
  (block-comment-centering-abort)
  (end-of-line)
  (insert "\n")
  (block-comment-insert-centering)
  )

(define-minor-mode block-comment-centering-mode
  "Toggle block comments centering mode"
  :init-value nil
  :lighter "[centering]"
    :keymap (let ((map (make-sparse-keymap)))
            ;; press C-g to abort comment mode
            (define-key map (kbd "C-g") 'block-comment-centering-abort)
            (define-key map (kbd "RET") 'block-comment-centering-abort)
	        (define-key map (kbd "M-j") 'block-comment-centering-newline)
            map)

    (if block-comment-centering-mode
	(block-comment-centering--add-hooks)
    (block-comment-centering--shutdown)
    )
  )

(defun block-comment-centering--init ()

  (unless (boundp 'block-comment-prefix)
    (block-comment--init "/*" " " "*" "*/" 20))

  (set (make-local-variable 'block-comment-centering--start-pos) nil)
  (set (make-local-variable 'block-comment-centering--end-pos) nil)
  (set (make-local-variable 'block-comment-centering--order) 0)
  (set (make-local-variable 'block-comment-centering--left-offset) 0)
  (set (make-local-variable 'block-comment-centering--right-offset) 0)
  )

(defun block-comment-centering--shutdown ()
  (setq post-command-hook (delete #'block-comment-centering--cursor-moved post-command-hook))
  (setq after-change-functions (delete #'block-comment-centering--edit after-change-functions))
  (block-comment-centering--init)
  )

(defun block-comment-centering--add-hooks ()
  ;; Keep track of the cursors position, if it leaves the block comment
  ;; then abort the centering mode)
  (add-to-list 'post-command-hook #'block-comment-centering--cursor-moved)

  ;; Add a hook that is called everytime the buffer is modified
  (add-to-list 'after-change-functions #'block-comment-centering--edit)
  )

(defun block-comment-centering--cursor-moved ()
  """Abort block-comment-mode if cursor is outside of block comment"""
  (let* (
         (start (marker-position block-comment-centering--start-pos))
         (end (marker-position block-comment-centering--end-pos))
         (cur (point))
         )

    (if (or (< cur start) (< end cur))      ;; If outside of row boundry
        (if (block-comment--is-body t)      ;; If still in a block comment body (new line)
            (block-comment--resume nil)     ;; Run resume on new line to continue centering
          (block-comment-centering-mode 0)  ;; If not on block comment body, exit centering
          )
    )
    )
  )

(defun block-comment-centering--removed-chars (left right)
  (save-excursion

    (let* ((fill-size (string-width block-comment-fill))

	   (left-fill-count     (/ left fill-size))
	   (left-fill-remainder (% left fill-size))

	   (right-fill-count     (/ right fill-size))
	   (right-fill-remainder (% right fill-size)))

      (end-of-line)

      ;; skip the postfix
      (left-char (string-width block-comment-postfix))

      (dotimes (_ right-fill-count) (insert block-comment-fill))
      (if (> right-fill-remainder 0)
	  (insert (substring block-comment-fill right-offset right-fill-remainder)))

      (beginning-of-line)

      ;; skip the prefix
      (right-char (string-width block-comment-prefix))

      (dotimes (_ left-fill-count) (insert block-comment-fill))
      (if (> left-fill-remainder 0)
	  (insert (substring block-comment-fill left-offset left-fill-remainder)))

      (let* ((left-offset block-comment-centering--left-offset)
	     (right-offset block-comment-centering--right-offset))

	(setq block-comment-centering--left-offset
	      (% (+ left-offset left-fill-remainder) fill-size))

	(setq block-comment-centering--right-offset
	      (% (+ right-offset right-fill-remainder) fill-size))
	)
      )
    )
  )

;; TODO: Create check that restarts centering when line has reached width after getting too large
(defun block-comment-centering--inserted-chars (left right)
  (let (
        (remain-space-left 0)
        (remain-space-right 0)
        (line-width 0)
        )

    (save-excursion

      ;; Set line width for this row
      (save-excursion

      (end-of-line)
      (setq line-width (current-column))
      )

      ;; Get space remaining on right
      (save-excursion
        (setq remain-space-right
              (block-comment--jump-to-end)
              )
        )

      ;; Get space remaining on left
      (save-excursion
        (setq remain-space-left
              (block-comment--jump-to-beginning)
              )
        )

      ;;;;;;;;;;;;;;;;;;;;;;;;;;; Left side ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; If no space on left side, perform operation on right side instead
      (when (< remain-space-left 3)
        (setq right (+ right left))
        (setq left 0)
        )

      ;; Remove characters at beginning of line
      (beginning-of-line)
      (right-char (string-width block-comment-postfix)) ;; remove the left portion
      (delete-char left)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;; Right side ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Remove characters at end of line
      (end-of-line)
      (left-char (string-width block-comment-prefix))

      (if (< remain-space-right 3)
          (insert (make-string right (string-to-char " ")))  ;; If there is no space left, make more space
          (delete-backward-char right)                       ;; If there is space left, remove the right portion
          )

      )
    )
  )

(defun block-comment-centering--edit (begin end length)
  (let* ((step (- (- end begin) length))
	 (min-step (/ step 2))
	 (max-step (- step min-step))

	 (left  (if (= block-comment-centering--order 0) max-step min-step))
	 (right (if (= block-comment-centering--order 0) min-step max-step)))

    (setq block-comment-centering--order (- 1 block-comment-centering--order))

    (if (< step 0)
	(block-comment-centering--removed-chars (- 0 right) (- 0 left))
      (block-comment-centering--inserted-chars left right))
    )
  )

(defun block-comment--insert-line ()
  """ Inserts a new block comment body line and puts cursur at the center """
  (let* (
         (fill-size (string-width block-comment-fill))

         (padding-width (- block-comment-width
                           (+ (string-width block-comment-prefix)
                              (string-width block-comment-postfix))))

         ;; How many times will the fill string fit inside the padding?
         (fill-count (/ padding-width fill-size))

         ;; How many characters of the fill string needs to be inserted to keep it balanced?
         (fill-remainder (% padding-width fill-size))

         (fill-left-count (/ fill-count 2))
         (fill-right-count (- fill-count fill-left-count))
     )

    (insert block-comment-prefix)

    ;; insert the left padding
    (dotimes (_ fill-left-count) (insert block-comment-fill))

    ;; This is the point where we want the cursor to end up
    (save-excursion
      ;; insert the right padding
      (dotimes (_ fill-right-count) (insert block-comment-fill))

      (if (> fill-remainder 0)
	  (insert (substring block-comment-fill 0 fill-remainder)))

      (insert block-comment-postfix)

      ;; store the end of the block comment
      (point-marker)
      )
    )
  )

(defun block-comment--insert-start-end-row ()
  """ Inserts a enclosing line at point """
  """ A enclosing line is a line inserted before and after the block comment body """

  (let* (
         (padding-length (- block-comment-width
                            (+ (string-width block-comment-enclose-prefix)
                               (string-width block-comment-enclose-postfix)
                               )
                            )
                         )
         (padding (make-string padding-length (string-to-char block-comment-enclose-fill)))
         )
    (insert block-comment-enclose-prefix)
    (insert padding)
    (insert block-comment-enclose-postfix)
    )
  )

(defun block-comment-insert-centering ()
  """ Inserts a block comment body line at point and initializes centering """
  """ Puts point at the center of the line                                 """
  (interactive)

  ;; init the centering mode without activating it
  (block-comment-centering--init)

  ;; go to the current lines start
  (beginning-of-line)

  ;; store the beginning of the block comment
  (setq block-comment-centering--start-pos (point-marker))
  (setq block-comment-centering--end-pos (block-comment--insert-line))

  (save-excursion
    (goto-char (marker-position block-comment-centering--end-pos))
    )

  ;; enter centering mode
  (block-comment-centering-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                             Helper functions                             """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--jump-to-beginning ()
  """ jumps to beginning of comment in body at point                   """
  """ Beginning means the first non-fill character in the body         """
  """ return: the number of fill characters remaining on the left side """
  (let (
        (body-start-pos nil)   ;; Start of block-comment body
        (comment-start-pos nil);; Start of user comment
        )

    (beginning-of-line)
    ;; Find start position in block comment
    (forward-char (string-width block-comment-prefix))

    ;; Set start of block-comment body
    (setq body-start-pos (point))

    (skip-syntax-forward " ")

    ;; Set start of user comment
    (setq comment-start-pos (point))

    ;; Return remaining space between user comment and start of block-comment body
    (- comment-start-pos body-start-pos)
    )
  )

(defun block-comment--jump-to-end ()
  """ jumps to end of comment in body at point                          """
  """ End means the last non-fill character in the body                 """
  """ return: the number of fill characters remaining on the right side """

  (let (
        (body-end-pos nil)   ;; End of block-comment body
        (comment-end-pos nil);; End of user comment
        )

    (end-of-line)
    ;; Find end position in block comment
    (backward-char (string-width block-comment-postfix))

    ;; Set end of block-comment body
    (setq body-end-pos (point))

    (skip-syntax-backward " ")

    ;; Set end of user comment
    (setq comment-end-pos (point))

    ;; Return remaining space between user comment and end of block-comment body
    (- body-end-pos comment-end-pos)
    )

  )


(defun block-comment--is-body (&optional inside-body)
  """ checks if the current row follows the format of a block comment body               """
  """ Param 'inside-body' specifies if point is required to be inside of the body or not """
  """       t   -> Point must be inside the body                                         """
  """       nil -> Point must be on the same row as body                                 """

  (let (
        (line-width 0)
        (read-prefix-pos nil)   ;; Position of current row:s prefix
        (read-postfix-pos nil)  ;; Position of current row:s postfix
        (point-in-body t)       ;; If point is inside body.
        )

    ;; Set line width for this row
    (save-excursion

      (end-of-line)
      (setq line-width (current-column))
      )

    ;; Check if prefix is present on this row
    (save-excursion
      (beginning-of-line)
      (setq read-prefix-pos
            (search-forward
             (concat block-comment-prefix " ")
             (line-end-position)
             t
             )
            )
      )

    ;; Check if postfix is present on this row
    (save-excursion

      (end-of-line)
      (setq read-postfix-pos
            (search-backward
             (concat " " block-comment-postfix)
             (line-beginning-position)
             t)
            )
      )

    ;; If inside-body is true, check if point is inside body
    (when (and
           read-prefix-pos
           read-postfix-pos
           inside-body)

      (setq point-in-body (and
                           (> (point) read-prefix-pos)
                           (< (point) read-postfix-pos)
                           )
            )
      )


    ;; Return value, t if in block comment row, else nil
    (and read-prefix-pos
         read-postfix-pos
         (> line-width (- block-comment-width 20))
         point-in-body
         )
    )

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                          Interactive functions                           """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment-insert ()
  """ Inserts a new block comment and init centering """
  (interactive)

  ;; go to the current lines start
  (beginning-of-line)

  ;; Start block comment
  (block-comment--insert-start-end-row)
  (insert "\n")

  (save-excursion
    (insert "\n")
    (block-comment--insert-start-end-row)
    )

  (block-comment-insert-centering)
  )


(defun block-comment--resume (&optional jump-back)
  """ Resumes block comment mode using existing block comment """
  """ If 'jump-back' is t, jumps to end of comment inside block """
  (interactive)

  (save-excursion
    ;; init the centering mode without activating it
    (block-comment-centering--init)

    ;; store the beginning of the block comment
    (beginning-of-line)
    (forward-char (+ 1 (string-width block-comment-prefix)))
    (setq block-comment-centering--start-pos (point-marker))

    (end-of-line)
    (backward-char (+ 1 (string-width block-comment-postfix)))
    (setq block-comment-centering--end-pos (point-marker))

    )

  ;; Jump to end of comment inside block
  (when jump-back
    (block-comment--jump-to-end)
    )

  ;; enter centering mode
  (block-comment-centering-mode 1)

  )

(defun block-comment-insert-or-resume ()
  """ Checks if point is inside block comment or not. If it is, resume previous block comment, else start new block comment """
  (interactive)

  ;;Check if in block comment
  (if (block-comment--is-body nil)
      (block-comment--resume t) ;; If t, resume with jump back condition
      (block-comment-insert)    ;; Else insert
  )
  )
