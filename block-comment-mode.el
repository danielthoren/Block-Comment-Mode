(provide 'block-comment-mode)

;; lägg detta i ditt mode:
;; (block-comment--init "/*" " " "*/" 80)


(defun block-comment--init (prefix fill padding postfix width)
  (interactive)
  (set (make-local-variable 'block-comment-prefix) prefix)
  (set (make-local-variable 'block-comment-fill) fill)
  (set (make-local-variable 'block-comment-padding) padding)
  (set (make-local-variable 'block-comment-postfix) postfix)
  (set (make-local-variable 'block-comment-width) width)
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
        (if (block-comment--is-body)        ;; If still in a block comment body (new line)
            (block-comment--resume)         ;; Run resume on new line to continue centering
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

(defun block-comment-centering--inserted-chars (left right)
  (save-excursion
    (end-of-line)
    ;; the number of characters to skip at start
    (left-char (string-width block-comment-prefix))
    (delete-backward-char right) ;; remove the right portion
    (beginning-of-line)

    ;; the number of characters to skip at end
    (right-char (string-width block-comment-postfix)) ;; remove the left portion
    (delete-char left)
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
    ;; The idea is to insert the prefix and postifx,
  ;; and use the fill to insert padding between them so that
  ;; the total line size is equal to block-comment-width
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
  ;; Inset start/end of block comment

  (let* (
         (padding-length (- block-comment-width
                            (+ (string-width block-comment-prefix) (string-width block-comment-postfix))
                            )
                         )
         (padding (make-string padding-length (string-to-char block-comment-padding)))
         )
    (insert block-comment-prefix)
    (insert padding)
    (insert block-comment-postfix)
    )
  )

(defun block-comment-insert-centering ()
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

(defun block-comment-insert ()
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

(defun block-comment--resume ()
  """ Resumes block comment mode using existing block comment """
  (interactive)

  ;; init the centering mode without activating it
  (block-comment-centering--init)

  ;; store the beginning of the block comment
  (beginning-of-line)
  (forward-char (+ 1 (string-width block-comment-prefix)))
  (setq block-comment-centering--start-pos (point-marker)) ;;TODO: Maybe take as arguments

  (end-of-line)
  (backward-char (+ 1 (string-width block-comment-postfix)))
  (setq block-comment-centering--end-pos (point-marker))

  (end-of-line)
  ;; Find start position in block comment
  (backward-char (string-width block-comment-postfix))

  (skip-syntax-backward " ")

  ;; enter centering mode
  (block-comment-centering-mode 1)

  )


(defun block-comment--is-body ()
  """ checks if the current row follows the format of a block comment body """
  (interactive)
  (let (
        (line-width 0)
        (read-prefix-pos nil)   ;; Position of current row:s prefix
        (read-postfix-pos nil)  ;; Position of current row:s postfix
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

    ;; (message "Is body: %s [prefix: %s, postfix: %s width: %s]"
    ;;          (and read-prefix-pos
    ;;               read-postfix-pos
    ;;               (> line-width (- block-comment-width 20))
    ;;               )
    ;;          read-prefix-pos
    ;;          read-postfix-pos
    ;;          (> line-width (- block-comment-width 20))
    ;;          )

    ;; Return value, t if in block comment row, else nil
    (and read-prefix-pos
         read-postfix-pos
         (> line-width (- block-comment-width 20))
         )
    )

  )


(defun block-comment-insert-or-resume ()
  """ Checks if point is inside block comment or not. If it is, resume previous block comment, else start new block comment """
  (interactive)

  ;;Check if in block comment
  (if (block-comment--is-body)
      (block-comment--resume)   ;; If t, resume
      (block-comment-insert)    ;; Else insert
  )
  )
