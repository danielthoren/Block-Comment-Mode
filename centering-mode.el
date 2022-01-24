(define-minor-mode centering-mode
  "Toggle Centering Mode."
  :init-value nil ; Initial value, nil for disabled
  :lighter "Block-comment"
  :keymap nil

  ((message "in the place")
   (if (and centering-mode (boundp 'my-comment-start))
      (
       (message "Setting default variables")
       (set (make-local-variable 'my-comment-start) "|")
       (set (make-local-variable 'my-comment-padding) " " )
       (set (make-local-variable 'my-comment-end) "|" )
       (set (make-local-variable 'my-comment-width) 80)
       )
    )
   )
  )

(defun init-local-variables (start end padding len)
  (message "setting local variables")
  (set (make-local-variable 'my-comment-start) start)
  (set (make-local-variable 'my-comment-padding) padding)
  (set (make-local-variable 'my-comment-end) end)
  (set (make-local-variable 'my-comment-width) len)
  )

(local-set-key (kbd "C-M-k") 'insert-header)

;; press C-g to abort centering mode
(local-set-key (kbd "C-g") 'centering-abort)

;; press Ret to abort centering mode and add a new line (after the centered text)
(local-set-key (kbd "RET") 'centering-abort-newline)

(defun centering-abort ()
  (interactive)
  (remove-hook 'after-change-functions #'centering-handle-input)
  (centering-mode 0))

(defun centering-abort-newline ()
  (interactive)
  (centering-abort)
  (end-of-line)
  (newline))

(defvar-local centering-order 0)

(defun centering-removed-chars (left right)
  (save-excursion
    (end-of-line)
    ;; the number of characters to skip at start
    (left-char 1)
    (insert (make-string right ? )) ;; insert the correct number of spaces to the right
    (beginning-of-line)
    ;; the number of characters to skip at end
    (right-char 1)
    (insert (make-string left ? )))) ;; insert the correct number of spaces to the left

(defun centering-inserted-chars (left right)
  (save-excursion
    (end-of-line)
    ;; the number of characters to skip at start
    (left-char 1)
    (delete-backward-char right) ;; remove the right portion
    (beginning-of-line)
    ;; the number of characters to skip at end
    (right-char 1) ;; remove the left portion
    (delete-char left)))


(defun centering-handle-input (beg end len)
  (let* ((step (- (- end beg) len)) ;; how many characters differance is there after the edit?
         (lostep (/ step 2)) ;; split into the lower...
         (histep (- step lostep)) ;; and upper half (lostep and histep are equal if step is divisible by 2)

         ;; how many spaces to remove from the left and right side (negative means we add spaces instead)
         (left  (if (= centering-order 0) histep lostep))
         (right (if (= centering-order 0) lostep histep)))

    ;; next time we order it the otherway around (so that the remainder is switching from left to right side)
    (setq-local centering-order (- 1 centering-order))

    ;; if step is negative then we have removed chars, so then we add spaces
    ;; otherwise we remove spaces because there where chars added
    (if (< step 0)
        (centering-removed-chars (- 0 left) (- 0 right))
      (centering-inserted-chars left right))))

(defun insert-header-start ()
  (add-hook 'after-change-functions #'centering-handle-input)
  (centering-mode t))

(defun insert-header ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (insert my-comment-start)
  (insert (make-string 'my-comment-width ? ))
  (save-excursion
    (insert (make-string 'my-comment-width ? ))
    (insert 'my-comment-end)
    (insert 'header-start)))

(provide 'centering-mode)
