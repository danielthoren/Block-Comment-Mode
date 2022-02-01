(define-minor-mode block-comment-mode
  "Toggle Comment Mode."
  :init-value nil ; Initial value, nil for disabled
  :lighter "Block-comment"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-M-k") 'insert-header)
            ;; press C-g to abort comment mode
            (define-key map (kbd "C-g") 'block-comment-abort)
            ;; press Ret to abort comment mode and add a new line (after the centered text)
            (define-key map (kbd "RET") 'block-comment-abort-newline)
            map)

  (if block-comment-mode
      (progn
       (message "Activating block comment mode")
       (unless (boundp 'my-comment-start)
         (progn
           (message "Setting default variables")
           (block-comment-init-local-variables "|" " " "|" 80)
          )
         )
       )
    )
  )

(defun block-comment-init-local-variables (start padding end len)
  (interactive)
  (message "setting local variables")
  (set (make-local-variable 'my-comment-start) start)
  (set (make-local-variable 'my-comment-padding) padding)
  (set (make-local-variable 'my-comment-end) end)
  (set (make-local-variable 'my-comment-width) len)
  )

(defun block-comment-abort ()
  (interactive)
  (remove-hook 'after-change-functions #'block-comment-handle-input)
  (block-comment-mode 0))

(defun block-comment-abort-newline ()
  (interactive)
  (block-comment-abort)
  (end-of-line)
  (newline))

(defvar-local block-comment-order 0)

(defun block-comment-removed-chars (left right)
  (save-excursion
    (end-of-line)
    ;; the number of characters to skip at start
    (left-char 1)
    (insert (make-string right ? )) ;; insert the correct number of spaces to the right
    (beginning-of-line)
    ;; the number of characters to skip at end
    (right-char 1)
    (insert (make-string left ? )))) ;; insert the correct number of spaces to the left

(defun block-comment-inserted-chars (left right)
  (save-excursion
    (end-of-line)
    ;; the number of characters to skip at start
    (left-char 1)
    (delete-backward-char right) ;; remove the right portion
    (beginning-of-line)
    ;; the number of characters to skip at end
    (right-char 1) ;; remove the left portion
    (delete-char left)))


(defun block-comment-handle-input (beg end len)
  (let* ((step (- (- end beg) len)) ;; how many characters differance is there after the edit?
         (lostep (/ step 2)) ;; split into the lower...
         (histep (- step lostep)) ;; and upper half (lostep and histep are equal if step is divisible by 2)

         ;; how many spaces to remove from the left and right side (negative means we add spaces instead)
         (left  (if (= block-comment-order 0) histep lostep))
         (right (if (= block-comment-order 0) lostep histep)))

    ;; next time we order it the otherway around (so that the remainder is switching from left to right side)
    (setq-local block-comment-order (- 1 block-comment-order))

    ;; if step is negative then we have removed chars, so then we add spaces
    ;; otherwise we remove spaces because there where chars added
    (if (< step 0)
        (block-comment-removed-chars (- 0 left) (- 0 right))
      (block-comment-inserted-chars left right))))

(defun insert-header-start ()
  (add-hook 'after-change-functions #'block-comment-handle-input)
  (block-comment-mode t))

(defun insert-header ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (message "Padding is: %s" my-comment-padding)
  (let* (
         (padding-length (- my-comment-width
                            (+ (string-width my-comment-start) (string-width my-comment-end))
                            )
                         )
         (padding (make-string padding-length (string-to-char my-comment-padding)))
         (half-comment-width (/ padding-length 2))
         )
  (message "Padding length: %d" padding-length)
  (insert my-comment-start)
  (insert padding)
  (insert my-comment-end)
  (insert "\n")

  (insert my-comment-start)
  (insert (make-string half-comment-width ? ))
  (save-excursion
    (insert (make-string half-comment-width ? ))
    (insert my-comment-end)
    (insert "\n")

    (insert my-comment-start)
    (insert padding)
    (insert my-comment-end)

    (insert-header-start)
  )
  )
)

(provide 'block-comment-mode)
