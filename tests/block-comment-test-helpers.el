;; Used to disable normal messages when unit tests run
(defvar block-comment--unit-tests t)
(setq block-comment--unit-tests t)

(defvar p-string "<p>")

(defun expect-buffer-equal (expected-string-with-p)
  """  Expect buffer string and pointer pos in buffer string to be equal to     """
  """  the given string and the pointer position within it                      """

  ;; Clean buffer and add newline at top for better error message
  (whitespace-cleanup)

  (let (
        (result-string (buffer-string))
        ;; Remove <p>
        (expected-string (replace-p expected-string-with-p "   "))
        )

    ;; Append newline at top for better error message
    (setq result-string (concat "\n" result-string))

    ;; Make strings easier to read in terminal
    (setq expected-string (make-whitespace-readable expected-string))
    (setq result-string (make-whitespace-readable result-string))

    (expect result-string :to-equal (replace-p expected-string "   "))

    ;; Check that position of point is correct
    (expect-point-at-p expected-string-with-p)
    )
  )

(defun jump-to-p (&optional replace-p-with delete-p)
  (interactive)
  """  Jump to the first occurance of the character '<p>'                       """
  """  Param 'replace-p-with': If given, replace the '<p>' character with this  """
  """  Param 'delete-p': If t, delete <p>                                       """
  (when replace-p-with (setq delete-p t))

  (beginning-of-buffer)
  (search-forward p-string)

  (if delete-p
      (delete-char (- (string-width p-string)))
    (insert (make-string (string-width p-string) " "))
    (backward-char (string-width p-string))
    )

  (when replace-p-with
    (insert replace-p-with)
    (backward-char (string-width replace-p-with))
    )
  )

(defun expect-point-at-p (string-with-p &optional replace-p-with)
  """  Function checks that the actual pointer is at the position marked by <p> """
  (unless replace-p-with (setq replace-p-with "   "))
  (let (
        (expected-pointer-position nil)
        (pointer-position (marker-position (point-marker)))
        )
    (with-temp-buffer
      (insert string-with-p)
      (whitespace-cleanup)
      (jump-to-p replace-p-with)
      (setq expected-pointer-position (marker-position (point-marker)))
      )

    (expect pointer-position :to-equal expected-pointer-position)
    )
  )

(defun remove-p (string-with-p)
  """  Remove the first occurance of the character '<p>'                     """
  """  Param 'string-with-p': The string to operate on                       """
  (replace-p string-with-p nil)
  )

(defun replace-p (string-with-p &optional replacement)
  """  Remove the first occurance of the character '<p>'                     """
  """  Param 'string-with-p': The string to operate on                       """
  """  Param 'replacement': If given, '<p>' is replaced by this character,   """
  """                       else <p> is removed                                """
  (unless replacement (setq replacement ""))
  (replace-regexp-in-string p-string replacement string-with-p)
  )

(defun file-to-string (file)
  """  Reads the given file and puts it into the buffer, then returns         """
  """  the buffer string                                                      """
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun make-whitespace-readable (string &optional char)
  """  Converts whitespaces of the given string to the given char and         """
  """  returns the new string.                                                """
  """  Param 'string': The string to operate on.                              """
  """  Param 'char': The char to fill whitespaces with.                       """
  """        Default -> '-'                                                   """
  (unless char (setq char "-"))

  (replace-regexp-in-string " " char string nil 'literal)
  )

(defun user-write-text (user-text)
  """  Writes one char at a time to the current buffer to simulate            """
  """  user input                                                             """
  (dotimes (i (length user-text))
    (insert (aref user-text i))
    )
  )

(defun user-remove-text (num-of-chars)
  """  Removes one char at a time from the current buffer to simulate         """
  """  user removal                                                           """
  (dotimes (i num-of-chars)
    (delete-backward-char 1)
    )
  )

(provide 'block-comment-test-helpers)
