(defun file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun make-whitespace-readable (string &optional char)
  """  Converts whitespaces of the given string to the given char and returns  """
  """  the new string.                                                         """
  (unless char (setq char "-"))

  (replace-regexp-in-string " " char string nil 'literal)
  )

(provide 'block-comment-test-helpers)
