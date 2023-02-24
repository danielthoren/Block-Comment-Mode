
(defun file-to-string (file)
  """  Reads the given file and puts it into the buffer, then returns         """
  """  the buffer string                                                      """
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
