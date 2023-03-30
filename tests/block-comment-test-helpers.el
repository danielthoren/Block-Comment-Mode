(defun jump-to-p (&optional replace-p-with delete-p)
  """  Jump to the first occurance of the character 'p'                       """
  """  Param 'replace-p-with': If given, replace the 'p' character with this  """
  """  Param 'replace-p-with': If t, delete p                                 """
  (when replace-p-with (setq delete-p t))

  (beginning-of-buffer)
  (search-forward "p")
  (backward-char 1)

  (when delete-p
    (delete-char 1)
    )

  (when replace-p-with
    (insert replace-p-with)
    )
  )

(defun remove-p (string-with-p)
  """  Remove the first occurance of the character 'p'                       """
  """  Param 'string-with-p': The string to operate on                       """
  (replace-p string-with-p nil)
  )

(defun replace-p (string-with-p &optional replacement)
  """  Remove the first occurance of the character 'p'                       """
  """  Param 'string-with-p': The string to operate on                       """
  """  Param 'replacement': If given, 'p' is replaced by this character,     """
  """                       else p is removed                                """
  (unless replacement (setq replacement ""))
  (replace-regexp-in-string "p" replacement string-with-p)
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

(provide 'block-comment-test-helpers)
