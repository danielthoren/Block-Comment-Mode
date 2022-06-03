;; FIXME: Bug when align-text is called. If text has been removed prior to
;;        invoking this function, it is reinserted either at the beginning
;;        of the text, or end

;; TODO: Make all rows extend when one row extends in width
;;       Make function that does this

;; TODO: Add toggling between different lengths of block comments

;; TODO: Implement automatic block comment width detection

;; TODO: Add automatic row breaking when block comment is longer
;;       than 80 characters

;; TODO: Add auto format on M-q

(provide 'block-comment-mode)

(define-minor-mode block-comment-mode
  "Toggle block comments mode"
  :init-value nil
  :lighter "[Block-comment-mode]"
    :keymap (let ((map (make-sparse-keymap)))
            ;; press C-g to abort comment mode
            (define-key map (kbd "C-g") 'block-comment-abort)
            (define-key map (kbd "RET") 'block-comment-abort)
            (define-key map (kbd "M-j") 'block-comment-newline)
            (define-key map (kbd "C-c C-c") 'block-comment-toggle-centering)
            map)

    (if block-comment-mode
    (block-comment--add-hooks)
    (block-comment--shutdown)
    )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                         Functions bound to keys                          """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment-abort ()
  """ Turns block-comment-mode off """
  (interactive)
  (block-comment-mode 0))

(defun block-comment-newline ()
  """ Inserts a new line and moves text to the right of point down"""
  (interactive)
  (let (
        (remain-text-start (point-marker))
        (remain-text-end nil)
        (indent-level 0)
        )

    (save-excursion
      (block-comment--jump-to-comment-start)
      (setq indent-level (current-column))
      )

    ;; Kill remaining text between point and end of body
    (block-comment--jump-to-last-char-in-body)
    (setq remain-text-end (point-marker))
    (kill-region remain-text-start remain-text-end)

    (block-comment-abort)
    (end-of-line)
    (insert "\n")
    (block-comment--insert-new-line indent-level)

    (yank)
    )
  )

(defun block-comment-toggle-centering ()
  """ Toggles centering mode """
  (interactive)
  (if block-comment-centering-enabled
      (setq block-comment-centering-enabled nil) ;; If enabled , disable
    (setq block-comment-centering-enabled t)     ;; If disabled, enabled
    )

  ;; Set order to right side (end of comment)
  (unless block-comment-centering-enabled
    (setq block-comment-centering--order 1)
    )
  ;; Align text
  (block-comment--align-text block-comment-centering-enabled)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                          Interactive functions                           """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--insert-or-resume ()
  """ This function is called to create or resume a block comment           """
  """ Checks if point is inside block comment or not.                       """
  """ If it is, resumeprevious block comment, else start new block comment  """
  (interactive)

  ;;Check if in block comment
  (if (block-comment--is-body nil)
      (block-comment--resume t)  ;; If t, resume with jump back condition
    (block-comment--insert)      ;; Else insert
    )
  )

(defun block-comment--init-comment-style (
                                          width
                                          prefix
                                          fill
                                          postfix
                                          enclose-prefix
                                          enclose-fill
                                          enclose-postfix)
  """ Initializes variables of block-comment-mode                           """
  """ This should be called during initialization of each mode where block- """
  """ comment-mode shall be used. Default behaviour is c/c++ comment style  """
  (interactive)
  (set (make-local-variable 'block-comment-width) width)

  (set (make-local-variable 'block-comment-prefix) prefix)
  (set (make-local-variable 'block-comment-fill) fill)
  (set (make-local-variable 'block-comment-postfix) postfix)

  (set (make-local-variable 'block-comment-enclose-prefix) enclose-prefix)
  (set (make-local-variable 'block-comment-enclose-fill) enclose-fill)
  (set (make-local-variable 'block-comment-enclose-postfix) enclose-postfix)

  ;; Used to remember if is centering or not
  (set (make-local-variable 'block-comment-centering-enabled) t)
  ;; Sets the target spacing between pre/postfix and user comment
  (set (make-local-variable 'block-comment-edge-offset) 2)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                          Startup/shutdown logic                          """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--init-variables ()
  """ Init function run when block-comment-mode is started.                   """
  """ Sets default values for variables                                       """
  (unless (boundp 'block-comment-prefix)
    (block-comment--init-comment-style 20   "/*" " " "*/"    "/*" "*" "*/" ))

  (set (make-local-variable 'block-comment-centering--start-pos) nil)
  (set (make-local-variable 'block-comment-centering--end-pos) nil)
  (set (make-local-variable 'block-comment-centering--order) 0)
  (set (make-local-variable 'block-comment-centering--left-offset) 0)
  (set (make-local-variable 'block-comment-centering--right-offset) 0)
  )

(defun block-comment--shutdown ()
  """ Turns block comment off by removing the hooks """
  (setq post-command-hook
        (delete #'block-comment-centering--cursor-moved post-command-hook))
  (setq after-change-functions
        (delete #'block-comment-centering--edit after-change-functions))
  (block-comment--init-variables)
  )

(defun block-comment--add-hooks ()
  """   Adds necessary hooks so that block-comment-mode can react to          """
  """   changes in the buffer                                                 """
  ;; Keep track of the cursors position, if it leaves the block comment
  ;; then abort the centering mode)
  (add-to-list 'post-command-hook #'block-comment-centering--cursor-moved)

  ;; Add a hook that is called everytime the buffer is modified
  (add-to-list 'after-change-functions #'block-comment-centering--edit)
  )

(defun block-comment--resume (&optional jump-back)
  """ Resumes block comment mode using existing block comment   """
  """ If 'jump-back' is t, jumps to end of comment inside block """
  """ else, inits block comment mode at point                   """

  (save-excursion
    ;; init the centering mode without activating it
    (block-comment--init-variables)

    ;; store the beginning of the block comment
    (beginning-of-line)
    (block-comment--jump-to-body-start 0)
    (backward-char 1)
    (setq block-comment-centering--start-pos (point-marker))

    (end-of-line)
    (block-comment--jump-to-body-end 0)
    (forward-char 1)
    (setq block-comment-centering--end-pos (point-marker))

    )

  ;; If there is a user comment, jump to end of said comment
  ;; If there is no user comment, jump to center if centering,
  ;;                              else jump to start
  (when jump-back
    (if (block-comment--has-comment)
        (block-comment--jump-to-last-char-in-body)
      (if block-comment-centering-enabled
          (block-comment--jump-to-body-center)
        (block-comment--jump-to-body-start)
        )
      )
    )

  ;; enter centering mode
  (block-comment-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                             Insert functions                             """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--insert ()
  """ Inserts a new block comment and init centering """

  ;; go to the current lines start
  (beginning-of-line)

  ;; Start block comment
  (block-comment--insert-start-end-row)
  (insert "\n")

  (save-excursion
    (insert "\n")
    (block-comment--insert-start-end-row)
    )

  (block-comment--insert-new-line)
  )


(defun block-comment--insert-new-line (&optional indent-level)
  """    Inserts a block comment body line below point, at the             """
  """    current indentation level and initializes centering               """

  (unless indent-level (setq indent-level   0))

  ;; init the centering mode without activating it
  (block-comment--init-variables)

  ;; Insert new line with same indent
  (block-comment--insert-line indent-level)

  ;; Set bomment body start pos
  (save-excursion
    (block-comment--jump-to-body-start 0)
    (setq block-comment-centering--start-pos (point-marker))
    )

  (save-excursion
    (block-comment--jump-to-body-end 0)
    (setq block-comment-centering--end-pos (point-marker))
    )

  ;; TODO: Why are we doing this?
  (save-excursion
    (goto-char (marker-position block-comment-centering--end-pos))
    )

  ;; Jump to center of user comment if centering enabled,
  ;; else jump to beginning of user comment
  (if block-comment-centering-enabled
      (block-comment--jump-to-body-center)
    (block-comment--jump-to-body-start)
    )

  ;; enter centering mode
  (block-comment-mode 1)
  )


(defun block-comment--insert-line (indent-level)
  """ Inserts a new block comment body line at point with 'indent-level' """
  (let* (
         (fill-size (string-width block-comment-fill))

         (padding-width (- block-comment-width
                           (+ (string-width block-comment-prefix)
                              (string-width block-comment-postfix))))

         ;; How many times will the fill string fit inside the padding?
         (fill-count (/ padding-width fill-size))

         ;; How many characters of the fill string needs to be inserted
         ;; to keep it balanced?
         (fill-remainder (% padding-width fill-size))

         (fill-left-count (/ fill-count 2))
         (fill-right-count (- fill-count fill-left-count))
         )

    (save-excursion
      (beginning-of-line)
      ;; Bring us to current indent level
      (insert (make-string indent-level (string-to-char " ")))
      ;; Insert the comment body
      (insert block-comment-prefix)
      (insert (make-string fill-count (string-to-char block-comment-fill)))
      (insert block-comment-postfix)

      ;; Return end of block comment
      (block-comment--jump-to-body-end 0)
      (point-marker)
      )
    )

  )

(defun block-comment--insert-start-end-row ()
  """ Inserts a enclosing line at point                                      """
  """ A enclosing line is a line inserted before and                         """
  """ after the block comment body                                           """

  (let* (
         (padding-length (- block-comment-width
                            (+ (string-width block-comment-enclose-prefix)
                               (string-width block-comment-enclose-postfix)
                               )
                            )
                         )
         (padding (make-string padding-length
                               (string-to-char block-comment-enclose-fill))
                  )
         )
    (insert block-comment-enclose-prefix)
    (insert padding)
    (insert block-comment-enclose-postfix)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                             Centering logic                              """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment-centering--cursor-moved ()
  """ This function is triggered by a hook every time point has moved        """
  """ Used to abort block-comment-mode if cursor is outside of block comment """
  (let* (
         (start (marker-position block-comment-centering--start-pos))
         (end (marker-position block-comment-centering--end-pos))
         (cur (point))
         )

    (if (or (< cur start) (< end cur))  ;; If outside of row boundry
        (if (block-comment--is-body t)  ;; If still in a block comment body
            (block-comment--resume nil) ;; Run resume on new line to continue
          (block-comment-mode 0)  ;; If not on block comment body, exit centering
          )
    )
    )
  )

(defun block-comment-centering--edit (begin end length)
  """   This function is triggered by a hook every time the user has         """
  """   inserted/removed characters. It checks if the user removed or added  """
  """   characters, then decides which side of the blockc omment should be   """
  """   affected. The rest of the work is delegated                          """
  (let* (
         (step (- (- end begin) length))
         (min-step (/ step 2))
         (max-step (- step min-step))

         (left  (if (= block-comment-centering--order 0) max-step min-step))
         (right (if (= block-comment-centering--order 0) min-step max-step))
         )

    (if (< step 0)
        (block-comment-centering--removed-chars block-comment-centering--order
                                                block-comment-centering-enabled)
      (progn
        ;; If centering is not enabled, only remove from right side
        ;; of user comment
        (unless block-comment-centering-enabled
          (setq left 0)
          (setq right step)
          )
        (block-comment-centering--inserted-chars left right))
      )

    ;; Alternate between putting larger step on left/right side
    ;; if centering is enabled
    (when block-comment-centering-enabled
      (setq block-comment-centering--order
            (- 1 block-comment-centering--order))
      )
    )
  )

(defun block-comment-centering--removed-chars (curr-side centering)
  """ Handles when the user removes characters. Inserts padding on right and  """
  """ left side. When user comment is wider than target width,                """
  """ no padding is inserted.                                                 """
  """ curr-side : The side that should get the largest fill count:            """
  """             0 -> left side (start of body)                              """
  """             1 -> right side (end of body)                               """
  """ centering : If t, the block comment is centered, else not               """
  (save-excursion

    (let* (
           ;; Get line width after change
           (line-width (- (block-comment--jump-to-comment-end)
                          (block-comment--jump-to-comment-start)
                          )
                       )
           ;; Get the removed width
           (removed-width (- block-comment-width
                             line-width)
                          )
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

(defun block-comment-centering--inserted-chars (left right)
  """   Handles when user inserts characters. Removes padding on right and """
  """  left side. If user comment grows larger than target width,          """
  """   stops removing characters                                          """
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
      (when (< remain-space-left block-comment-edge-offset)
        (setq right (+ right left))
        (setq left 0)
        )

      ;; Remove characters at beginning of line
      (block-comment--jump-to-body-start 0)
      (delete-char left)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;; Right side ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Remove characters at end of line
      (block-comment--jump-to-body-end 0)

      (if (< remain-space-right block-comment-edge-offset)
          ;; If there is no space left, make more space
          (progn
            (insert (make-string right
                                 (string-to-char block-comment-fill))
                    )
            ;; Update end of block comment to avoid aborting block comment mode
            (setq block-comment-centering--end-pos (point-marker))
            )
          ;; If there is space left, remove the right portion
          (delete-backward-char right)
          )

      )
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                             Helper functions                             """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun block-comment--align-text (centering)
  """   Aligns the text in the comment body, centering it if param            """
  """   'centering' is t, else aligning to the left.                          """
  """   If there is no user comment in body, put point at appropriate pos     """
  (let (
        (comment-text-start nil)
        (comment-text-end nil)
        )
    (if (block-comment--has-comment) ;; Align text if there is any
        (progn
          (block-comment--jump-to-first-char-in-body)
          (setq comment-text-start (point-marker))
          (block-comment--jump-to-last-char-in-body)
          (setq comment-text-end (point-marker))
          (kill-region comment-text-start comment-text-end)

          (if centering
              (block-comment--jump-to-body-center)
            (block-comment--jump-to-body-start)
            )

          (yank)
          )
      (if centering ;; If there is no text, move point to appropriate place
          (block-comment--jump-to-body-center)
        (block-comment--jump-to-body-start)
        )
      )
    )
  )

(defun block-comment--jump-to-body-center ()
  """ Jumps to the center of the block comment body """

  (let (
        (line-width 0)
        (middle-point 0)
        )

    ;; Set line width for this row
    (save-excursion

      (end-of-line)
      (setq line-width (current-column))
      )

    (setq middle-point (/ line-width 2))

    (beginning-of-line)
    (forward-char middle-point)

    )
  )

(defun block-comment--jump-to-body-start (&optional edge-offset)
  """ Jumps to the start of block comment body """
  (unless edge-offset (setq edge-offset block-comment-edge-offset))

  (let (
        (start-pos (point-marker))
        (line-end (line-end-position))
        )
    (beginning-of-line)
    ;; Jump back one since search forward starts searching on point + 1
    (backward-char 1)
    ;; Place point at end of prefix if a prefix is found
    (if (search-forward block-comment-prefix
                        line-end
                        t)
        (forward-char edge-offset)
      (goto-char start-pos)
      )
    )
  (point-marker)
  )

(defun block-comment--jump-to-comment-start ()
  """ Jump to block comment start, before the prefix """
  (interactive)
  (block-comment--jump-to-body-start 0)
  (backward-char (string-width block-comment-prefix))
  (point-marker)
  )

(defun block-comment--jump-to-body-end (&optional edge-offset)
  """ Jumps to the end of block comment body """
  (unless edge-offset (setq edge-offset block-comment-edge-offset))

  (let (
        (start-pos (point-marker))
        (line-start (line-beginning-position))
        )
    (end-of-line)
    ;; Jump forward one since search ba starts searching on point + 1
    (forward-char 1)
    ;; Place point at end of prefix if a prefix is found
    (if (search-backward block-comment-postfix
                        line-start
                        t)
        (backward-char edge-offset)
      (goto-char start-pos)
      )
    )
  (point-marker)
  )

(defun block-comment--jump-to-comment-end ()
  """ Jump to block comment end, after the postfix """
  (interactive)
  (block-comment--jump-to-body-end 0)
  (forward-char (string-width block-comment-postfix))
  (point-marker)
  )

(defun block-comment--jump-to-first-char-in-body ()
  """ jumps to beginning of comment in body at point                   """
  """ Beginning means the first non-fill character in the body         """
  """ return: the number of fill characters remaining on the left side """
  (let (
        (body-start-pos nil)   ;; Start of block-comment body
        (comment-start-pos nil);; Start of user comment
        )

    (beginning-of-line)
    ;; Find start position in block comment
    (block-comment--jump-to-body-start 0)

    ;; Set start of block-comment body
    (setq body-start-pos (point))

    (skip-syntax-forward " ")

    ;; Set start of user comment
    (setq comment-start-pos (point))

    ;; Return remaining space between user comment and start of
    ;; block-comment body
    (- comment-start-pos body-start-pos)
    )
  )

(defun block-comment--jump-to-last-char-in-body ()
  """ jumps to end of comment in body at point                          """
  """ End means the last non-fill character in the body                 """
  """ return: the number of fill characters remaining on the right side """

  (let (
        (body-end-pos nil)   ;; End of block-comment body
        (comment-end-pos nil);; End of user comment
        )

    (end-of-line)
    ;; Find end position in block comment
    (block-comment--jump-to-body-end 0)

    ;; Set end of block-comment body
    (setq body-end-pos (point))

    (skip-syntax-backward " " block-comment-centering--start-pos)

    ;; Set end of user comment
    (setq comment-end-pos (point))

    ;; Return remaining space between user comment and end of block-comment body
    (- body-end-pos comment-end-pos)
    )
  )

(defun block-comment--has-comment ()
  """ Checks if the block-comment-body at point contains a user comment """
  """ If it does, then return t, else nil                               """
  (let (
        (body-end 0)
        )
    (save-excursion
      (setq body-end (block-comment--jump-to-body-end))
      (block-comment--jump-to-body-start)
      (skip-syntax-forward " " body-end)
      (not (equal (point-marker)
                  body-end
                  )
           )
      )
    )
  )

(defun block-comment--is-body (&optional inside-body enclose)
  """ checks if the current row follows the format of a block comment body    """
  """ Param 'inside-body' specifies if point is required to be inside of the  """
  """                     body or not:                                        """
  """       t   -> Point must be inside the body                              """
  """       nil -> Point must be on the same row as body                      """
  """ Param 'enclose' specifies if we should look for enclose, or normal body """
  """       t   -> Look for enclose by using param 'enclose-fill'             """
  """       nil -> Look for block body by using param 'fill'                  """

  (let (
        (line-width 0)
        (read-prefix-pos nil)   ;; Position of current row:s prefix
        (read-postfix-pos nil)  ;; Position of current row:s postfix
        (point-in-body t)       ;; If point is inside body.
        ;; If looking for body, use fill,
        ;; if looking for enclose, use enclose-fill
        (fill-type (if enclose
                          block-comment-enclose-fill
                        block-comment-fill
                        )
                      )
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
             (concat block-comment-prefix fill-type)
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
             (concat fill-type block-comment-postfix)
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
