;;; block-comment-mode.el --- Generate and format block comments  -*- lexical-binding: t; -*-

;; Author: Daniel Thoren <danne_thoren@hotmail.com>
;; Maintainer: Daniel Thoren <danne_thoren@hotmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software…

;; Version: 0.1

;;; Commentary:

;; FIXME: Bug in alignment with previous rows comment. After aligning
;; once with an empty comment row above, the list in the function
;; 'block-comment--align-get-next' decarese from 5 elements long, to 3
;; elements. After this point, the new size of 3 is persistent, even
;; if point moves to a new row with a non-empty comment row above.

;; FIXME: Bug when using centering mode. Does not swap every other letter,
;;        instead swapping every few letters

;; TODO: Detect style assumes that current row is of body type.
;;       Should detect if it is body or pre/postfix line

;; TODO: Many functions assume that we have space above/beneath the comment.
;;       This assumption should not be made

;; TODO: Test extensively, then tag for release 1 (write unit tests)



;; TODO: Fix all warning when building with cask

;; TODO: Look over how local variables are managed:
;;      TODO: Look over initializations:
;;            * Variables are default inited in insert-or-resume after the current
;;              style has been detected and set. Should this even work?
;;      TODO: Look over variable defenitions, should these happen in the define
;;            -minor-mode? Now they are re-defined regularly in default-init-variables

;; TODO: Swap argument order for init-comment-style to take preamble row first, then body

;;;;;;;;;;;;;;;;;;;;;;;; Release 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Fix default style based on prog mode

;; TODO: implement offset between top enclose body and bottom enclose

;; TODO: Split mode into multiple files

;; TODO: Adhere to GNU coding convention:
;;       https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html

;; TODO: Add Toggling Between Different Lengths of block comments

;; TODO: Implement automatic block comment width detection

;; TODO: Add automatic row breaking when block comment is longer
;;       than 80 characters

;; TODO: Make block comment width indentation sensative, meaning that it does
;;       not exceed a strict width limit (80 characters)

;;; Code:

;; TODO: Add docstring to all variables

;; Global variables shared between buffers
(defvar block-comment-width 80)
(defvar block-comment-prefix nil)
(defvar block-comment-fill nil)
(defvar block-comment-postfix nil)

(defvar block-comment-enclose-prefix-top nil)
(defvar block-comment-enclose-fill-top nil)
(defvar block-comment-enclose-postfix-top nil)

(defvar block-comment-enclose-prefix-bot nil)
(defvar block-comment-enclose-fill-bot nil)
(defvar block-comment-enclose-postfix-bot nil)

;; Default values
(defvar block-comment-prefix-default nil)
(defvar block-comment-fill-default nil)
(defvar block-comment-postfix-default nil)

(defvar block-comment-enclose-prefix-top-default nil)
(defvar block-comment-enclose-fill-top-default nil)
(defvar block-comment-enclose-postfix-top-default nil)

(defvar block-comment-enclose-prefix-bot-default nil)
(defvar block-comment-enclose-fill-bot-default nil)
(defvar block-comment-enclose-postfix-bot-default nil)

;; Sets the target spacing between pre/postfix and user comment
(defvar block-comment-edge-offset 2)

;; Buffer local variables
(defvar-local block-comment-centering--start-pos nil)
(defvar-local block-comment-centering--end-pos nil)
(defvar-local block-comment-centering--order 1)
(defvar-local block-comment-centering--left-offset 0)
(defvar-local block-comment-centering--right-offset 0)
(defvar-local block-comment-centering--enabled nil)
(defvar-local block-comment-has-hooks nil)
(defvar-local block-comment--force-no-hooks nil)

;; Variables used in function 'block-comment--align-get-next' that needs to be
;; dynamically bound since they are inserted into a list then sorted using a lambda

;; TODO: Add block comment prefix if no other solution is found
(defvar-local body-start-distance nil)
(defvar-local prev-indent-start-distance nil)
(defvar-local prev-indent-end-distance nil)
(defvar-local body-end-distance nil)
(defvar-local body-center-distance nil)


(define-minor-mode block-comment-mode
  "Toggle block comments mode"
  :init-value nil
  :lighter "[Block-comment-mode]"
    :keymap (let ((map (make-sparse-keymap)))
            ;; press C-g to abort comment mode
              (define-key map (kbd "C-g") 'block-comment-abort)
              (define-key map (kbd "RET") 'block-comment-newline)
              (define-key map (kbd "M-j") 'block-comment-newline-indent)
              (define-key map (kbd "C-c C-c") 'block-comment-toggle-centering)
              (define-key map (kbd "TAB") 'block-comment-align-next)
              map)

    (progn
      (when block-comment-mode

        (block-comment--default-init-variables)

        (if (block-comment--insert-or-resume)
            ;; Add hooks, starting mode
            (block-comment--add-hooks)
          ;; Disable mode if insertion failed
          (setq block-comment-mode nil))
        )

      (unless block-comment-mode
        (block-comment--shutdown))
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                         Functions bound to keys                          """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment-start ()
  (interactive)
  (block-comment-mode 1)
  )

(defun block-comment-abort ()
  (interactive)
  """ Turns block-comment-mode off """
  (block-comment-mode 0))

(defun block-comment-newline-indent ()
  (interactive)
  """  Acts just like normal comments with M-j, meaning that the new line     """
  """  is indented to the same text indent as the previous line               """

  (let (
        (has-prev-comment (block-comment--has-comment))
        )
    (block-comment-newline)
    (when has-prev-comment
        (block-comment--align :prev-start)
      )
    )
  )

(defun block-comment-toggle-centering ()
  (interactive)
  """ Toggles centering mode """
  (if block-comment-centering--enabled
      (progn
        (setq-local block-comment-centering--enabled nil) ;; If enabled , disable
        (setq-local block-comment-centering--order 1) ;; Set order to right side (end of comment)
        (block-comment--message "BC: Centering disabled")
        )
    (progn
      (setq-local block-comment-centering--enabled t)     ;; If disabled, enabled
      (block-comment--align :center)
      (when (block-comment--has-comment)
        (block-comment--jump-to-last-char-in-body)
        )
      (block-comment--message "BC: Centering enabled")
      )
    )
  )

(defun block-comment-align-next ()
  (interactive)
  """  Moves the block comment text to the next alignment                    """
  (block-comment--align (block-comment--align-get-next))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                          Interactive functions                           """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--insert-or-resume ()
  """ This function is called to create or resume a block comment           """
  """ Checks if point is inside block comment or not.                       """
  """ If it is, resumeprevious block comment, else start new block comment  """

  (let (
        (inserted t)
        )

    ;;Check if in block comment
    (if (block-comment--detect-style)
        (progn

          ;; Force hooks off
          (block-comment--remove-hooks)
          (block-comment--force-no-hooks)

          ;; Jump to starting position to prevent width alignment from using
          ;; rightmost cursor position when  calculating target width
          (block-comment--jump-back)
          ;; Align width of each row in the comment
          (block-comment--align-width)
          ;; Jump to starting position again since width alignment will have
          ;; changed the row width, thus moving the cursor
          (block-comment--jump-back)

          ;; init row boundries
          (block-comment--init-row-boundries)

          ;; Enable hooks again
          (block-comment--allow-hooks)
          )
      ;; Else try to insert new comment if the current line is empty
      (if (block-comment--is-current-line-empty)
          (setq inserted (block-comment--insert))
        ;; If not empty, print error message
        (progn
          (block-comment--message "Line is not empty!")
          (setq inserted nil)
          ))
      )

    ;; Return if insertion was successful or not
    inserted
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                          Startup/shutdown logic                          """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--default-init-variables ()
  """ Init function run when block-comment-mode is started.                   """
  """ Sets default values for variables                                       """
  ;; TODO: Fix default style based on language?
  ;; TODO: Init style seperately?
  (unless block-comment-prefix
    (block-comment--init-comment-style 80   "/*" " " "*/"    "/*" "*" "*/" ))

  (setq-local block-comment-centering--start-pos nil)
  (setq-local block-comment-centering--end-pos nil)
  (setq-local block-comment-centering--order 1)
  (setq-local block-comment-centering--left-offset 0)
  (setq-local block-comment-centering--right-offset 0)
  (setq-local block-comment-centering--enabled nil)
  (setq-local block-comment-has-hooks nil)
  (setq-local block-comment--force-no-hooks nil)
  )

(defun block-comment--init-row-boundries ()
  """ Init comment row boundries for the current row.                        """

  ;; Set comment body start pos
  (save-excursion
    (block-comment--jump-to-body-start 0)
    (setq block-comment-centering--start-pos (point-marker))
    )

  ;; Set comment body end pos
  (save-excursion
    (block-comment--jump-to-body-end 0)
    (setq block-comment-centering--end-pos (point-marker))
    )
  )

(defun block-comment--init-comment-style (
                                          width
                                          prefix
                                          fill
                                          postfix
                                          enclose-prefix
                                          enclose-fill
                                          enclose-postfix
                                          &optional enclose-prefix-bot
                                                    enclose-fill-bot
                                                    enclose-postfix-bot)
  """ Initializes variables of block-comment-mode                             """
  """ This should be called during initialization of each mode where block-   """
  """ comment-mode shall be used. Default behaviour is c/c++ comment style    """
  """    -> enclose-prefix-bot :                                              """
  """    -> enclose-fill-bot :                                                """
  """    -> enclose-postfix-bot : If present, then a different set of         """
  """                             variables are used for the bottom enclose   """
  """                             than the top. If not, then the same         """
  """                             settings are used for both top and bottom   """

  (unless enclose-prefix-bot
    (setq enclose-prefix-bot enclose-prefix)
    (setq enclose-fill-bot enclose-fill)
    (setq enclose-postfix-bot enclose-postfix)
    )

  (setq block-comment-width width)

  (setq block-comment-prefix prefix)
  (setq block-comment-fill fill)
  (setq block-comment-postfix postfix)

  (setq block-comment-enclose-prefix-top enclose-prefix)
  (setq block-comment-enclose-fill-top enclose-fill)
  (setq block-comment-enclose-postfix-top enclose-postfix)

  (setq block-comment-enclose-prefix-bot enclose-prefix-bot)
  (setq block-comment-enclose-fill-bot enclose-fill-bot)
  (setq block-comment-enclose-postfix-bot enclose-postfix-bot)

  ;; Default parameters
  (setq block-comment-prefix-default prefix)
  (setq block-comment-fill-default fill)
  (setq block-comment-postfix-default postfix)

  (setq block-comment-enclose-prefix-top-default enclose-prefix)
  (setq block-comment-enclose-fill-top-default enclose-fill)
  (setq block-comment-enclose-postfix-top-default enclose-postfix)

  (setq block-comment-enclose-prefix-bot-default enclose-prefix-bot)
  (setq block-comment-enclose-fill-bot-default enclose-fill-bot)
  (setq block-comment-enclose-postfix-bot-default enclose-postfix-bot)

  ;; Sets the target spacing between pre/postfix and user comment
  (setq block-comment-edge-offset 2)
  )

(defun block-comment--shutdown ()
  """ Turns block comment off by removing the hooks """
  (block-comment--remove-hooks)
  )

(defun block-comment--force-no-hooks ()
  """  Enables the hook override, which overrides the add-hooks function      """
  """  behaviour. Meant to be used during initialization.                     """
  (block-comment--remove-hooks)
  (setq block-comment--force-no-hooks t)
  )

(defun block-comment--allow-hooks ()
  """  Disables the hook override                                             """
  (setq block-comment--force-no-hooks nil)
  )

(defun block-comment--remove-hooks ()
  """  Adds necessasry hooks                                                  """
  (when block-comment-has-hooks
    (setq post-command-hook
          (delete #'block-comment-centering--cursor-moved post-command-hook))
    (setq after-change-functions
          (delete #'block-comment-centering--edit after-change-functions))

    ;; Keep track of hook status
    (setq block-comment-has-hooks nil)
    )
  )

(defun block-comment--add-hooks ()
  """   Adds necessary hooks so that block-comment-mode can react to          """
  """   changes in the buffer. This behaviour can be overridden by the        """
  """   function 'block-comment--force-no-hooks'. In which case, the hooks    """
  """   will be forcibly disabled until the corresponding allow hooks         """
  """   is called.                                                            """
  (when (and (not block-comment-has-hooks) (not block-comment--force-no-hooks))
    ;; Keep track of the cursors position, if it leaves the block comment
    ;; then abort the centering mode)
    (add-to-list 'post-command-hook #'block-comment-centering--cursor-moved)

    ;; Add a hook that is called everytime the buffer is modified
    (add-to-list 'after-change-functions #'block-comment-centering--edit)

    ;; Keep track of hook status
    (setq block-comment-has-hooks t)
    )
  )

(defun block-comment--jump-back ()
  """  Jumps to starting position of current comment row based on current     """
  """  state. OBS: Assumes that current row holds a block comment             """
  """  if there is a user comment inside block: Jumps to end of comment       """
  """  else if centering is enabled:            Jump to center                """
  """  else centering is enabled:               Jump to start                 """
  (if (block-comment--has-comment)
      (block-comment--jump-to-last-char-in-body)
    (if block-comment-centering--enabled
        (block-comment--jump-to-body-center)
      (block-comment--jump-to-body-start)
      )
    )
  )

(defun block-comment-centering--cursor-moved ()
  """   This function is triggered by a hook every time point has moved.         """
  """   Used to abort block-comment-mode if cursor is outside of row boundry.    """
  """   If we are outside of row boundry, check if we are on a new block         """
  """   comment row. If we are, resume on the new row.                           """
  (let* (
         (start (marker-position block-comment-centering--start-pos))
         (end (marker-position block-comment-centering--end-pos))
         (cur (point-marker))
         )
    (if (or (< cur start) (< end cur))  ;; If outside of row boundry
        (if (block-comment--is-body t)  ;; If still in a block comment body
            (progn ;; Set up variables for new row
              (block-comment--default-init-variables)
              (block-comment--init-row-boundries)
              )
          (block-comment-mode 0)  ;; If not on block comment body, exit centering
          )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                             Insert functions                             """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--insert ()
  """ Inserts a new block comment and init centering """

  ;; Reset style parameters if they are incomplete
  (block-comment--reset-style-if-incomplete)

  ;; If at the top of the buffer, insert new line before inserting
  ;; block comment to avoid truncating the comment
  ;; (if (= (line-number-at-pos) 1)
  ;;     (newline))

  (let (
        (max-viable-column (- block-comment-width
                              (+
                               (string-width block-comment-prefix)
                               (string-width block-comment-postfix)
                               (* block-comment-edge-offset 2)
                               (/ block-comment-width 8)
                               ))
                           )
        )

    ;; Only insert comment if there is enough horizontal room
    (if (> max-viable-column (current-column))
        (progn

          ;; Insert top enclose
          (block-comment--insert-enclose block-comment-enclose-prefix-top
                                         block-comment-enclose-fill-top
                                         block-comment-enclose-postfix-top)

          (newline)

          ;; Insert bot enclose
          (block-comment--indent-accoring-to-previous-block-row)
          (block-comment--insert-enclose block-comment-enclose-prefix-bot
                                         block-comment-enclose-fill-bot
                                         block-comment-enclose-postfix-bot)

          (beginning-of-line)
          (newline)
          (forward-line -1)

          ;; Insert body
          (block-comment--indent-accoring-to-previous-block-row)

          (block-comment--insert-line (- block-comment-width (current-column)))
          (block-comment--init-row-boundries)
          (block-comment--jump-back)

          ;; return t
          t
          )
      (progn
        (block-comment--message "Not enough room to insert comment!")
        ;; return nil
        nil
        )
      ) ;; end if
    )
  )

(defun block-comment-newline (&optional target-width)
  (interactive)
  """ Inserts a new line and moves text to the right of point down"""

  (let (
        (remain-text-start (point-marker))
        (remain-text-end nil)
        (remain-text nil)
        )

    (block-comment--remove-hooks)

    ;; Get current block-comment width
    (unless target-width (setq target-width (block-comment--get-comment-width)))

    (when (and (block-comment--is-body)
               (block-comment--has-comment)
               (not (block-comment--is-point-right-of-comment)))
      (block-comment--jump-to-last-char-in-body)
      (setq remain-text-end (point-marker))

      ;; Delete remaining text between point and end of body
      (setq remain-text (delete-and-extract-region remain-text-start
                                                   remain-text-end))

      ;; Insert the same amount of fill characters that we just removed to keep
      ;; alignment
      (insert (make-string (string-width remain-text)
                           (string-to-char block-comment-fill)))
      )

    (end-of-line)
    (insert "\n")
    (block-comment--indent-accoring-to-previous-block-row)

    (block-comment--insert-line target-width)
    (block-comment--init-row-boundries)
    (block-comment--add-hooks)
    (block-comment--jump-back)

    ;; If there is text to the right of point, reinsert the deleted text
    (when remain-text
      (insert remain-text)
      (block-comment--jump-to-first-char-in-body)
      )
    )
  )

(defun block-comment--insert-line (width)
  """  Inserts a new block comment body line at point with 'indent-level'     """
  """  Param 'width' : The width of the comment line                          """
  (let* (
         (fill-count (+ 1 (- width
                             (+ (string-width block-comment-prefix)
                                (string-width block-comment-postfix)
                                )
                             )
                        )
                     )
         )

    (save-excursion
      ;; Insert the comment body
      (insert block-comment-prefix)
      (insert (make-string fill-count (string-to-char block-comment-fill)))
      (insert block-comment-postfix)
      ) ;; End excursion
    )
  )

(defun block-comment--insert-enclose (prefix fill postfix)
  """ Inserts a enclosing line at point                                      """
  """ A enclosing line is a line inserted before and                         """
  """ after the block comment body                                           """

  (let* (
         (target-width (+ 1 (- block-comment-width (current-column))))
         (padding-length (- target-width
                            (+ (string-width prefix)
                               (string-width postfix))))
         )

    (insert prefix)
    (insert (make-string padding-length
                         (string-to-char fill)))
    (insert postfix)
    ) ;; End let
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                             Style detection                               """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--detect-style ()
  """  Attempts to detect the block comment style at point. This is done by     """
  """  looking for prefix/postfix on the current row. If found, then looks      """
  """  for enclose by moving up/down until the format no longer follows         """
  """  the current row. Then check if there is a prefix/postfix with only       """
  """  fill character between. If either enclose is not found, then set both    """
  """  enclose to the empty string ''. When body enclose is found, then         """
  """  return t, else nil                                                       """
  """   -> Return: t if body style found, else nil                              """
  (let (
        (body-found nil)
        (enclose-top-found nil)
        (enclose-bot-found nil)
        (lines-in-buffer (block-comment--get-line-count-in-buffer))
        (at-bottom nil)
        )

    ;;-------------------------- Detect body style ------------------------------

    ;; Detect block comment body style and set global symbols
    (setq body-found (block-comment--detect-body-style 'block-comment-prefix
                                                       'block-comment-postfix))

    ;; Only try to find enclose if there is a block comment body
    (when body-found
      ;;-------------------------- Detect enclose top style -----------------------

      (save-excursion
        ;; Move to row above block comment body
        (while (progn
                 ;; Move up one line
                 (forward-line -1)

                 ;; Continue if still in block comment body and not at top of buffer
                 (and (not (block-comment--is-at-buffer-top))
                      (block-comment--is-body nil))
                 )
          )

        ;; Detect style
        (setq enclose-top-found (block-comment--detect-enclose-style
                                 'block-comment-enclose-prefix-top
                                 'block-comment-enclose-fill-top
                                 'block-comment-enclose-postfix-top))
        )

      ;;-------------------------- Detect enclose top style -----------------------

      (save-excursion
        ;; Move to row above block comment body
        (while (progn
                 ;; Move up one line
                 (forward-line 1)

                 ;; Check if at bottom of buffer
                 (setq at-bottom (block-comment--is-at-buffer-bot lines-in-buffer))

                 ;; Continue if still in block comment body and not at bottom of buffer
                 (and (not at-bottom)
                      (block-comment--is-body nil))
                 )
          )

        ;; Detect style
        (setq enclose-bot-found (block-comment--detect-enclose-style
                                 'block-comment-enclose-prefix-bot
                                 'block-comment-enclose-fill-bot
                                 'block-comment-enclose-postfix-bot))
        )

      ;; If either enclose was not found, set both to non-existent
      (unless (and enclose-top-found
                   enclose-bot-found)

        (setq block-comment-enclose-prefix-top "")
        (setq block-comment-enclose-fill-top "")
        (setq block-comment-enclose-postfix-top "")

        (setq block-comment-enclose-prefix-bot "")
        (setq block-comment-enclose-fill-bot "")
        (setq block-comment-enclose-postfix-bot "")
        )
      )

    ;; Return t if style found, else nil
    body-found
    )
  )

(defun block-comment--detect-body-style (body-prefix-symbol
                                         body-postfix-symbol)
  """   Function auto detects what body style is used, meaning which prefix   """
  """   and postfix that is used for the block comment body on the current    """
  """   row. The global style is updated accordingly.                         """
  """   Param 'body-prefix-symbol' : The symbol to which the new prefix shall """
  """                                be written.                              """
  """   Param 'body-postfix-symbol' : The symbol to which the new postfix     """
  """                                 shall be written.                       """
  """   -> Return: t if found, else nil                                       """
  """   OBS: It is assumed that the current row contains a block comment,     """
  """        behaviour is undefined if it does not!                           """

  (let* (
         (start-pos nil)
         (end-pos nil)
         (fill-margin-pos nil)
         (prefix "")
         (prefix-fill "")
         (postfix "")
         (postfix-fill "")
         (encountered-error nil)
         )

    ;; Only try to detect if the line is not blank
    (unless (block-comment--is-blank-line)

      ;; (condition-case nil
      (save-excursion
        (end-of-line)
        (skip-syntax-backward " " (line-beginning-position))

        (unless (= (point) (line-beginning-position))
          (setq end-pos (point-marker))
          (skip-syntax-backward "^ " (line-beginning-position))
          (setq start-pos (point-marker))

          ;; If there is enough space remaining
          (if (< block-comment-edge-offset (- (marker-position (point-marker))
                                              (point-min)))
              (progn
                (backward-char block-comment-edge-offset)
                (setq fill-margin-pos (point-marker))

                (setq postfix (buffer-substring start-pos end-pos))
                (setq postfix-fill (buffer-substring start-pos fill-margin-pos))
                )
            (progn
              (setq encountered-error t)
              )
            )
          )
        )

        ;; (error
        ;;  (setq encountered-error t)
        ;;  (block-comment--error "block-comment--is-enclose: Encountered end-of-buffer" "BC: end-of-buffer")
        ;;  )
        ;; )

      ;; Find prefix
      (save-excursion
        (beginning-of-line)
        (skip-syntax-forward " " (line-end-position))

        (unless (= (point) (line-end-position))
          (setq start-pos (point-marker))
          (skip-syntax-forward "^ " (line-end-position))
          (setq end-pos (point-marker))

          ;; If there is enough space remaining
          (if (< block-comment-edge-offset (- (point-max)
                                              (marker-position (point-marker))))
              (progn
                (forward-char block-comment-edge-offset)
                (setq fill-margin-pos (point-marker))

                (setq prefix (buffer-substring start-pos end-pos))
                (setq prefix-fill (buffer-substring end-pos fill-margin-pos))
                )
            (progn
              (setq encountered-error t)
              )
            )

          )
        )
      )

    ;; Only modify when prefix/postfix was found
    (if (and (not encountered-error)
             (> (string-width prefix) 0)
             (> (string-width postfix) 0)
             (string= prefix-fill postfix-fill))
        ;; If found, modify the given symbols and return t
        (progn
          (set body-prefix-symbol prefix)
          (set body-postfix-symbol postfix)
          t
          )
      ;; If not found, return nil
      nil
      )
    )
  )

(defun block-comment--detect-enclose-style (prefix-symbol
                                            fill-symbol
                                            postfix-symbol)
  """  Function auto detects what enclose style is used, meaning which        """
  """  prefix,fill and postfix that is used for the enclose on the current    """
  """  row. The given symbols are updated with the new values,                """
  """  if they are found. If they are not, the are set to ""                  """
  """   Param 'prefix-symbol' : The symbol to which the new prefix            """
  """                           shall be written.                             """
  """   Param 'fill-symbol' : The symbol to which the new fill                """
  """                         shall be written.                               """
  """   Param 'postfix-symbol' : The symbol to which the new postfix          """
  """                            shall be written.                            """
  """   -> Return: t if enclose style found, else nil                         """
  """  OBS: Point must be on the enclose row before calling this function!    """

  (let* (
         (enclose-prefix nil)
         (enclose-fill nil)
         (enclose-postfix nil)
         (enclose-found nil)
         )

    ;;-------------------------- Find fill ----------------------------------
    (setq enclose-fill (block-comment--detect-enclose-fill))

    (when enclose-fill
      ;;-------------------------- Find prefix ----------------------------------
      (setq enclose-prefix (block-comment--detect-enclose-prefix enclose-fill))

      ;;-------------------------- Find postfix ----------------------------------
      (setq enclose-postfix (block-comment--detect-enclose-postfix enclose-fill))

      ;;-------------------------- sanity check ----------------------------------

      ;; If all components were found, and is enclose returns true, set given
      ;; symbols to the found values
      (when (and enclose-prefix
                 enclose-postfix
                 (block-comment--is-enclose enclose-prefix
                                            enclose-fill
                                            enclose-postfix))

        (set prefix-symbol enclose-prefix)
        (set fill-symbol enclose-fill)
        (set postfix-symbol enclose-postfix)
        (setq enclose-found t)
        )
      )

    ;; Return t if found, else nil
    enclose-found
    )
  )

(defun block-comment--detect-enclose-fill ()
  """  Detects the fill string of the enclose on the current row and          """
  """  returns said fill.                                                     """
  """   -> Return: The fill string                                            """

  (let (
        (block-start nil)
        (block-end nil)
        (block-middle nil)
        (enclose-fill nil)
        )

    ;; Find block end
    (end-of-line)
    (skip-syntax-backward " " (line-beginning-position))
      (setq block-end (current-column))

    ;; Find block start
    (beginning-of-line)
    (skip-syntax-forward " " (line-end-position))
      (setq block-start (current-column))

    ;; If block end is less than block start, then the row is empty
    (when (and block-end block-start (> block-end block-start))
      ;; Jump to middle
      (setq block-middle (/ (- block-end block-start) 2))
      (forward-char block-middle)

      (setq enclose-fill (string (char-after)))
      )

    ;; Return fill string
    enclose-fill
    )
  )

(defun block-comment--detect-enclose-prefix (enclose-fill)
  """  Detects the prefix string of the enclose on the current row and        """
  """  returns said prefix.                                                   """
  """   Param 'fill' : The enclose fill for the current row                   """
  """   -> Return: The prefix string                                          """
  (let (
        (block-start nil)
        (block-end nil)
        (enclose-prefix nil)
        )
    (save-excursion
      (beginning-of-line)
      ;; Skip to the first non blank character and define this as the start of
      ;; the prefix
      (skip-syntax-forward " " (line-end-position))
      (setq block-start (point-marker))
      )
    (save-excursion
      ;; Skip backward from the middle until first non fill character was found
      (skip-chars-backward enclose-fill (line-beginning-position))
      (setq block-end (point-marker))
      )

    (if (<= block-end block-start)
        ;; If all characters from the middle to the start are the same, then
        ;; prefix is the same character as fill
        (setq enclose-prefix enclose-fill)
      ;; If not, then capture the prefix
      (progn
        (setq enclose-prefix (buffer-substring block-start block-end))
        ;; Sanity check, prefix should not be longer than 5
        (when (> (string-width enclose-prefix) 5)
          (setq enclose-prefix nil)
          )
        )
      )

    ;; Return the prefix
    enclose-prefix
    )
  )

(defun block-comment--detect-enclose-postfix (enclose-fill)
  """  Detects the postfix string of the enclose on the current row and       """
  """  returns said postfix.                                                  """
  """   Param 'fill' : The enclose fill for the current row                   """
  """   -> Return: The postfix string                                         """
  (let (
        (block-start nil)
        (block-end nil)
        (enclose-postfix nil)
        )
    (save-excursion
      (end-of-line)
      ;; Skip to the first non blank character and define this as the end of
      ;; the postfix
      (skip-syntax-backward " " (line-beginning-position))
      (setq block-end (point-marker))
      )
    (save-excursion
      ;; Skip backward from the middle until first non fill character was found
      (skip-chars-forward enclose-fill (line-end-position))
      (setq block-start (point-marker))
      )

    (if (<= block-end block-start)
        ;; If all characters from the middle to the start are the same, then
        ;; prefix is the same character as fill
        (setq enclose-postfix enclose-fill)
      ;; If not, then capture the prefix
      (progn
        (setq enclose-postfix (buffer-substring block-start block-end))
        ;; Sanity check, prefix should not be longer than 5
        (when (> (string-width enclose-postfix) 5)
          (setq enclose-postfix nil)
          )
        )
      )
    ;; Return postfix string
    enclose-postfix
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                             Centering logic                              """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                                                  block-comment-centering--enabled)
        (progn
          ;; If centering is not enabled, only remove from right side
          ;; of user comment
          (unless block-comment-centering--enabled
            (setq left 0)
            (setq right step)
            )
          (block-comment-centering--inserted-chars left right))
        )

      ;; Alternate between putting larger step on left/right side
      ;; if centering is enabled
      (when block-comment-centering--enabled
        (setq block-comment-centering--order
              (- 1 block-comment-centering--order))
        )
      )

  ;; Re-align point if it is outside of boundry
  (block-comment--align-width)
  (block-comment--align-point)
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
           ;; Get position of right hand side of comment
           (comment-end-pos (progn
                              (block-comment--jump-to-comment-end 0)
                              (current-column)))
           ;; Get the removed width
           (removed-width (- block-comment-width
                             comment-end-pos))
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
  """   left side. If user comment grows larger than target width,         """
  """   stops removing characters                                          """
  (let (
        (remain-space-left 0)
        (remain-space-right 0)
        (edge-offset block-comment-edge-offset)
        )

    ;; Make edge offset 1 char larger when centering to make it easier to
    ;; differentiate between the modes
    (when block-comment-centering--enabled
      (setq edge-offset (+ edge-offset 1))
      )

    (save-excursion

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
      (when (<= remain-space-left edge-offset)
        (setq right (+ right left))
        (setq left 0)
        )

      ;; Remove characters at beginning of line
      (block-comment--jump-to-body-start 0)
      (delete-char left)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;; Right side ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Remove characters at end of line
      (block-comment--jump-to-body-end 0)

      (if (<= remain-space-right edge-offset)
          ;; If there is no space left, make more space
          (progn
            (insert (make-string right
                                 (string-to-char block-comment-fill))
                    )
            ;; Update end of block comment to avoid aborting block comment mode
            (setq block-comment-centering--end-pos (point-marker))
            )
          ;; If there is space left, remove the right portion
          (delete-char (- right))
          )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                         Text alignment functions                         """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--align (next-alignment)
  """  Checks which alignment the comment text currently has and moves the    """
  """  text block to the next alignment. The following alignments are         """
  """  available:                                                             """
  """             :start -> Align text to the start of the comment            """
  """             :prev-start -> Align text with the beginning of the         """
  """                            previous row:s text block                    """
  """             :prev-end -> Align text with the end of the previous        """
  """                          row:s text block                               """
  """             :end -> Align with the end of body                          """
  (block-comment--remove-hooks)
  (let (
        (comment-text-start nil) ;; Start of comment text
        (comment-text-end nil)   ;; End of comment text
        (point-start-pos (point-marker))

        (relative-text-position nil)   ;; Points relative position inside comment text from the left
        (comment-text nil)
        (comment-text-width nil)
        (remain-fill nil)
        )

    (when (block-comment--has-comment) ;; Align text if there is any

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
            (progn ;; Position positive (to the right
              (setq comment-text-width (string-width comment-text))
              (setq remain-fill (- (+ (block-comment--jump-to-last-char-in-body)
                                      comment-text-width)
                                   block-comment-edge-offset))
              ;; If there is not enough space left, reduce relative position
              (when (> relative-text-position remain-fill)
                (setq relative-text-position remain-fill))
              )
          (progn ;; Position negative (to the left)
            (setq remain-fill (- (block-comment--jump-to-first-char-in-body)
                                 block-comment-edge-offset))
            ;; If there is not enough space left, reduce relative position
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

(defun block-comment--align-get-next ()
  """  Checks the current alignment of the comment body text and retuns the    """
  """  next alignment, taking positioning into consideration. The following    """
  """  alignments are available:                                               """
  """               :start -> Align text to the start of the comment           """
  """               :prev-start -> Align text with the beginning of the        """
  """                              previous row:s text block                   """
  """               :prev-end -> Align text with the end of the previous       """
  """                            row:s text block                              """
  """               :end -> Align with the end of body                         """
  """  -> Return: One of the symbols defined above                             """
  (let (
        (comment-text-start nil) ;; Start of comment text
        (comment-text-end nil)   ;; End of comment text
        (body-start nil)         ;; The body start position
        (prev-indent-start 0)    ;; The first char position of the text in the previous block comment
        (body-center 0)          ;; Body center position
        (prev-indent-end 0)      ;; The last char position of the text in the previous block comment
        (body-end nil)           ;; Body end position
        (text-center nil)        ;; The center of the comment text
        )

    ;; Find text boundry if there is text
    (if (block-comment--has-comment)
        (progn
          (save-excursion
            (block-comment--jump-to-last-char-in-body)
            (setq comment-text-end (current-column))

            (block-comment--jump-to-first-char-in-body)
            (setq comment-text-start (current-column))
            ))
      ;; If no text exist, use current position
      (progn)
      (setq comment-text-start (current-column))
      (setq comment-text-end (current-column))
      )

    ;; Find internal indent of previous row
    (save-excursion
      (forward-line -1)
      ;; Leave values at 0 if the previous line does not contain a block comment
      ;; body, or if the body is emtpy
      (when (and (block-comment--is-body)
                 (block-comment--has-comment))

        (block-comment--jump-to-first-char-in-body)
        (setq prev-indent-start (current-column))

        (block-comment--jump-to-last-char-in-body)
        (setq prev-indent-end (current-column))
        ))

    ;; Find body start/center/end positions
    (save-excursion
      (block-comment--jump-to-body-start)
      (setq body-start (current-column))

      (block-comment--jump-to-body-center)
      (setq body-center (current-column))

      (block-comment--jump-to-body-end)
      (setq body-end (current-column))
      )

    ;; TODO: Scoping issue with lambda
    (setq body-start-distance (- body-start comment-text-start))
    (setq prev-indent-start-distance (- prev-indent-start comment-text-start))
    (setq prev-indent-end-distance (- prev-indent-end comment-text-start))
    (setq body-end-distance (- body-end comment-text-start))

    ;; The center of the comment text
    (setq text-center (if (= comment-text-start comment-text-end)
                     comment-text-start
                   (ceiling (- comment-text-end comment-text-start)
                            2)))
    ;; Distance from text center to body center
    (setq body-center-distance (- body-center
                             (+ comment-text-start
                                text-center)))

    (let* (
          (list '((body-start-distance . :start)
                  (body-center-distance . :center)
                  (prev-indent-start-distance . :prev-start)
                  (prev-indent-end-distance . :prev-end)
                  (body-end-distance . :end)))

          (curr-elem 0)
          )

      ;; Sort by distance
      (setq list (sort list
                       (lambda (a b)
                         (< (symbol-value (car a)) (symbol-value (car b))))))

      ;; Iterate until first distance larger than 0 that can fit
      ;; inside of the body is found, or until the end of the list.
      (while (and (< curr-elem (- (length list) 1))
                  (or (>= 0 (symbol-value (car (nth curr-elem list))))
                      (< (- body-end (+ comment-text-end (symbol-value (car (nth curr-elem list))))) 0 )
                      )
                  )

        (setq curr-elem (+ curr-elem 1))
        )

      ;; If curr elem is end alignment, check if we should wrap around.
      ;; When text already is end-aligned, wrap around to start aligned
      (when (and (= curr-elem (- (length list) 1))
                 (>= comment-text-end body-end))
        (setq curr-elem 0)
        )

      ;; If no text on row, when reaching end position the curr elem will be
      ;; nil. Wrap around fixed here
      (unless (nth curr-elem list)
        (setq curr-elem 0)
        )

      (cdr (nth curr-elem list))
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                       Width alignment functions                          """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--align-width ()
  """  Aligns the width of all rows in accordance with the widest row          """
  (let* (
        (indentation (block-comment--get-indent-level))
        (rightmost-text-column (block-comment--get-rightmost-comment-text-column))
        (point-column (current-column))
        (rightmost-column (max point-column rightmost-text-column))

        (target-width (+ (- rightmost-column indentation)
                         block-comment-edge-offset
                         (string-width block-comment-prefix))
                      )
        )

    ;; Dont make width less than target minus indentation
    (when (< target-width (- block-comment-width indentation))
      (setq target-width (- block-comment-width indentation))
      )

    ;; Disable hooks to disable centering when adjusting width
    (block-comment--remove-hooks)

    ;; FIXME: Dont start from row beneath comment, this failes when there are no
    ;; rows beneath. Start on last row instead
    (save-excursion
      (block-comment--jump-to-last-comment-row)
      (block-comment--adjust-rows-above-to-target-width target-width)
      )

    ;; Re-enable hooks
    (block-comment--add-hooks)
    )
  )

(defun block-comment--adjust-rows-above-to-target-width (target-width)
  """  Aligns all block comment rows, starting from the current line and      """
  """  upward to the given target width                                       """

  """  Param 'target-width': The width to align to                            """
  (let (
        (curr-width 0)
        (width-diff 0)
        (is-body nil)
        (is-enclose-top nil)
        (is-enclose-bot nil)
        (at-top nil)
        )

    ;; Align all block comment rows above
    (while (progn

             ;; Check if this is body or enclose
             (setq is-body (block-comment--is-body nil))
             (setq is-enclose-top (block-comment--is-enclose-top nil))
             (setq is-enclose-bot (block-comment--is-enclose-bot nil))

             ;; Exit if not in body or if at top of buffer
             (and (not at-top)
                  (or is-body is-enclose-top is-enclose-bot))
             )

      (setq curr-width (block-comment--get-comment-width))
      (setq width-diff (- target-width curr-width))

      ;; When normal block comment line
      (save-excursion
        (cond (is-body
               (block-comment--align-body-width width-diff
                                                block-comment-fill))

              ;; else if: enclose-top
              (is-enclose-top
               (block-comment--align-enclose-width width-diff
                                                   block-comment-enclose-fill-top))

              ;; else if: enclose-bot
              (is-enclose-bot
               (block-comment--align-enclose-width width-diff
                                                   block-comment-enclose-fill-bot))
              ))

      ;; Check if at top after performing logic in order to process the top
      ;; line before breaking the while loop
      (setq at-top (block-comment--is-at-buffer-top))

      ;; Move up one line
      (block-comment--move-line -1)

      ) ;; end while
    )
  )

(defun block-comment--align-body-width (width-diff fill)
  """  Changes the block comment width  'width-diff' characters, inserting     """
  """  if the diff is positive and removing if it is positive.                 """
  """  If inserting, then inserts half at beginning, and half at the end of    """
  """  comment body. Takes the centering mode indo consideration.              """
  """  Param 'width-diff': How much the width should change, increases if      """
  """                      positive, decreases if negative                     """
  """  Param 'fill'      : The char to fill with                               """
  """  OBS: This function assumes that the block comment body fits inside the  """
  """  new boundry!                                                            """

  (if (< width-diff 0)
      (block-comment--align-body-width-decrease width-diff)
    (block-comment--align-body-width-increase width-diff fill)
    )
  )

(defun block-comment--align-body-width-increase (increase fill)
  """  Increases the block comment body width with 'increase' number of       """
  """   fill characters.                                                      """
  """  Param 'increase': How much the width should change, increases if       """
  """                    positive, decreases if negative                      """
  """  Param 'fill'    : The char to fill with                                """

    (let* (
         (step (abs increase))
         (min-step (/ step 2))
         (max-step (- step min-step))

         (left  (if (= block-comment-centering--order 0) max-step min-step))
         (right (if (= block-comment-centering--order 0) min-step max-step))
         )

      (if (block-comment--is-centering-row)
          ;; Alternate between putting larger step on left/right side
          ;; if centering is enabled
          (progn
            (when block-comment-centering--enabled
              (setq block-comment-centering--order
                    (- 1 block-comment-centering--order))
              )
            )
        ;; When not centering, only add to the right
        (progn
          (setq right (+ left right))
          (setq left 0)
          )
        ) ;; End if

      (block-comment--jump-to-body-start 0)
      (insert (make-string left
                           (string-to-char fill)))

      (block-comment--jump-to-body-end 0)
      (insert (make-string right
                           (string-to-char fill)))
      )
    )

(defun block-comment--align-body-width-decrease (decrease)
  """  Decrease the block comment body width with 'decrease' amount           """
  """  Param 'decrease': How much the width should change, increases if      """
  """                    positive, decreases if negative                      """
  """  Param 'fill'    : The char to fill with                                """

  (let* (
         (step (abs decrease))
         (min-step (/ step 2))
         (max-step (- step min-step))

         (left  0)
         (right 0)
         )

    (if (block-comment--is-centering-row)
        (progn
          ;; When centering, move text to center to avoid truncating text
          (block-comment--align :center)
          ;; Remove hooks again since function above adds them
          (block-comment--remove-hooks)
          ;; Take centering order into consideration
          (setq left  (if (= block-comment-centering--order 1) max-step min-step))
          (setq right (if (= block-comment-centering--order 1) min-step max-step))

          ;; Alternate between putting larger step on left/right side
          ;; if centering is enabled
          (setq block-comment-centering--order
                (- 1 block-comment-centering--order))
          )
      ;; When not centering, only remove from the right if possible
      (let (
            (remain-right (block-comment--jump-to-last-char-in-body 0))
            )
        ;; Try to remove as many characters as
        ;; possible from the right side to keep the formatting
        (setq right (if (> remain-right step) step remain-right))
        (setq left (- step right))
        )
      )

      (block-comment--jump-to-body-start 0)
      (delete-char left)

      (block-comment--jump-to-body-end 0)
      (delete-char (- right))
    )
  )

(defun block-comment--align-enclose-width (width-diff fill)
  """  Changes the block comment width  'width-diff' characters, inserting     """
  """  if the diff is positive and removing if it is positive.                 """
  """  Param 'width-diff': How much the width should change, increases if      """
  """                      positive, decreases if negative                     """
  """  Param 'fill'      : The char to fill with                               """
  """  OBS: This function assumes that the block comment body fits inside the  """
  """  new boundry!                                                            """
  (let* (
         (step (abs width-diff))
         (min-step (/ step 2))
         (max-step (- step min-step))
         )
    (block-comment--jump-to-body-center)

    ;; If width should increase
    (when (> width-diff 0)
      (insert (make-string step
                           (string-to-char fill)
                           )
              )
      ) ;; End when width-diff positive

    (when (< width-diff 0)
      (delete-char min-step)
      (delete-char (- max-step))
      )
    ) ;; End when width-diff negative
  )

(defun block-comment--align-point ()
  """  If point is outside of the comment bounds after width alignment, put  """
  """  point inside of the bounds                                            """
  (let (
        (right-boundry nil)
        (left-boundry nil)
        (curr-pos (point-marker))
        )
    (save-excursion
        (setq right-boundry (block-comment--jump-to-body-end))
        (setq left-boundry (block-comment--jump-to-body-start))
      )

    (when (> curr-pos right-boundry)
      (backward-char (- curr-pos right-boundry)))
    (when (< curr-pos left-boundry)
      (forward-char (- right-boundry curr-pos)))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                             Helper functions                             """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--reset-style-if-incomplete ()
  """  Resets style to default if any of the style parameters is missing      """

  (when (or (string= block-comment-prefix "")
            (string= block-comment-fill "")
            (string= block-comment-postfix "")
            (string= block-comment-enclose-prefix-top "")
            (string= block-comment-enclose-fill-top "")
            (string= block-comment-enclose-postfix-top "")
            (string= block-comment-enclose-prefix-bot "")
            (string= block-comment-enclose-fill-bot "")
            (string= block-comment-enclose-postfix-bot ""))
    (setq block-comment-prefix block-comment-prefix-default)
    (setq block-comment-fill block-comment-fill-default)
    (setq block-comment-postfix block-comment-postfix-default)
    (setq block-comment-enclose-prefix-top block-comment-enclose-prefix-top-default)
    (setq block-comment-enclose-fill-top block-comment-enclose-fill-top-default)
    (setq block-comment-enclose-postfix-top block-comment-enclose-postfix-top-default)
    (setq block-comment-enclose-prefix-bot block-comment-enclose-prefix-bot-default)
    (setq block-comment-enclose-fill-bot block-comment-enclose-fill-bot-default)
    (setq block-comment-enclose-postfix-bot block-comment-enclose-postfix-bot-default)
    )
  )

(defun block-comment--indent-accoring-to-previous-block-row ()
  """  Indent current row in accordance with the block comment row on           """
  """  the previous line. Auto detects if previous line is body or enclose-top  """
  """  -> return: Current indentation level                                     """
  (block-comment--move-line -1)

  (let (
        (indent-level 0)
        (prefix block-comment-prefix)
        )

    ;; Detect if previous line is enclose. Not looking for bottom enclose since
    ;; we never indent relative to thet
    (when (block-comment--is-enclose-top)
      (setq prefix block-comment-enclose-prefix-top)
      )

    (setq indent-level (block-comment--get-indent-level prefix))

    (block-comment--move-line 1)

    (beginning-of-line)
    (insert (make-string indent-level
                         (string-to-char " ")
                         )
            )

    ;; Return indent level
    indent-level
    )
  )

(defun block-comment--get-indent-level (&optional prefix)
  """  Get the indentation level (int) of the current row                       """
  """  Param 'prefix' : The prefix to look for                                  """
  """                   Default: block-comment-prefix                           """
  """           return: Current indentation level                               """
  (unless prefix (setq prefix block-comment-prefix))

  (save-excursion
    (block-comment--jump-to-comment-start prefix)
    (current-column))
  )


(defun block-comment--is-centering-row (&optional tolerance)
  """  Checks if the current block comment row is centering or non-centering.    """
  """  If the left margin is larger than (edge-offset + 1) and the diff          """
  """  between the margins is less than tolerance,                               """
  """  then is centering                                                         """
  """  Param 'tolerance': How much off center the text is allowed to be          """
  """                     -> Default = 2                                         """
  """  -> Return: t if text is centered, else nil                                """
  (unless tolerance (setq tolerance 2))

  (save-excursion
  (let (
        (begin-width (block-comment--jump-to-first-char-in-body))
        (end-width (block-comment--jump-to-last-char-in-body 0))
        )
    ;; If diff between begin/end width is smaller than x, then assume
    ;; that we are in centering mode
    (and (> begin-width (+ block-comment-edge-offset 1))
         (> tolerance
            (abs (- begin-width end-width)))
         )
    )
    )
  )

(defun block-comment--is-point-right-of-comment ()
  """ Returns t if current point if right of block comment text               """
  (save-excursion
    (let (
          (current-pos (current-column))
          (text-end (progn
                      (block-comment--jump-to-last-char-in-body)
                      (current-column)
                    ))
          )
      (>= current-pos text-end)
      )
    )
  )

(defun block-comment--move-line (count)
  """  Moves point 'count' lines up/down, keeping the column position.         """
  """  Param 'count':                                                          """
  """                 +x -> move point x lines down                            """
  """                 -x -> move point x lines up                              """
  (let (
        (column (current-column))
        )
    (forward-line count)
    (move-to-column column)
    )
  )

(defun block-comment--has-comment ()
  """ Checks if the block-comment-body at point contains a user comment """
  """ If it does, then return t, else nil                               """
  (let (
        (body-end nil)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                            Get functions                                  """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--get-line-count-in-buffer ()
  """  Gets the number of lines in the current buffer                        """
  """  -> Return: The number of lines in the current buffer                  """
  (count-lines (point-min) (point-max))
  )

(defun block-comment--get-widest-comment-text ()
  """  Finds the width of the widest block comment text above point and        """
  """  returns said width. The block comment text is the actual user text      """
  """  inside the block comment body.                                          """
  (let (
        (widest-width 0)
        (curr-width 0)
        (at-top nil)
        )

    (save-excursion
      ;; Jump to the last non-postfix row in the block comment
      (block-comment--jump-to-last-comment-row 0 t)

      ;; Get widest comment text
      (while (and (not at-top)
                  (block-comment--is-body nil))

        (setq curr-width (block-comment--get-comment-text-width))
        (when (> curr-width widest-width)
          (setq widest-width curr-width)
          )

        ;; Check if at top after performing logic in order to process the top
        ;; line before breaking the while loop
        (setq at-top (block-comment--is-at-buffer-top))

        ;; Move up one line
        (forward-line -1)
        )
      )

    ;; Return widest width
    widest-width
    ) ;; End let
  )

(defun block-comment--get-rightmost-comment-text-column ()
  """  Gets the column of the non-fill character farthest to the right in  """
  """  the current block comment.                                          """
  (let (
        (rightmost-column 0)
        (curr-column 0)
        (at-top nil)
        )

    (save-excursion
      ;; Jump to the last non-postfix row in the block comment
      (block-comment--jump-to-last-comment-row 0 t)

      ;; Get right most text column position
      (while (and (not at-top)
                  (block-comment--is-body nil))

        (block-comment--jump-to-last-char-in-body 0)
        (setq curr-column (current-column))
        (when (> curr-column rightmost-column)
          (setq rightmost-column curr-column)
          )

        ;; Check if at top after performing logic in order to process the top
        ;; line before breaking the while loop
        (setq at-top (block-comment--is-at-buffer-top))

        ;; Move up one line
        (forward-line -1)
        )
      )

    ;; Return rightmost column
    rightmost-column
    ) ;; End let
  )

(defun block-comment--get-comment-width ()
  """  Returns the width of the block comment row at point, meaning the        """
  """  entire width of the block comment, from first char of prefix, to last   """
  """  char of postfix. This function works on both a comment row, and         """
  """  a pre/post amble row                                                    """

  (let* (
        (comment-start 0)
        (comment-end 0)
        )

    (save-excursion
      (setq comment-start (block-comment--jump-to-comment-start))
      (setq comment-end (block-comment--jump-to-comment-end 0))
      )

    (- comment-end comment-start)
    )
  )

(defun block-comment--get-body-width (&optional prefix postfix)
  """  Returns the width of the block comment body at point, meaning the       """
  """  width between the pre/post fix                                          """
  """  Param 'prefix' : The prefix to look for                                 """
  """                   Default: block-comment-prefix                          """
  """  Param 'postfix' : The postfix to look for                               """
  """                    Default: block-comment-postfix                        """

  (unless prefix (setq prefix block-comment-prefix))
  (unless postfix (setq postfix block-comment-postfix))

  (let (
        (comment-start 0)
        (comment-end 0)
        )

    (save-excursion
      (setq comment-start (block-comment--jump-to-body-start block-comment-edge-offset
                                                             prefix))
      (setq comment-end (block-comment--jump-to-body-end block-comment-edge-offset
                                                         postfix))
      )
    (- comment-end comment-start)
    )
  )

(defun block-comment--get-comment-text-width ()
  """  Gets the width of the actual text within the block comment              """
  (let (
        (text-start 0)
        (text-end 0)
        )

    (save-excursion
      ;; Jump to first text column position
      (block-comment--jump-to-first-char-in-body)
      (setq text-start (current-column))

      ;; Jump to last text column position, no offset
      (block-comment--jump-to-last-char-in-body 0)
      (setq text-end (current-column))
      ) ;; End save-excursion

    ;; Return text width
    (- text-end text-start)
    ) ;; End let
  )

(defun block-comment--get-row-prefix-postfix ()
  (interactive)
  """  Gets the prefix & postfix based on the line type at point.             """
  """  Ret: The (prefix, postfix) of the line type at point as a cons-cell    """
  (let (
        (prefix nil)
        (postfix nil)
        )
    ;; Select the current rows pre/postfix
    (cond ((block-comment--is-body)
           (progn
             (setq prefix block-comment-prefix)
             (setq postfix block-comment-postfix))
           )
          ((block-comment--is-enclose-top)
           (progn
             (setq prefix block-comment-enclose-prefix-top)
             (setq postfix block-comment-enclose-postfix-top))
           )
          ((block-comment--is-enclose-bot)
           (progn
             (setq prefix block-comment-enclose-prefix-bot)
             (setq postfix block-comment-enclose-postfix-bot))
           )
          )

    (cons prefix postfix)
    )
  )

(defun block-comment--get-row-prefix ()
  (car (block-comment--get-row-prefix-postfix))
  )

(defun block-comment--get-row-postfix ()
  (cdr (block-comment--get-row-prefix-postfix))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                              Is x functions                               """
"""     -> Functions that check if current row is block comment of type x     """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--is-at-buffer-top ()
  """  Checks if point is at the first line in the current buffer            """
  """  -> Return: t if current pos is at the first line, else nil            """
  (equal (line-number-at-pos) 1)
  )

(defun block-comment--is-at-buffer-bot (&optional lines-in-buffer)
  """  Checks if point is at the last line in the current buffer             """
  """  Param 'lines-in-buffer' : The number of lines in the current buffer.  """
  """                            Can be sent as a parameter to minimize the  """
  """                            the number of times this value needs to be  """
  """                            calculated.                                 """
  """  -> Return: t if current pos is at the last line, else nil             """
  (unless lines-in-buffer (setq lines-in-buffer (block-comment--get-line-count-in-buffer)))
  (equal (line-number-at-pos) lines-in-buffer)
  )

(defun block-comment--is-current-line-empty ()
  (interactive)
  """ Checks if current line contains any non ' ' characters                 """
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:blank:]]*$")))

(defun block-comment--is-blank-line (&optional pos)
  """  Checks if line at pos/point is emtpy, returns t if so, else nil        """
  """  Param 'pos' : The marker position to check                             """
  """                default: (point)                                         """
  """  -> Return: t if line is blank, else nil                                """
  (save-excursion
    (goto-char (or pos (point)))
    (beginning-of-line)
    (= (point-at-eol)
       (progn
         (skip-syntax-forward " ")
         (point)
         )
       )
    )
  )

(defun block-comment--is-enclose-top (&optional inside-body)
  """  Checks if the current row follows the format of a block comment        """
  """  top enclose                                                            """
  """  Param 'inside': specifies if point is required to be inside of the     """
  """                body or not:                                             """
  """                t   -> Point must be inside the body                     """
  """                nil -> Point must be on the same row as body             """
  """                Default: nil                                             """

  (let (
        (match-signature nil)
        (has-body-beneath nil)
        (start-column (current-column))
        )

    (setq match-signature (block-comment--is-enclose block-comment-enclose-prefix-top
                                                     block-comment-enclose-fill-top
                                                     block-comment-enclose-postfix-top
                                                     inside-body))
    (save-excursion
      (forward-line 1)
      (setq has-body-beneath (block-comment--is-body))
      )

    (and match-signature has-body-beneath)
    )
  )

(defun block-comment--is-enclose-bot (&optional inside-body)
  """  Checks if the current row follows the format of a block comment        """
  """  bottom enclose                                                         """
  """  Param 'inside': specifies if point is required to be inside of the     """
  """                body or not:                                             """
  """                t   -> Point must be inside the body                     """
  """                nil -> Point must be on the same row as body             """
  """                Default: nil                                             """
  (let (
        (match-signature nil)
        (has-body-beneath nil)
        (has-body-above nil)
        )
    (setq match-signature (block-comment--is-enclose block-comment-enclose-prefix-bot
                                                     block-comment-enclose-fill-bot
                                                     block-comment-enclose-postfix-bot
                                                     inside-body))

    (save-excursion
      (forward-line 1)
      (setq has-body-beneath (block-comment--is-body))
      )

    (save-excursion
      (forward-line -1)
      (setq has-body-above (block-comment--is-body))
      )

    (and match-signature has-body-above (not has-body-beneath))
    )
  )

(defun block-comment--is-body (&optional inside-body)
  (interactive)
  """ Checks if the current row follows the format of a block comment body    """
  """  Param 'inside': specifies if point is required to be inside of the     """
  """                body or not:                                             """
  """                t   -> Point must be inside the body                     """
  """                nil -> Point must be on the same row as body             """
  """                Default: nil                                             """

  (block-comment--is-comment block-comment-prefix
                             block-comment-fill
                             block-comment-postfix
                             inside-body))

(defun block-comment--is-enclose (prefix fill postfix &optional inside-body)
  """ checks if the current row follows the format of a enclose                   """
  """ with the given prefix, fill and postfix.                                    """
  """  Param 'prefix' : The prefix to look for                                    """
  """  Param 'fill' : The fill to use                                             """
  """  Param 'postfix' : The postfix to look for                                  """

  (let (
        (is-comment (block-comment--is-comment prefix fill postfix inside-body))
        (is-enclose nil)
        (block-start nil)
        (block-end nil)
        (enclose-body nil)
        (enclose-body-template nil)
        (encountered-error nil)
        )
    (when is-comment
      (condition-case nil
          (save-excursion
            ;; Make sure there is only fill characters in-between prefix/postfix
            (beginning-of-line)
            (skip-syntax-forward " " (line-end-position))
            (forward-char (+ 1 (string-width prefix)))
            (setq block-start (point-marker))

            (end-of-line)
            (skip-syntax-backward " " (line-beginning-position))
            (backward-char (+ 2 (string-width postfix)))
            (setq block-end (point-marker))
            )
        ((end-of-buffer beginning-of-buffer) ;; TODO: Add specific handling
        ;; (error
         (setq encountered-error t)
         (block-comment--error "block-comment--is-enclose: Encountered end-of-buffer" "BC: end-of-buffer")
         )
        )

      (unless encountered-error
        (setq enclose-body (buffer-substring block-start block-end))
        (setq enclose-body-template (make-string (string-width enclose-body)
                                                 (string-to-char fill)))

        ;; If the entire enclose body contains the fill character,
        ;; the current line containes enclose, return t, else nil
        (when (string= enclose-body-template enclose-body)
          (setq is-enclose t)
          )
        )
      )
    ;; Return if this is enclose
    is-enclose
    )
  )

(defun block-comment--is-comment (prefix fill postfix &optional inside)
  (interactive)
  """ checks if the current row follows the format of a block comment body     """
  """ with the given prefix, fill and postfix.                                 """
  """  Param 'prefix' : The prefix to look for                                 """
  """  Param 'fill' : The fill to use                                          """
  """  Param 'postfix' : The postfix to look for                               """
  """  Param 'inside' specifies if point is required to be inside of the       """
  """                body or not:                                              """
  """       t   -> Point must be inside the body                               """
  """       nil -> Point must be on the same row as body                       """
  (let (
        (read-prefix-pos nil)   ;; Position of current row:s prefix
        (read-postfix-pos nil)  ;; Position of current row:s postfix
        (point-in-body t)       ;; If point is inside body.
        )
    ;; Check if prefix is present on this row
    (save-excursion
      (beginning-of-line)
      (setq read-prefix-pos
            (search-forward
             (concat prefix fill)
             (line-end-position)
             t)))

    ;; Check if postfix is present on this row
    (save-excursion
      (end-of-line)
      (setq read-postfix-pos
            (search-backward
             (concat fill postfix)
             (line-beginning-position)
             t)))

    ;; If inside-body is true, check if point is inside body
    (when (and
           inside
           read-prefix-pos
           read-postfix-pos)

      (setq point-in-body (and
                           (> (point-marker) read-prefix-pos)
                           (< (point-marker) read-postfix-pos)
                           )))

    ;; Return value, t if in block comment row, else nil
    (and read-prefix-pos
         read-postfix-pos
         point-in-body)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                         Jump to functions                                """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--jump-to-previous-text-column (&optional end)
  (interactive)
  """  Jump to the same column as the text block in the previous block        """
  """  comment row. If param end is set to t, then jump to same column        """
  """  as the end of the text block in the previous row.                      """
  """    Param 'end': If t, jump to same column as end of text block in       """
  """                 previous row, else the start                            """

  (let* (
         (prev-block-start nil)
         (prev-comment-column nil)
         (prev-text-offset nil)
         )

    ;; Get previous text block offset from comment start
    (save-excursion
      (forward-line -1)
      (block-comment--jump-to-comment-start)
      (setq prev-block-start (current-column))

      (if end
          (block-comment--jump-to-last-char-in-body)
        (block-comment--jump-to-first-char-in-body))

      (setq prev-comment-column (current-column))
      (setq prev-text-offset (- prev-comment-column prev-block-start))
      )

    ;; Move to same position on current row
    (block-comment--jump-to-comment-start)
    (forward-char prev-text-offset)
    )
  )

(defun block-comment--jump-to-comment-start (&optional prefix)
  (interactive)
  """  Jump to block comment start, the first char of the prefix               """
  """  Param 'prefix' : The prefix to look for                                 """
  """                   Default: block-comment-prefix                          """
  """  Ret: The position of the comment start                                  """

  (unless prefix (setq prefix (block-comment--get-row-prefix)))

  ;; TODO: Do we need this?
  ;; (when (> (line-number-at-pos) 0)
    (block-comment--jump-to-body-start (- 0 (string-width prefix)) prefix)
    ;; )
  (point-marker)
  )

(defun block-comment--jump-to-comment-end (&optional offset postfix)
  (interactive)
  """  Jump to block comment end, the char directly after after the postfix.    """
  """  Param 'offset': Offset can be used to move the position from the         """
  """                  default position                                         """
  """                  Default: 1                                               """
  """  Param 'postfix' : The postfix to look for                                """
  """                    Default: block-comment-postfix                         """
  """  Return: point-marker                                                     """

  (unless offset (setq offset 1))
  (unless postfix (setq postfix (block-comment--get-row-postfix)))

  (block-comment--jump-to-body-end (- 0 (+ (string-width postfix) offset)) postfix)
  (point-marker)
  )

(defun block-comment--jump-to-body-center ()
  (interactive)
  """  Jumps to the center of the block comment body and returns the end      """
  """  final column position                                                  """
  (let (
        (start-point 0)
        (end-point 0)
        (line-width 0)
        (middle-point 0)
        )

    ;; Set line width for this row
    (save-excursion

      (block-comment--jump-to-comment-start)
      (setq start-point (current-column))

      (block-comment--jump-to-comment-end)
      (setq end-point (current-column))
      )
    (setq line-width (- end-point start-point))
    (setq middle-point (/ line-width 2))

    (when (> middle-point 0)
      (block-comment--jump-to-comment-start)
      (forward-char middle-point)
      )

    (current-column)
    )
  )

(defun block-comment--jump-to-body-start (&optional edge-offset prefix)
  """  Jumps to the start of block comment body                               """
  """  Param 'edge-offset': The offset from the block comment prefix          """
  """                       Default: block-comment-edge-offset                """
  """  Param 'prefix' : The prefix to look for                                """
  """                   Default: block-comment-prefix                         """
  """  Ret : The position of the body start                                   """
  (unless edge-offset (setq edge-offset block-comment-edge-offset))
  (unless prefix (setq prefix (block-comment--get-row-prefix)))

  (let (
        (start-pos (point-marker))
        (line-end (line-end-position))
        )
    (beginning-of-line)

    ;; Place point at end of prefix if a prefix is found
    (if (search-forward prefix
                        line-end
                        t)
        (forward-char edge-offset)
      (goto-char start-pos)
      )
    )
  (point-marker)
  )

(defun block-comment--jump-to-body-end (&optional edge-offset postfix)
  """  Jumps to the end of block comment body, meaning the inside of the        """
  """  block comment, excluding the pre/postfix and the edge offset.            """
  """  Param 'edge-offset': Sets a custom edge offset, meaning the distance     """
  """                       to the postfix.                                     """
  """                       Default: block-comment-edge-offset                  """
  """  Param 'postfix' : The postfix to look for                                """
  """                    Default: block-comment-postfix                         """
  """  Ret: The position of point                                               """
  (unless edge-offset (setq edge-offset block-comment-edge-offset))
  (unless postfix (setq postfix (block-comment--get-row-postfix)))

  (let (
        (start-pos (point-marker))
        (line-start (line-beginning-position))
        )
    (end-of-line)

    ;; Place point at start of postfix if a postfix is found
    (if (search-backward postfix
                         line-start
                         t)
        (backward-char (+ edge-offset 1))
      (goto-char start-pos)
      )
    )
  (point-marker)
  )

(defun block-comment--jump-to-first-char-in-body (&optional offset)
  (interactive)
  """   Jumps to the first char in the comment body text                       """
  """   Beginning means the first non-fill character in the body               """
  """   Param: 'offset': The offset can be used to change where to jump:       """
  """                    +x -> Jump closer to postfix                          """
  """                    -x -> Jump closer to prefix                           """
  """   Ret: the number of fill characters remaining on the left side          """

  (unless offset
    (setq offset 0)
    )

  (let (
        (body-start-pos nil)   ;; Start of block-comment body
        (comment-start-pos nil);; Start of user comment
        )

    (beginning-of-line)
    ;; Find start position in block comment
    (block-comment--jump-to-body-start 0)

    ;; Set start of block-comment body
    (setq body-start-pos (current-column))

    (skip-syntax-forward " " (line-end-position))

    ;; Set start of user comment
    (setq comment-start-pos (current-column))

    (forward-char offset)

    ;; Return remaining space between user comment and start of
    ;; block-comment body
    (- comment-start-pos body-start-pos)
    )
  )

(defun block-comment--jump-to-last-char-in-body (&optional offset)
  (interactive)
  """  jumps to end of comment in body at point End means the place right     """
  """  after the last non-fill character in the body                          """
  """  Param: 'offset': Jumps to last char in body + this offset. Default = 1 """
  """  Ret: the number of fill characters remaining on the right side         """
  (let (
        (body-end-pos nil)   ;; End of block-comment body
        (comment-end-pos nil);; End of user comment
        )
    ;; Set default value
    (unless offset
      (setq offset 1)
      )

    (end-of-line)
    ;; Find end position in block comment
    (block-comment--jump-to-body-end 0)

    ;; Set end of block-comment body
    (setq body-end-pos (current-column))

    ;; Jump back to character pos right after last char in body
    (skip-syntax-backward " " (line-beginning-position))
    ;; Jump back one more to stand on last char in body
    (backward-char 1)
    ;; Jump forward by offset
    (forward-char offset)

    ;; Set end of user comment
    (setq comment-end-pos (current-column))

    ;; Return remaining space between user comment and end of block-comment body
    (- body-end-pos comment-end-pos)
    )
  )

(defun block-comment--jump-to-last-comment-row (&optional offset stop-before-postfix)
  """  Moves point down to last block comment row.                           """
  """  Param 'offset': The offset can be used to tweak the relative          """
  """                  position that point ends on:                          """
  """                      +x -> Move point x lines further down             """
  """                      -x -> Move point x lines further up               """
  """                  Default: 0                                            """
  """  Param 'stop-before-postfix': if t, stop on last block comment row     """
  """                               (before postfix)                         """
  """                   Default: nil                                         """
  (unless offset (setq offset 0))

  (let (
        (is-body nil)
        (is-enclose-top nil)
        (is-enclose-bot nil)
        (lines-in-buffer (block-comment--get-line-count-in-buffer))
        (at-bottom nil)
        )
    ;; Move to line below bottom of block comment
    (while (progn
             ;; Move down one line
             (forward-line 1)

             ;; Check if this is body or enclose
             (setq is-body (block-comment--is-body nil))
             (setq is-enclose-top (block-comment--is-enclose-top nil))
             (unless stop-before-postfix
               (setq is-enclose-bot (block-comment--is-enclose-bot nil)))

             (setq at-bottom (block-comment--is-at-buffer-bot lines-in-buffer))

             ;; Exit if not in comment or if at bottom of buffer
             (and (not at-bottom)
                  (or is-body is-enclose-top is-enclose-bot))
             )
      )

    ;; Move back up to last comment row if necessary
    (unless at-bottom
      (forward-line -1)
      )

    )

  ;; Move according to offset
  (block-comment--move-line offset)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                               General Util                                """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun block-comment--unit-tests-running ()
  (and (boundp 'block-comment--unit-tests)
       block-comment--unit-tests)
  )

(defun block-comment--message (messageStr)
  (when (not (block-comment--unit-tests-running))
    (message messageStr)
    )
  )

(defun block-comment--error (errMsg &optional errEcho)
  (when (not (block-comment--unit-tests-running))

    (unless errEcho (setq errEcho errMsg))

    ;; Format strings
    (setq errMsg (concat "Error: " errMsg))
    (setq errMsg (propertize errMsg 'face 'error))
    (setq errEcho (propertize errEcho 'face 'error))

    ;; Print echo message to Echo area in red
    (let ((message-log-max nil))
      (message errEcho)
      )

    ;; Print message to Messages buffer in red
    (with-current-buffer (messages-buffer)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert errMsg)
        )
      )
    )
  )

(provide 'block-comment-mode)

;;; block-comment-mode.el ends here
