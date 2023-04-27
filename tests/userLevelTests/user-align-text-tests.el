;;; -*- lexical-binding: nil; -*-

;; Turn lexical binding off to enable desired behaviour of the :var form

(add-to-list 'load-path "cask")
(require 'block-comment-mode)

(add-to-list 'load-path "tests")
(require 'block-comment-test-helpers)

(describe "Test user align text"

  (before-each
    (erase-buffer)
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                               Align no text                               """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Align no text: left -> center"
    (let(
         (start-string "
/*******************************************************************************/
/*  <p>                                                                        */
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*                                      <p>                                    */
/*******************************************************************************/
")
         )

      (insert start-string)
      (jump-to-p "   " t)

      ;; Resume block comment
      (block-comment-start)
      (block-comment-align-next)
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string "   ")
      )
    )

(it "Align no text: center -> right"
    (let(
         (start-string "
/*******************************************************************************/
/*  <p>                                                                        */
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*                                                                          <p>*/
/*******************************************************************************/
")
         )

      (insert start-string)
      (jump-to-p "   " t)

      ;; Resume block comment
      (block-comment-start)
      (forward-char 36) ;; Put point at center of comment
      (block-comment-align-next)
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string "   ")
      )
    )

(it "Align no text: right -> left"
    (let(
         (start-string "
/*******************************************************************************/
/*  <p>                                                                        */
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*  <p>                                                                        */
/*******************************************************************************/
")
         )

      (insert start-string)
      (jump-to-p "   " t)

      ;; Resume block comment
      (block-comment-start)
      (forward-char 72) ;; Put point at end of comment
      (block-comment-align-next)
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string "   ")
      )
    )

(it "Align no text: left -> left (cycle through positions)"
    (let(
         (start-string "
/*******************************************************************************/
/*  <p>                                                                        */
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*  <p>                                                                        */
/*******************************************************************************/
")
         )

      (insert start-string)
      (jump-to-p "   " t)

      ;; Resume block comment
      (block-comment-start)
      (block-comment-align-next)
      (block-comment-align-next)
      (block-comment-align-next)
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string "   ")
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                       Align single row of user text                       """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Align single row user text: left -> center"
    (let(
         (start-string "
/*******************************************************************************/
/*  This is user text.<p>                                                      */
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*                             This is user text.<p>                           */
/*******************************************************************************/
")
         )

      (insert start-string)
      (jump-to-p "   " t)

      ;; Resume block comment
      (block-comment-start)
      (block-comment-align-next)
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string "   ")
      )
    )

(it "Align single row user text: center -> right"
    (let(
         (start-string "
/*******************************************************************************/
/*                             This is user text.<p>                           */
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*                                                        This is user text.<p>*/
/*******************************************************************************/
")
         )

      (insert start-string)
      (jump-to-p "   " t)

      ;; Resume block comment
      (block-comment-start)
      (block-comment-align-next)
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string "   ")
      )
    )

(it "Align single row user text: right -> left"
    (let(
         (start-string "
/*******************************************************************************/
/*                                                        This is user text.<p>*/
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*  This is user text.<p>                                                      */
/*******************************************************************************/
")
         )

      (insert start-string)
      (jump-to-p "   " t)

      ;; Resume block comment
      (block-comment-start)
      (block-comment-align-next)
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string "   ")
      )
    )

(it "Align single row user text: left -> left (cycle through positions)"
    (let(
         (start-string "
/*******************************************************************************/
/*  This is user text.<p>                                                      */
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*  This is user text.<p>                                                      */
/*******************************************************************************/
")
         )

      (insert start-string)
      (jump-to-p "   " t)

      ;; Resume block comment
      (block-comment-start)
      (block-comment-align-next)
      (block-comment-align-next)
      (block-comment-align-next)
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string "   ")
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"""                   Align with previous row of user text                    """
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(it "Align multi row user text: left -> prev-row-start-aligned"
    (let(
         (start-string "
/*******************************************************************************/
/*                  This is user text:                                         */
/*  * User written point                                                       */<p>
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*                  This is user text:                                         */
/*                  * User written point<p>                                    */
/*******************************************************************************/
")
         )

      (insert start-string)
      (jump-to-p "   " t)

      ;; Resume block comment
      (block-comment-start)
      (block-comment-align-next)
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string "   ")
      )
    )

(it "Align multi row user text: left -> prev-row-end-aligned"
    (let(
         (start-string "
/*******************************************************************************/
/*  This is user text:                                                         */
/*  * User written point                                                       */<p>
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*  This is user text:                                                         */
/*                    * User written point<p>                                  */
/*******************************************************************************/
")
         )

      (insert start-string)
      (jump-to-p "   " t)

      ;; Resume block comment
      (block-comment-start)
      (block-comment-align-next)
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string "   ")
      )
    )

(it "Align multi row user text: left -> left (cycle through positions)"
    (let(
         (start-string "
/*******************************************************************************/
/*                  This is user text:                                         */
/*  * User written point                                                       */<p>
/*******************************************************************************/
")
         (expected-string "
/*******************************************************************************/
/*                  This is user text:                                         */
/*  * User written point<p>                                                    */
/*******************************************************************************/
")
         )

      (insert start-string)
      (jump-to-p "   " t)

      ;; Resume block comment
      (block-comment-start)
      (block-comment-align-next) ;; start of prev comment
      (block-comment-align-next) ;; Center
      (block-comment-align-next) ;; end of prev comment
      (block-comment-align-next) ;; end
      (block-comment-align-next) ;; start
      (block-comment-abort)

      ;; Check for equality
      (expect-buffer-equal expected-string "   ")
      )
    )
)
