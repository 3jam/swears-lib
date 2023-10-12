;;;; ------------------------------------------------------------ ;;;;
;;;; This module fascilitates controlling a Heathkit H19(A) terminal
;;;; to directly take advantage of some of its non-standard features.
;;;; This includes graphics characters (which are not VT-100
;;;; compatible) and the normally inaccessible 25th line useful for
;;;; status lines. This library should be compatible with the Zenith
;;;; Z19 as well, but this is untested.
;;;; ------------------------------------------------------------ ;;;;

#lang racket
(require h19
         braw)

(provide swears-position?
         swears-position-make
         swears-position-get-x
         swears-position-get-y

         swears-max-x
         swears-max-y

         swears-raw

         swears-cursor-home
         swears-cursor-right
         swears-cursor-left
         swears-cursor-down
         swears-cursor-up
         swears-cursor-move
         swears-cursor-save-position
         swears-cursor-restore-position
         swears-cursor-get-position

         swears-clear-screen
         swears-erase-to-home
         swears-erase-to-end
         swears-erase-line
         swears-erase-left
         swears-erase-right
         swears-insert-line-above
         swears-delete-line
         swears-delete-character

         swears-insert-mode
         swears-video-mode
         swears-cursor-visibility
         swears-cursor-shape
         swears-wrap
         swears-25th-line
         swears-keyboard
         swears-keypad-shifted
         swears-reset-settings

         swears-escape?
         swears-character?
         swears-read-character

         swears-write-character
         swears-write-string

         swears-write-horizontal-line
         swears-write-vertical-line
         swears-write-box
         swears-write-filled-box

         swears-write-graphics-horizontal-line
         swears-write-graphics-vertical-line
         swears-write-graphics-box

         swgfx-circle
         swgfx-triangle-upper-right
         swgfx-line-vertical
         swgfx-line-horizontal
         swgfx-line-vertical-and-horizontal
         swgfx-line-down-left
         swgfx-line-up-left
         swgfx-line-up-right
         swgfx-line-down-right
         swgfx-sign-plus-minus
         swgfx-arrow-right
         swgfx-box-medium-shade
         swgfx-sign-division
         swgfx-arrow-down
         swgfx-box-quadrant-lower-right
         swgfx-box-quadrant-lower-left
         swgfx-box-quadrant-upper-left
         swgfx-box-quadrant-upper-right
         swgfx-box-half-top
         swgfx-box-half-right
         swgfx-triangle-upper-left
         swgfx-line-horizontal-down
         swgfx-line-vertical-left
         swgfx-line-horizontal-up
         swgfx-line-vertical-right
         swgfx-line-diagonal-cross
         swgfx-line-diagonal-forward
         swgfx-line-diagonal-backward
         swgfx-line-horizontal-top
         swgfx-line-horizontal-bottom
         swgfx-line-vertical-left-side
         swgfx-line-vertical-right-side
         swgfx-pilcrow)

;; ---------------------------------------------------------------- ;;

(struct swears-position (x y))

(define/contract (swears-position-make x y)
  (-> exact-integer? exact-integer? swears-position?)
  (swears-position x y))

(define/contract (swears-position-get-x position)
  (-> swears-position? exact-integer?)
  (swears-position-x position))

(define/contract (swears-position-get-y position)
  (-> swears-position? exact-integer?)
  (swears-position-y position))

;; ---------------------------------------------------------------- ;;

(define swears-max-x 79)

;; Note that while the H19 does have a 25th line, it cannot be
;; accessed via cursor down, or line feed, so we do not include it
;; as another line for the purposes of ``swears-max-y''. Instead one
;; must manually move to and from this line using
;; ``(swears-cursor-move)'' in order to access it. The 25th line must
;; also be enabled using ``(swears-25th-line)''.
(define swears-max-y 23)

;; ---------------------------------------------------------------- ;;

(define/contract (swears-get-terminal-settings)
  (-> braw?)
  (braw-get))

;; Caches the terminal state the first time this is called.
(define swears-initial-settings
  (let ((settings #f))
    (lambda ()
      (when (false? settings)
        (set! settings (swears-get-terminal-settings)))
      settings)))

;; Note that raw mode does not disable line wrap. That is disabled
;; with ``(swears-wrap)''
(define/contract (swears-raw #:state state-flag)
  (-> #:state (or/c 'on 'off) void?)
  (cond ((eqv? state-flag 'on)
         (braw-start (swears-initial-settings)))
        ((eqv? state-flag 'off)
         (braw-stop (swears-initial-settings)))))

;; ---------------------------------------------------------------- ;;

(define (swears-cursor-home)
  (h19-cursor-home))

(define (swears-cursor-right)
  (h19-cursor-forward))

(define (swears-cursor-left)
  (h19-cursor-backward))

(define (swears-cursor-down)
  (h19-cursor-down))

(define (swears-cursor-up)
  (h19-cursor-up))

(define/contract (swears-cursor-move position)
  (-> swears-position? void?)
  (when (swears-position? position)
    (h19-direct-cursor-addressing (swears-position-get-y position)
                                  (swears-position-get-x position))))
;)

;; It is safe to save and restore the cursor position in programs that
;; use swears. This library always preserves the cursor's last saved
;; position.
(define (swears-cursor-save-position)
  (h19-save-cursor-position))

(define (swears-cursor-restore-position)
  (h19-set-cursor-to-previously-saved-position))

(define (swears-cursor-get-position)
  (-> swears-position?)
  (let-values (((line column) (h19-cursor-position-report)))
    (swears-position-make column line)))

;; ---------------------------------------------------------------- ;;

(define (swears-clear-screen)
  (h19-clear-display))

(define (swears-erase-to-home)
  (h19-erase-to-beginning-of-display))

(define (swears-erase-to-end)
  (h19-erase-to-end-of-display))

(define (swears-erase-line)
  (h19-erase-entire-line))

;; Includes character under cursor
(define (swears-erase-left)
  (h19-erase-to-beginning-of-line))

;; Includes character under cursor
(define (swears-erase-right)
  (h19-erase-to-end-of-line))

(define (swears-insert-line-above)
  (h19-insert-line))

(define (swears-delete-line)
  (h19-delete-line))

(define (swears-delete-character)
  (h19-delete-character))

;; ---------------------------------------------------------------- ;;

(define/contract (swears-insert-mode #:state state-flag)
  (-> #:state (or/c 'on 'off) void?)
  (cond ((eqv? state-flag 'on) (h19-enter-insert-character-mode))
        ((eqv? state-flag 'off) (h19-exit-insert-character-mode))))

(define/contract (swears-reverse-video-mode #:state state-flag)
  (-> #:state (or/c 'on 'off) void?)
  (cond ((eqv? state-flag 'on) (h19-enter-reverse-video-mode))
        ((eqv? state-flag 'off) (h19-exit-reverse-video-mode))))

(define/contract (swears-graphics-mode #:state state-flag)
  (-> #:state (or/c 'on 'off) void?)
  (cond ((eqv? state-flag 'on) (h19-enter-graphics-mode))
        ((eqv? state-flag 'off) (h19-exit-graphics-mode))))

(define (swears-video-mode #:mode mode-flag #:state state-flag)
  (-> #:mode (or/c 'reverse 'graphics 'both 'normal)
      #:state (or/c 'on 'off)
      void?)
  (cond ((eqv? mode-flag 'both)
         (begin (swears-video-mode #:mode 'reverse #:state state-flag)
                (swears-video-mode #:mode 'graphics #:state state-flag)))
        ((eqv? mode-flag 'reverse)
         (swears-reverse-video-mode #:state state-flag))
        ((eqv? mode-flag 'graphics)
         (swears-graphics-mode #:state state-flag))))

(define/contract (swears-cursor-visibility #:state state-flag)
  (-> #:state (or/c 'on 'off) void?)
  (cond ((eqv? state-flag 'on) (h19-reset-mode 5))
        ((eqv? state-flag 'off) (h19-set-mode 5))))

(define/contract (swears-cursor-shape #:shape shape)
  (-> #:shape (or/c 'block 'underscore) void?)
  (cond ((eqv? shape 'block) (h19-set-mode 4))
        ((eqv? shape 'underscore) (h19-reset-mode 4))))

(define/contract (swears-wrap #:state state-flag)
  (-> #:state (or/c 'on 'off) void?)
  (cond ((eqv? state-flag 'on)
         (h19-wrap-around-at-end-of-line))
        ((eqv? state-flag 'off)
         (h19-discard-at-end-of-line))))

(define/contract (swears-25th-line #:state state-flag)
  (-> #:state (or/c 'on 'off) void?)
  (cond ((eqv? state-flag 'on) (h19-set-mode 1))
        ((eqv? state-flag 'off) (h19-reset-mode 1))))

(define/contract (swears-keyboard #:state state-flag)
  (-> #:state (or/c 'on 'off) void?)
  (cond ((eqv? state-flag 'on) (h19-keyboard-enabled))
        ((eqv? state-flag 'off) (h19-keyboard-disabled))))

;; Keypad shifted allows the terminal user to use 2, 4, 6, 8 as arrow
;; keys, instead of a number pad.
(define/contract (swears-keypad-shifted #:state state-flag)
  (-> #:state (or/c 'on 'off) void?)
  (cond ((eqv? state-flag 'on) (h19-set-mode 6))
        ((eqv? state-flag 'off) (h19-reset-mode 6))))

;; We can't detect terminal settings such as cursor shape, or the
;; state of the keypad, so this will restore the terminal to the
;; settings the terminal is set to with its DIP switches
(define (swears-reset-settings)
  (h19-reset-to-power-up-configuration))

;; ---------------------------------------------------------------- ;;

;; These procedures allow movement and video modes to be toggled for
;; a single write of a string or character

(define (swears-optional-move position)
  (when (not (false? position))
    (swears-cursor-move position)))

(define/contract (swears-optional-mode procedure #:mode mode-flag)
  (-> procedure?
    #:mode (or/c 'reverse 'graphics 'both 'normal)
    void?)
  (swears-video-mode #:mode mode-flag #:state 'on)
  (procedure)
  (swears-video-mode #:mode mode-flag #:state 'off))

;; ---------------------------------------------------------------- ;;

(define *swears-escape-alist*
  '((#\S . f1)
    (#\T . f2)
    (#\U . f3)
    (#\V . f4)
    (#\W . f5)
    (#\J . erase)
    (#\P . blue)
    (#\Q . red)
    (#\R . white)
    (#\@ . IC)      ; Insert Character
    (#\L . IL)      ; Insert Line
    (#\N . DC)      ; Delete Character
    (#\M . DL)      ; Delete Line
    (#\A . up)
    (#\B . down)
    (#\D . left)
    (#\C . right)
    (#\H . home)))

;; This procedure allows us to test if we have read a symbol
;; representing an escape sequence.
(define/contract (swears-escape? v)
  (-> any/c boolean?)
  (not (false? (member v (map cdr *swears-escape-alist*)))))

;; A swears-character is either an ASCII character or one of the above
;; escapes
(define/contract (swears-character? v)
  (-> any/c boolean?)
  (or (char? v) (swears-escape? v)))

;; If #:parse-escapes isn't explicitly set to `#f', a user pressing
;; `<ESC>' will cause this procedure to potentially block for 0.091
;; seconds, while it waits to see if a special escape key was pressed.
;; If you intend to use this procedure in a time sensitive context,
;; you will unfortunately have to handle escape sequences yourself.
(define/contract
  (swears-read-character #:timeout (timeout -1.0)
                         #:parse-escapes (parse-escapes-flag #t))
  (->* ()
       (#:timeout number?
        #:parse-escapes boolean?)
       swears-character?)
  (let ((c (braw-read timeout)))
    (if (false? parse-escapes-flag)
      c
      (if (eqv? c #\u001B)
        (swears-parse-escape)
        c))))

;; Detect if one of the H19(A)'s special keys was pressed to send an
;; escape sequence, such as one of the `f' keys, or an arrow key.
;; If a valid escape sequence was sent at least as fast as 110 baud
;; (the slowest rate of the H19(A)) we assume it was done by special
;; key. We will throw out an invalid second character sent within that
;; window of time. It is possible to dupe an escape sequence by typing
;; very rapidly, but this is also true for all TTYs under linux.
(define (swears-parse-escape)
  (let* ((c (braw-read 0.1))
         (result (assoc c *swears-escape-alist*)))
    (if (false? result)
      ;; disregard the second character if it isn't a valid escape
      ;; sequence and just return the escape itself.
      #\u001B
      (cdr result))))

(define (swears-write-character character
                                #:mode (mode-flag 'normal)
                                #:position (position #f))
  (swears-optional-move position)
  (swears-optional-mode (lambda () (braw-write character))
                        #:mode mode-flag))

(define (swears-write-string string
                             #:mode (mode-flag 'normal)
                             #:position (position #f))
  (swears-optional-move position)
  (swears-optional-mode
    (lambda () (for-each braw-write (string->list string)))
    #:mode mode-flag))

;; ---------------------------------------------------------------- ;;

(define (swears-cursor-get-saved-position)
  (let ((current (swears-cursor-get-position))
        (saved (begin (swears-cursor-restore-position)
                      (swears-cursor-get-position))))
    (swears-cursor-move current)
    saved))

(define (swears-cursor-set-saved-position position)
  (let ((current (swears-cursor-get-position)))
    (swears-cursor-move position)
    (swears-cursor-save-position)
    (swears-cursor-move current)))

;; ---------------------------------------------------------------- ;;

(define (swears-write-horizontal-line character
                                      length
                                      #:mode (mode-flag 'normal)
                                      #:position (position #f))
  (swears-write-string (make-string length character)
                       #:mode mode-flag
                       #:position position))

(define (swears-write-vertical-line character
                                    length
                                    #:mode (mode-flag 'normal)
                                    #:position (position #f))
  (define (vertical-loop)
    ; We use this version of the loop when we are not on the rightmost
    ; column of the terminal when cursor movement will be consistent
    (define (fast len)
      (unless (= len 0)
        (swears-write-character character)
        (swears-cursor-down)
        (swears-cursor-left)
        (fast (- len 1))))
    ; This version of the loop is slower, because moving the cursor
    ; takes 4 bytes. It is only used on the rightmost column of the
    ; terminal
    (define (slow len pos)
      (unless (= len 0)
        (let ((below (swears-position-make (swears-position-get-x pos)
                                           (+ (swears-position-get-y pos) 1))))
          (swears-write-character character)
          (swears-cursor-move below)
          (slow (- len 1) below))))
    ; Get our cursor position and decide which version of the loop we
    ; will use
    (let ((origin (swears-cursor-get-position)))
      (if (= (swears-position-get-x origin) swears-max-x)
        (slow length origin)
        (fast length))))
  ; Wrap the loop in an optional move and graphics mode
  (swears-optional-move position)
  (swears-optional-mode (lambda () (vertical-loop))
                        #:mode mode-flag))

(define (swears-write-box vertical-char horizontal-char
                          top-left-char top-right-char
                          bottom-left-char bottom-right-char
                          width height
                          #:mode (mode-flag 'normal)
                          #:position (position #f))
  (define (write-box)
    (let ((origin (swears-cursor-get-position)))
      ; Draw top line with corners
      (swears-write-character top-left-char)
      (swears-write-horizontal-line horizontal-char (- width 2))
      (swears-write-character top-right-char)
      ; Draw right side line
      (swears-cursor-move
        (swears-position-make (- (+ (swears-position-get-x origin) width) 1)
                              (+ (swears-position-get-y origin) 1)))
      (swears-write-vertical-line vertical-char (- height 2))
      ; Draw left side line
      (swears-cursor-move (swears-position-make (swears-position-get-x origin)
                                                (+ (swears-position-get-y origin) 1)))
      (swears-write-vertical-line vertical-char (- height 2))
      ; Draw bottom line with corners
      (swears-write-character bottom-left-char)
      (swears-write-horizontal-line horizontal-char (- width 2))
      (swears-write-character bottom-right-char)))
  ; Wrap the loop in an optional move and graphics mode
  (swears-optional-move position)
  (swears-optional-mode (lambda () (write-box)) #:mode mode-flag))

(define (swears-write-filled-box character
                                 width height
                                 #:mode (mode-flag 'normal)
                                 #:position (position #f))
  (define (fill-box ht)
    (unless (= ht 0)
      (let ((origin (swears-cursor-get-position)))
        (swears-write-horizontal-line character width)
        (swears-cursor-move (swears-position-make (swears-position-get-x origin)
                                                  (+ (swears-position-get-y origin) 1)))
        (fill-box (- ht 1)))))
  (swears-optional-move position)
  (swears-optional-mode (lambda () (fill-box height)) #:mode mode-flag))

;; GRAPHICS SHORTCUTS --------------------------------------------- ;;

(define (swears-write-graphics-horizontal-line length
                                               #:position (position #f))
  (swears-write-horizontal-line swgfx-line-horizontal
                                length
                                #:mode 'graphics
                                #:position position))

(define (swears-write-graphics-vertical-line length
                                             #:position (position #f))
  (swears-write-vertical-line swgfx-line-vertical
                              length
                              #:mode 'graphics
                              #:position position))

(define (swears-write-graphics-box width height
                                   #:position (position #f))
  (swears-write-box swgfx-line-vertical swgfx-line-horizontal
                    swgfx-line-down-right swgfx-line-down-left
                    swgfx-line-up-right swgfx-line-up-left
                    width height
                    #:mode 'graphics
                    #:position position))

;;; GRAPHICS CHARACTERS ------------------------------------------ ;;;

(define swgfx-circle #\^)
(define swgfx-triangle-upper-right #\_)
(define swgfx-line-vertical #\`)
(define swgfx-line-horizontal #\a)
(define swgfx-line-vertical-and-horizontal #\b)
(define swgfx-line-down-left #\c)
(define swgfx-line-up-left #\d)
(define swgfx-line-up-right #\e)
(define swgfx-line-down-right #\f)
(define swgfx-sign-plus-minus #\g)
(define swgfx-arrow-right #\h)
(define swgfx-box-medium-shade #\i)
(define swgfx-sign-division #\j)
(define swgfx-arrow-down #\k)
(define swgfx-box-quadrant-lower-right #\l)
(define swgfx-box-quadrant-lower-left #\m)
(define swgfx-box-quadrant-upper-left #\n)
(define swgfx-box-quadrant-upper-right #\o)
(define swgfx-box-half-top #\p)
(define swgfx-box-half-right #\q)
(define swgfx-triangle-upper-left #\r)
(define swgfx-line-horizontal-down #\s)
(define swgfx-line-vertical-left #\t)
(define swgfx-line-horizontal-up #\u)
(define swgfx-line-vertical-right #\v)
(define swgfx-line-diagonal-cross #\w)
(define swgfx-line-diagonal-forward #\x)
(define swgfx-line-diagonal-backward #\y)
(define swgfx-line-horizontal-top #\z)
(define swgfx-line-horizontal-bottom #\{)
(define swgfx-line-vertical-left-side #\|)
(define swgfx-line-vertical-right-side #\})
(define swgfx-pilcrow #\~)
