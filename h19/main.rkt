#lang racket
(require braw)

(provide h19-cursor-home
         h19-cursor-forward
         h19-cursor-backward
         h19-cursor-down
         h19-cursor-up
         h19-cursor-position-report
         h19-save-cursor-position
         h19-set-cursor-to-previously-saved-position
         h19-direct-cursor-addressing

         h19-clear-display
         h19-erase-to-beginning-of-display
         h19-erase-to-end-of-display
         h19-erase-entire-line
         h19-erase-to-beginning-of-line
         h19-erase-to-end-of-line
         h19-insert-line
         h19-delete-line
         h19-delete-character
         h19-enter-insert-character-mode
         h19-exit-insert-character-mode

         h19-reset-to-power-up-configuration
         h19-modify-baud-rate
         h19-set-mode
         h19-reset-mode
         h19-enter-ansi-mode
         h19-enter-heath-mode

         h19-enter-hold-screen-mode
         h19-exit-hold-screen-mode
         h19-enter-reverse-video-mode
         h19-exit-reverse-video-mode
         h19-enter-graphics-mode
         h19-exit-graphics-mode
         h19-enter-keypad-shifted-mode
         h19-exit-keypad-shifted-mode
         h19-enter-alternate-keypad-mode
         h19-exit-alternate-keypad-mode

         h19-keyboard-disabled
         h19-keyboard-enabled
         h19-wrap-around-at-end-of-line
         h19-discard-at-end-of-line
         h19-transmit-25th-line
         h19-transmit-page)

(define h19-columns 80)

;; The H19 can switch between 24 and 25 rows, but the 25th row is
;; not handled like a normal row of text. For purposes of drawing
;; things like lines we ignore the 25th row since we cannot cursor
;; down to it, nor reach it with a line feed. Instead to write on
;; the 25th line, we must first enable it, manually move there with
;; direct cursor addressing, and then leave the same way. This can
;; be used useful for a status line that will not scroll with the
;; rest of the screen, potentially increasing redrawing efficiency
;; if a status line is being used.
(define h19-rows 24)

(define (h19-write-string string)
  (for-each braw-write (string->list string)))

;; When in "Heath Mode," the H19 uses two ASCII bytes when directly
;; addressing the cursor. ASCII 32 is equivalent to 0 in terms of
;; columns and rows. These two procedures convert to and from these
;; ASCII bytes and integers.
(define (h19-position-char->integer char)
  (- (char->integer char) 32))

(define (h19-integer->position-char integer)
  (integer->char (+ integer 32)))

;;; CURSOR FUNCTIONS --------------------------------------------- ;;;

(define (h19-cursor-home)
  (h19-write-string "\eH"))

(define (h19-cursor-forward)
  (h19-write-string "\eC"))

(define (h19-cursor-backward)
  (h19-write-string "\eD"))

(define (h19-cursor-down)
  (h19-write-string "\eB"))

(define (h19-cursor-up)
  (h19-write-string "\eA"))

;; Reverse index is reverse line feed, creating a new blank line
;; above the cursor and moving the cursor there. If you are at the
;; top of the screen, all text on the screen will move down one row.
(define (h19-reverse-index)
  (h19-write-string "\eI"))

(define/contract (h19-cursor-position-report)
  (-> (values exact-integer? exact-integer?))
  (h19-write-string "\en")
  (let ((line (begin (braw-read -1) (braw-read -1) (braw-read -1)))
        (column (braw-read -1)))
    (values (h19-position-char->integer line)
            (h19-position-char->integer column))))

(define (h19-save-cursor-position)
  (h19-write-string "\ej"))

(define (h19-set-cursor-to-previously-saved-position)
  (h19-write-string "\ek"))

(define/contract (h19-direct-cursor-addressing line column)
  (-> exact-integer? exact-integer? void?)
  (h19-write-string
    (string-append
      "\eY"
      (string (h19-integer->position-char line)
              (h19-integer->position-char column)))))

;;; ERASING AND EDITING ------------------------------------------ ;;;

(define (h19-clear-display)
  (h19-write-string "\eE"))

(define (h19-erase-to-beginning-of-display)
  (h19-write-string "\eb"))

(define (h19-erase-to-end-of-display)
  (h19-write-string "\eJ"))

(define (h19-erase-entire-line)
  (h19-write-string "\el"))

;; Includes the character under the cursor
(define (h19-erase-to-beginning-of-line)
  (h19-write-string "\eo"))

;; Includes the character under the cursor
(define (h19-erase-to-end-of-line)
  (h19-write-string "\eK"))

(define (h19-insert-line)
  (h19-write-string "\eL"))

(define (h19-delete-line)
  (h19-write-string "\eM"))

(define (h19-delete-character)
  (h19-write-string "\eN"))

(define (h19-enter-insert-character-mode)
  (h19-write-string "\e@"))

(define (h19-exit-insert-character-mode)
  (h19-write-string "\eO"))

;;; CONFIGURATION ------------------------------------------------ ;;;

(define (h19-reset-to-power-up-configuration)
  (h19-write-string "\ez"))

;; Rate Codes:
;; A = 110      B = 150     C = 300     D = 600     E = 1200
;; F = 1800     G = 2000    H = 2400    I = 3600    J = 4800
;; K = 7200     L = 9600    M = 19200*

;; * The H19's UART does not have a buffer and cannot keep up with
;; 19200 baud. That said, the terminal does have the ability to switch
;; to this baud rate, and could be used in some situations where the
;; user is careful not to send data too quickly.
(define/contract (h19-modify-baud-rate rate)
  (-> (lambda (x) (member x '(#\A #\B #\C #\D #\E #\F #\G
                              #\H #\I #\J #\K #\L #\M)))
      void?)
  (h19-write-string (string-append "\er" (string rate))))

;; Mode Codes:
;; 1 = enable 25th line
;; 2 = no key click
;; 3 = hold screen mode
;; 4 = block cursor
;; 5 = cursor off
;; 6 = keypad shifted
;; 7 = alternate keypad mode
;; 8 = auto LF on CR
;; 9 = auto CR on LF
(define/contract (h19-set-mode mode)
  (-> (lambda (x) (member x '(1 2 3 4 5 6 7 8 9))) void?)
  (h19-write-string (string-append "\ex" (number->string mode))))

;; MODE CODES:
;; 1 = disable 25th line
;; 2 = enable key click
;; 3 = exit hold screen mode
;; 4 = underscore cursor
;; 5 = cursor on
;; 6 = keypad unshifted
;; 7 = exit alternate keypad mode
;; 8 = no auto LF
;; 9 = no auto CR
(define/contract (h19-reset-mode mode)
  (-> (lambda (x) (member x '(1 2 3 4 5 6 7 8 9))) void?)
  (h19-write-string (string-append "\ey" (number->string mode))))

;; ``(h19-enter-heath-mode)'' is the only "ANSI mode" procedure
;; provided by this library. "Heath Mode" is faster due to its terse
;; escape codes. Some features will not be available in "ANSI Mode."
;; If the terminal is in "ANSI Mode" it is suggested that you switch
;; to "Heath Mode" at the start of your program, then return back
;; when quitting out.
(define (h19-enter-ansi-mode)
  (h19-write-string "\e<"))

(define (h19-enter-heath-mode)
  (h19-write-string "\e?2h"))

;;; MODES OF OPERATION ------------------------------------------- ;;;

(define (h19-enter-hold-screen-mode)
  (h19-write-string "\e["))

(define (h19-exit-hold-screen-mode)
  (h19-write-string "\e\\"))

(define (h19-enter-reverse-video-mode)
  (h19-write-string "\ep"))

(define (h19-exit-reverse-video-mode)
  (h19-write-string "\eq"))

(define (h19-enter-graphics-mode)
  (h19-write-string "\eF"))

(define (h19-exit-graphics-mode)
  (h19-write-string "\eG"))

(define (h19-enter-keypad-shifted-mode)
  (h19-write-string "\et"))

(define (h19-exit-keypad-shifted-mode)
  (h19-write-string "\eu"))

(define (h19-enter-alternate-keypad-mode)
  (h19-write-string "\e="))

(define (h19-exit-alternate-keypad-mode)
  (h19-write-string "\e>"))

;;; ADDITIONAL FUNCTIONS ----------------------------------------- ;;;

(define (h19-keyboard-disabled)
  (h19-write-string "\e}"))

(define (h19-keyboard-enabled)
  (h19-write-string "\e{"))

(define (h19-wrap-around-at-end-of-line)
  (h19-write-string "\ev"))

(define (h19-discard-at-end-of-line)
  (h19-write-string "\ew"))

(define (h19-identify-as-vt52)
  (h19-write-string "\eZ"))

(define (h19-transmit-25th-line)
  (h19-write-string "\e]"))

(define (h19-transmit-page)
  (h19-write-string "\e#"))
