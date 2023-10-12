#lang racket
(require swears)

;; Read characters until an ``f4'' is read
(define (input-loop)
  (let ((c #\null))
    (set! c (swears-read-character))
    (unless (eqv? c 'f4)
      (input-loop))))

;; Print ASCII characters from ``first'' to ``last''
(define (print-ascii first last)
  (define (loop i)
    (when (<= i last)
      (begin (swears-write-character (integer->char i))
             (loop (+ i 1)))))
  (loop first))

;; Draw a frame to contain a set of printable ASCII characters
(define (draw-ascii-box mode-name)
  (let ((p (swears-cursor-get-position)))
    ; Draw the main box that will border the ASCII characters
    (swears-write-graphics-box
      34
      5
      #:position (swears-position-make
                   (swears-position-get-x p)
                   (+ (swears-position-get-y p) 2)))
    ; Draw a box to hold the mode name
    (swears-write-box swgfx-line-vertical
                      swgfx-line-horizontal
                      swgfx-line-down-right
                      swgfx-line-down-left
                      swgfx-line-vertical-right
                      swgfx-line-horizontal-up
                      (+ 14 (string-length mode-name))
                      3
                      #:mode 'graphics
                      #:position p)
    ; Print the mode name
    (swears-write-string
      (string-append "VIDEO MODE: " mode-name)
      #:position (swears-position-make (+ (swears-position-get-x p) 1)
                                       (+ (swears-position-get-y p) 1)))
    ; Put the cursor in position to begin writing ASCII characters
    (swears-cursor-move
      (swears-position-make (+ (swears-position-get-x p) 1)
                            (+ (swears-position-get-y p) 3)))))

;; Write all printable ASCII characters across three lines
(define (draw-all-ascii-chars)
  (let ((p (swears-cursor-get-position)))
    (print-ascii 32 63)
    (swears-cursor-move
      (swears-position-make (swears-position-get-x p)
                            (+ (swears-position-get-y p) 1)))
    (print-ascii 64 95)
    (swears-cursor-move
      (swears-position-make (swears-position-get-x p)
                            (+ (swears-position-get-y p) 2)))
    (print-ascii 96 126)))

;; Slowly clear the screen by scrolling text upwards
(define (loop-scroll i)
  (unless (= i 0)
    (swears-write-character
             #\linefeed
             #:position (swears-position-make 0 23))
    (sleep 0.1)
    (loop-scroll (- i 1))))

;; Draw a box with a drop shadow
(define (draw-box-with-shadow width height position)
  ; Draw the box's shadow
  (swears-write-filled-box
    swgfx-box-medium-shade
    (- width 1)
    (- height 1)
    #:mode 'graphics
    #:position (swears-position-make
                 (+ (swears-position-get-x position) 1)
                 (+ (swears-position-get-y position) 1)))
  ; Draw the box outline
  (swears-write-graphics-box
    (- width 1)
    (- height 1)
    #:position position)
  ; Fill the interior of the box with whitespace
  (swears-write-filled-box
    #\space
    (- width 3)
    (- height 3)
    #:position (swears-position-make
                 (+ (swears-position-get-x position) 1)
                 (+ (swears-position-get-y position) 1))))

;;;; Setup terminal --------------------------------------------------
(swears-raw #:state 'on)
(swears-wrap #:state 'off)
(swears-25th-line #:state 'on)
(swears-cursor-visibility #:state 'off)
(swears-clear-screen)

;;;; Draw all printable characters -----------------------------------
;; Normal mode
(swears-cursor-move (swears-position-make 0 0))
(draw-ascii-box "NORMAL")
(draw-all-ascii-chars)

;; Reverse video mode
(swears-cursor-move (swears-position-make 0 8))
(draw-ascii-box "REVERSE")
(swears-video-mode #:mode 'reverse #:state 'on)
(draw-all-ascii-chars)
(swears-video-mode #:mode 'reverse #:state 'off)

;; Graphics mode
(swears-cursor-move (swears-position-make 0 16))
(draw-ascii-box "GRAPHICS")
(swears-video-mode #:mode 'graphics #:state 'on)
(draw-all-ascii-chars)
(swears-video-mode #:mode 'graphics #:state 'off)

;;;; Draw on the 25th line -------------------------------------------
(swears-write-string
  "     Press `f4' to quit at any time. (This is the 25th line of the terminal)    "
  #:mode 'reverse
  #:position (swears-position-make 0 24))

;;;; Draw boxes with drop shadows ------------------------------------
(draw-box-with-shadow 16 9 (swears-position-make 38 0))
(draw-box-with-shadow 25 14 (swears-position-make 45 4))

;;;; Draw some lines and characters ----------------------------------
(swears-write-horizontal-line #\-
                              38
                              #:position (swears-position-make 37 20))
(swears-write-horizontal-line #\=
                              40
                              #:position (swears-position-make 37 21))
(swears-write-horizontal-line swgfx-arrow-right
                              42
                              #:mode 'graphics
                              #:position (swears-position-make 37 22))

(swears-write-vertical-line #\|
                            20
                            #:position (swears-position-make 75 0))
(swears-write-vertical-line #\l
                            21
                            #:position (swears-position-make 77 0))
(swears-write-vertical-line swgfx-arrow-down
                            22
                            #:mode 'graphics
                            #:position (swears-position-make 79 0))

(swears-write-character #\+ #:position (swears-position-make 75 20))
(swears-write-character #\# #:position (swears-position-make 77 21))
(swears-write-character swgfx-circle
                        #:mode 'graphics
                        #:position (swears-position-make 79 22))

;;;; Read input and quit on ``f4'' -----------------------------------
(input-loop)

;;;; Show off the 25th line while exiting program --------------------
(swears-write-string
  "The 25th line can remain on the screen while the terminal scrolls the main text!"
  #:mode 'reverse
  #:position (swears-position-make 0 24))

;; Do a fancy scroll as we quit to move the text up while keeping the
;; 25th line stationary
(loop-scroll 24)

;;;; Reset terminal state --------------------------------------------
;; Remember that you cannot assume the settings of the terminal, so we
;; must use this to restore the terminal to its original state. This
;; means cursor visibility, and shape will be correct, whether the
;; keypad is shifted or not, whether wrap is enabled or not, etc.
;; Convieniently, it also clears the screen, so it is a useful way to
;; end a full screen TUI program.
(swears-reset-settings)
(swears-raw #:state 'off)
