#lang scribble/manual

@(require scribble/racket
        (for-label racket/base
                   braw))

@title[#:version "0.1"]{@racketmodname[h19]}

@author[@author+email["Jen Mojallali" "jam@z80.computer"]]

@defmodule[h19]

@;--------------------------------------------------------------------

@section{Introduction}

This is the low-level API for @racket[swears]. The @racket[swears]
library should be all that is needed for most prorgrams, but this
library is available for the rare case when you need more direct
control over the terminal.

Aside from @racket[h19-cursor-position-report] and
@racket[h19-direct-cursor-addressing] whose differences are documented
below, all procedures are derived from the H19(A) manual. Please look
there for detailed descriptions of the behavior of these procedures.

@;--------------------------------------------------------------------
@section{Cursor Functions}

@defproc[(h19-cursor-home) void?]{
}

@defproc[(h19-cursor-forward) void?]{
}

@defproc[(h19-cursor-backward) void?]{
}

@defproc[(h19-cursor-down) void?]{
}

@defproc[(h19-cursor-up) void?]{
}

@defproc[(h19-cursor-position-report) (values exact-integer?
                                              exact-integer?)]{
  @bold{NOTE!} This procedure has slightly different behavior than
  what is documented in the manual:

  When in "Heath Mode," the H19(A) uses two bytes to indicate the
  coordinates of the cursor on the screen, starting with the byte 0x20
  being the first column/row. This procedure converts the two bytes
  returned by the terminal to @racket[exact-integer?]s (0-79)
  automatically, so no conversion need be done by the caller.
}

@defproc[(h19-save-cursor-position) void?]{
}

@defproc[(h19-set-cursor-to-previously-saved-position) void?]{
}

@defproc[(h19-direct-cursor-addressing [line exact-integer?]
                                       [column exact-integer?])
                                       void?]{
  @bold{NOTE!} This procedure has slightly different behavior than
  what is documented in the manual:

  When in "Heath Mode," the H19(A) uses two bytes to indicate the
  coordinates of the cursor on the screen, starting with the byte 0x20
  being the first column/row. This procedure converts two
  @racket[exact-integer?] arguments (0-79) to the correct bytes
  automatically, so no conversion need be done by the caller.
}

@;--------------------------------------------------------------------
@section{Erasing and Editing}

@defproc[(h19-clear-display) void?]{
}

@defproc[(h19-erase-to-beginning-of-display) void?]{
}

@defproc[(h19-erase-to-end-of-display) void?]{
}

@defproc[(h19-erase-entire-line) void?]{
}

@defproc[(h19-erase-to-beginning-of-line) void?]{
}

@defproc[(h19-erase-to-end-of-line) void?]{
}

@defproc[(h19-insert-line) void?]{
}

@defproc[(h19-delete-line) void?]{
}

@defproc[(h19-delete-character) void?]{
}

@defproc[(h19-enter-insert-character-mode) void?]{
}

@defproc[(h19-exit-insert-character-mode) void?]{
}

@;--------------------------------------------------------------------
@section{Configuration}

@defproc[(h19-reset-to-power-up-configuration) void?]{
}

@defproc[(h19-modify-baud-rate
           [rate (lambda (x) (member x '(#\A #\B #\C #\D #\E #\F #\G
                                         #\H #\I #\J #\K #\L #\M)))])
           void?]{
}

@defproc[(h19-set-mode [mode (lambda (x) (member x '(1 2 3 4 5 6 7 8 9)))]) void?]{
}

@defproc[(h19-reset-mode [mode (lambda (x) (member x '(1 2 3 4 5 6 7 8 9)))]) void?]{
}

@defproc[(h19-enter-ansi-mode) void?]{
}

@defproc[(h19-enter-heath-mode) void?]{
}

@;--------------------------------------------------------------------
@section{Modes of Operation}

@defproc[(h19-enter-hold-screen-mode) void?]{
}

@defproc[(h19-exit-hold-screen-mode) void?]{
}

@defproc[(h19-enter-reverse-video-mode) void?]{
}

@defproc[(h19-exit-reverse-video-mode) void?]{
}

@defproc[(h19-enter-graphics-mode) void?]{
}

@defproc[(h19-exit-graphics-mode) void?]{
}

@defproc[(h19-enter-keypad-shifted-mode) void?]{
}

@defproc[(h19-exit-keypad-shifted-mode) void?]{
}

@defproc[(h19-enter-alternate-keypad-mode) void?]{
}

@defproc[(h19-exit-alternate-keypad-mode) void?]{
}

@;--------------------------------------------------------------------
@section{Additional Functions}

@defproc[(h19-keyboard-disabled) void?]{
}

@defproc[(h19-keyboard-enabled) void?]{
}

@defproc[(h19-wrap-around-at-end-of-line) void?]{
}

@defproc[(h19-discard-at-end-of-line) void?]{
}

@defproc[(h19-transmit-25th-line) void?]{
}

@defproc[(h19-transmit-page) void?]{
}
