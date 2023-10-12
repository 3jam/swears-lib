#lang scribble/manual

@(require scribble/racket
        (for-label racket/base
                   braw))

@title[#:version "0.1"]{@racketmodname[swears]}

@author[@author+email["Jen Mojallali" "jam@z80.computer"]]

@defmodule[swears]

@;--------------------------------------------------------------------

@section{Introduction}

This module fascilitates controlling a Heathkit H19(A) terminal to
directly take advantage of some of its non-standard features. This
includes graphics characters (which are not VT-100 compatible) and the
normally inaccessible 25th line useful for status lines. This library
should be compatible with the Zenith Z19 as well, but this is
untested.

The @racket[braw] module is required to use this module.

@;--------------------------------------------------------------------

@section{Storing and Manipulating Position Data}

The swears library provides the @racket[swears-position] datatype to
hold x and y coordinates for cursor manipulation.

@defproc[(swears-position? [v any/c?]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @racket[swears-position]
  struct. Returns @racket[#f] otherwise.

}

@defproc[(swears-position-make [x exact-integer?]
                               [y exact-integer?])
         swears-position?]{
  Returns a @racket[swears-position] struct holding the x and y
  coordinates provided.
}

@defproc[(swears-position-get-x [position position?]) exact-integer?]{
  Returns an @racket[exact-integer?] with the x coordinate of a
  @racket[swears-position] struct.
}

@defproc[(swears-position-get-y [position position?]) exact-integer?]{
  Returns an @racket[exact-integer?] with the y coordinate of a
  @racket[swears-position] struct.
}

@;--------------------------------------------------------------------

@section{Terminal Dimensions}

@defthing[swears-max-x exact-integer?]{
  Alias for 79. The final column of the display.
}

@defthing[swears-max-y exact-integer?]{
  Alias for 23. The final row of the display.

  Note that while the H19(A) does have a 25th line, it cannot be
  accessed via cursor down, or line feed, so we do not include it as
  another line for the purposes of @racket[swears-max-y]. Instead one
  must manually move to and from this line using
  @racket[swears-cursor-move] in order to access it. The 25th line
  must also be enabled using @racket[swears-25th-line].
}

@;--------------------------------------------------------------------

@section{Enabling Raw Mode}

@defproc[(swears-raw [#:state state-flag (or/c 'on 'off)]) void?]{
  Swap the terminal in and out of raw mode. The @racket[state-flag]
  argument specifies the mode:
  @itemize[@item{@racket['on] --- put the terminal in raw mode.}
           @item{@racket['off] --- put the terminal in cooked mode.}]
  Note that this does not disable line wrap, which must be manually
  disabled with @racket[swears-wrap] if you intend for it to be off.
}

@;--------------------------------------------------------------------

@section{Cursor Movement}

@defproc[(swears-cursor-home) void?]{
  Move the cursor to the 0, 0 position.
}

@defproc[(swears-cursor-right) void?]{
  Move the cursor right one column. If the cursor is in column 79 it
  will not move. Line wrap does not change the behavior of this
  procedure.
}

@defproc[(swears-cursor-left) void?]{
  Move the cursor left one column. If the cursor is in column 0 it
  will not move. Line wrap does not change the behavior of this
  procedure.
}

@defproc[(swears-cursor-down) void?]{
  Move the cursor down one line. If the cursor is on line 23 it will
  not move. Line wrap and the state of the 25th line do not change the
  behavior of this procedure. This procedure cannot move the cursor
  when it is on the 25th line.
}

@defproc[(swears-cursor-up) void?]{
  Move the cursor up one line. If the cursor is on line 0 it will not
  move. Line wrap does not change the behavior of this procedure. This
  procedure cannot move the cursor when it is on the 25th line.
}

@defproc[(swears-cursor-move [position position?]) void?]{
  Move the cursor to the position indicated by @racket[position].
}

@defproc[(swears-cursor-save-position) void?]{
  Use the H19(A)'s cursor saving feature to store the current cursor
  position in the terminal.
}

@defproc[(swears-cursor-restore-position) void?]{
  Use the H19(A)'s cursor saving feature to restore the cursor to the
  most recently saved position stored by the terminal.
}

@defproc[(swears-cursor-get-position) swears-position?]{
  Returns a @racket[swears-position] with the current location of the cursor
}

@;--------------------------------------------------------------------

@section{Erasing Text}

@defproc[(swears-clear-screen) void?]{
  Clear the entire display. Moves the cursor to 0, 0.
}

@defproc[(swears-erase-to-home) void?]{
  Clear the screen from the current cursor position to 0, 0. Does not
  move the cursor.
}

@defproc[(swears-erase-to-end) void?]{
  Clear the screen from the current cursor position to 79, 23. The
  25th line is unaffected. Does not move the cursor.
}

@defproc[(swears-erase-line) void?]{
  Clear the line the cursor is currently on. Does not move the cursor.
}

@defproc[(swears-erase-left) void?]{
  Clear from the character the cursor is on to the first column of the
  display. Does not move the cursor.
}

@defproc[(swears-erase-right) void?]{
  Clear from the character the cursor is on to the last column of the
  display. Does not move the cursor.
}

@defproc[(swears-insert-line-above) void?]{
  Create a new blank line above the cursor and move the cursor to the
  first column of the new line. Scrolls all lines below down one line.
  The 25th line, if enabled, will not scroll.
}

@defproc[(swears-delete-line) void?]{
  Delete the line the cursor is currently on and move the cursor to
  the first column of the line below the starting cursor position.
  Scrolls all lines below the cursor up one line. The 25th line, if
  enabled, will not scroll.
}

@defproc[(swears-delete-character) void?]{
  Delete the character the cursor is on. All text right of the cursor
  will move left one position. The line below will not move, even if
  text has wrapped onto that line.
}

@;--------------------------------------------------------------------

@section{Terminal Settings}

Use these procedures to change the settings of the terminal.

Because we have no way to get the terminal to report these settings
(as we do with cursor position) if a certain setting is preferred, it
must be explicitly enabled. If you modify these settings, you should
reset them when the program terminates using
@racket[(swears-reset-settings)].

@defproc[(swears-insert-mode
           [#:state state-flag (or/c 'on 'off)])
           void?]{
  Toggle the inserting of characters. The @racket[state-flag] argument
  specifies if the characters will be inserted:
  @itemize[
    @item{@racket['on] --- insert mode is on. Characters will be
          inserted before the cursor, and the rest of the line will
          move to the right. Regardless of the state of line wrapping,
          characters will be pushed off the right side of the screen
          if the line is full.}
    @item{@racket['off] --- insert mode if off. Any new characters
          will overwrite existing characters.}]
}

@defproc[(swears-video-mode
           [#:mode mode-flag (or/c 'reverse 'graphics 'both)]
           [#:state state-flag (or/c 'on 'off)])
           void?]{
  Toggle video modes on and off. The @racket[mode-flag] determines
  which video mode(s) will be modified:
  @itemize[
    @item{@racket['reverse] --- inverse video mode.}
    @item{@racket['graphics] --- graphics mode.}
    @item{@racket['both] --- both inverse video mode and graphics
          modes will be set.}]
  The @racket[state-flag] determines if the video mode(s) will be
  enabled or disabled:
  @itemize[
    @item{@racket['on] --- enable selected video mode.}
    @item{@racket['off] --- disable selected video mode.}]

  Note that handy bindings are provided to enable easier use of
  graphics characters. See @secref["bindings"].
}

@defproc[(swears-cursor-visibility
           [#:state state-flag (or/c 'on 'off)])
           void?]{
  Toggle the cursor visibility. The @racket[state-flag] argument
  specifies if the cursor will be visible:
  @itemize[
    @item{@racket['on] --- cursor is visible.}
    @item{@racket['off] --- cursor is not visible.}]
}

@defproc[(swears-cursor-shape
           [#:shape shape (or/c 'block 'underscore)])
           void?]{
  Change the cursor shape. The @racket[shape] argument specifies which
  shape the cursor will be:
  @itemize[
    @item{@racket['block] --- the cursor will be block shaped.}
    @item{@racket['underscore] --- the cursor will be an underscore.}]
}

@defproc[(swears-wrap
           [#:state state-flag (or/c 'on 'off)])
           void?]{
  Toggle line wrap. The @racket[state-flag] argument specifies whether
  line wrap is enabled:
  @itemize[
    @item{@racket['on] --- line wrap is enabled.}
    @item{@racket['off] --- line wrap is disabled.}]
}

@defproc[(swears-25th-line
           [#:state state-flag (or/c 'on 'off)])
           void?]{
  Toggle the 25th line. The @racket[state-flag] argument specifies
  whether the 25th line is enabled:
  @itemize[
    @item{@racket['on] --- the 25th line is enabled.}
    @item{@racket['off] --- the 25th line is disabled.}]
}

@defproc[(swears-keyboard
           [#:state state-flag (or/c 'on 'off)])
           void?]{
  Toggle whether the terminal will send characters when keys are
  pressed. The @racket[state-flag] argument specifies whether the
  keyboard is enabled:
  @itemize[
    @item{@racket['on] --- the keyboard is enabled.}
    @item{@racket['off] --- the keyboard is disabled.}]
}

@defproc[(swears-keypad-shifted
           [#:state state-flag (or/c 'on 'off)])
           void?]{
  Toggle the terminal's number pad between typing escape sequences and
  numbers. The @racket[state-flag] argument specifies whether the
  keypad is shifted:
  @itemize[
    @item{@racket['on] --- the keypad sends escape sequences.}
    @item{@racket['off] --- the keypad sends numbers.}]
}

@defproc[(swears-reset-settings) void?]{
  Restore the terminal to its power-up state. The screen is cleared
  and the cursor is moved to 0, 0. This will return any of the
  settings modified in this section to the state selected by the DIP
  switches within the terminal.
}

@;--------------------------------------------------------------------

@section{Reading Terminal Input}

@defproc[(swears-escape? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is one of 18 symbols
  representing special keys pressed on the keyboard which can be
  returned by @racket[swears-read-character]. Returns
  @racket[#f] otherwise.
}

@defproc[(swears-character? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is either a
  @racket[swears-escape?] or @racket[char?]. Returns @racket[#f]
  otherwise.
}

@defproc[(swears-read-character
           [#:timeout timeout number? -1.0]
           [#:parse-escapes parse-escapes-flag boolean? #t])
         swears-character?]{
  Return a character read from the terminal.
  If @racket[timeout] is greater than zero, this procedure will block
  for @racket[timeout] seconds, or until a character is read from the
  terminal.
  If @racket[timeout] is zero, this procedure will return immediately.
  If @racket[timeout] is less than zero, this procedure will block
  indefinitely while waiting for a character to be read from the
  terminal. This is the default behavior.

  If a special key on the keyboard is pressed when
  @racket[parse-escapes-flag] is @racket[#t] this procedure will
  return a symbol indicating which special key was pressed. A user
  pressing @racket[<ESC>] will cause this procedure to potentially
  block for 0.091 seconds while it waits to see if a special escape
  key was pressed. If you intend to use this procedure in a time
  sensitive context, you will unfortunately have to handle escape
  sequences yourself by setting @racket[parse-escapes-flag] to
  @racket[#f].

  Potential symbols are:
  @tabular[#:style 'boxed
           #:row-properties '(bottom-border ())
    (list
      (list @tt{symbol}       @tt{escape sequence})
      (list @racket['f1]      @litchar{\eS})
      (list @racket['f2]      @litchar{\eT})
      (list @racket['f3]      @litchar{\eU})
      (list @racket['f4]      @litchar{\eV})
      (list @racket['f5]      @litchar{\eW})
      (list @racket['erase]   @litchar{\eJ})
      (list @racket['blue]    @litchar{\eP})
      (list @racket['red]     @litchar{\eQ})
      (list @racket['white]   @litchar{\eR})
      (list @racket['IC]      @litchar{\e@"@"})
      (list @racket['IL]      @litchar{\eL})
      (list @racket['DC]      @litchar{\eN})
      (list @racket['DL]      @litchar{\eM})
      (list @racket['up]      @litchar{\eA})
      (list @racket['down]    @litchar{\eB})
      (list @racket['left]    @litchar{\eD})
      (list @racket['right]   @litchar{\eC})
      (list @racket['home]    @litchar{\eH}))]
}

@;--------------------------------------------------------------------

@section[#:tag "writing"]{Writing to the Terminal}

When possible, procedures that write to the terminal provide a means
of switching video modes and moving the cursor prior to writing.

The @italic{@racket[mode-flag]} argument specifies what video modes if
any will be enabled when writing to the terminal:

@itemize[
  @item{@racket['normal] --- the default video mode. This is set by
        default and doesn't need to be specified by the caller.}
  @item{@racket['graphics] --- graphics mode.}
  @item{@racket['reverse] --- reverse video mode.}
  @item{@racket['both] --- both graphics mode and reverse video
        mode.}]

The @italic{@racket[position]} argument specifies where the cursor
will be moved prior to writing to the terminal. It is set to
@racket[#f] by default, and need only be set to a @racket[position?]
object if the caller intends to move the cursor prior to writing to
the screen.

@subsection{Characters and Strings}

@defproc[(swears-write-character
           [character char?]
           [#:mode mode-flag (or/c 'normal 'graphics 'reverse 'both) 'normal]
           [#:position position (or/c position? #f) #f]) void?]{
  Send a character to the terminal. The @racket[mode-flag] and
  @racket[position] arguments are documented in @secref["writing"].
}

@defproc[(swears-write-string [string string?]
           [#:mode mode-flag (or/c 'normal 'graphics 'reverse 'both) 'normal]
           [#:position position (or/c position? #f) #f]) void?]{
  Send a string to the terminal. The @racket[mode-flag] and
  @racket[position] arguments are documented in @secref["writing"].
}

@subsection{Shapes}

@defproc[(swears-write-horizontal-line
           [character char?]
           [length exact-integer?]
           [#:mode mode-flag (or/c 'normal 'graphics 'reverse 'both) 'normal]
           [#:position position (or/c position? #f) #f]) void?]{
  Write a horizontal line of length @racket[length] using the
  character @racket[character]. The line is drawn from left to right.
  The @racket[mode-flag] and @racket[position] arguments are
  documented in @secref["writing"].

}

@defproc[(swears-write-vertical-line
           [character char?]
           [length exact-integer?]
           [#:mode mode-flag (or/c 'normal 'graphics 'reverse 'both) 'normal]
           [#:position position (or/c position? #f) #f]) void?]{
  Write a vertical line of length @racket[length] using the character
  @racket[character]. The line is drawn from the top to bottom. The
  @racket[mode-flag] and @racket[position] arguments are documented in
  @secref["writing"].
}

@defproc[(swears-write-box
           [vertical-char char?]
           [horizontal-char char?]
           [top-left-char char?]
           [top-right-char char?]
           [bottom-left-char char?]
           [bottom-right-char char?]
           [width exact-integer?]
           [height exact-integer?]
           [#:mode mode-flag (or/c 'normal 'graphics 'reverse 'both) 'normal]
           [#:position position (or/c position? #f) #f]) void?]{
  Draw a box using the characters and dimensions provided. The box is
  drawn from the top left. The @racket[mode-flag] and
  @racket[position] arguments are documented in @secref["writing"].
}

@defproc[(swears-write-filled-box
           [character char?]
           [width exact-integer?]
           [height exact-integer?]
           [#:mode mode-flag (or/c 'normal 'graphics 'reverse 'both) 'normal]
           [#:position position (or/c position? #f) #f]) void?]{
  Draw a filled box using the character and dimensions provided. The
  box is drawn from the top left. The @racket[mode-flag] and
  @racket[position] arguments are documented in @secref["writing"].
}

@subsection{Graphical Shapes}
The following procedures provide shortcuts to draw commonly used
shapes in graphics mode.

@defproc[(swears-write-graphics-horizontal-line
           [length exact-integer?]
           [#:position position (or/c position? #f) #f]) void?]{
  An alias for @racket[swears-write-horizontal-line] using the
  H19(A)'s box drawing characters. The @racket[position] argument is
  documented in @secref["writing"].
}

@defproc[(swears-write-graphics-vertical-line
           [length exact-integer?]
           [#:position position (or/c position? #f) #f]) void?]{
  An alias for @racket[swears-write-vertical-line] using the H19(A)'s
  box drawing characters. The @racket[position] argument is documented
  in @secref["writing"].
}

@defproc[(swears-write-graphics-box
           [width exact-integer?]
           [height exact-integer?]
           [#:position position (or/c position? #f) #f]) void?]{
  An alias for @racket[swears-write-box] using the H19(A)'s box
  drawing characters. The @racket[position] argument is documented in
  @secref["writing"].
}

@;--------------------------------------------------------------------

@section[#:tag "bindings"]{Graphics Character Bindings}
The following bindings are aliases for the characters in the second
column. In graphics mode, the unicode character in the third column
will be drawn.

@tabular[#:style 'boxed
         #:row-properties '(bottom-border ())
  (list
    (list @tt{symbol}                                   @tt{character}      @tt{graphics})
    (list @racket[swgfx-circle]                         @litchar{#\^}       @"\u25CF")
    (list @racket[swgfx-triangle-upper-right]           @litchar{#\_}       @"\u25E5")
    (list @racket[swgfx-line-vertical]                  @litchar{#\`}       @"\u2502")
    (list @racket[swgfx-line-horizontal]                @litchar{#\a}       @"\u2500")
    (list @racket[swgfx-line-vertical-and-horizontal]   @litchar{#\b}       @"\u253C")
    (list @racket[swgfx-line-down-left]                 @litchar{#\c}       @"\u2510")
    (list @racket[swgfx-line-up-left]                   @litchar{#\d}       @"\u2518")
    (list @racket[swgfx-line-up-right]                  @litchar{#\e}       @"\u2514")
    (list @racket[swgfx-line-down-right]                @litchar{#\f}       @"\u250C")
    (list @racket[swgfx-sign-plus-minus]                @litchar{#\g}       @"\u00B1")
    (list @racket[swgfx-arrow-right]                    @litchar{#\h}       @"\u2192")
    (list @racket[swgfx-box-medium-shade]               @litchar{#\i}       @"\u2592")
    (list @racket[swgfx-sign-division]                  @litchar{#\j}       @"\u00F7")
    (list @racket[swgfx-arrow-down]                     @litchar{#\k}       @"\u2193")
    (list @racket[swgfx-box-quadrant-lower-right]       @litchar{#\l}       @"\u2597")
    (list @racket[swgfx-box-quadrant-lower-left]        @litchar{#\m}       @"\u2596")
    (list @racket[swgfx-box-quadrant-upper-left]        @litchar{#\n}       @"\u2598")
    (list @racket[swgfx-box-quadrant-upper-right]       @litchar{#\o}       @"\u259D")
    (list @racket[swgfx-box-half-top]                   @litchar{#\p}       @"\u2580")
    (list @racket[swgfx-box-half-right]                 @litchar{#\q}       @"\u2590")
    (list @racket[swgfx-triangle-upper-left]            @litchar{#\r}       @"\u25E4")
    (list @racket[swgfx-line-horizontal-down]           @litchar{#\s}       @"\u252C")
    (list @racket[swgfx-line-vertical-left]             @litchar{#\t}       @"\u2524")
    (list @racket[swgfx-line-horizontal-up]             @litchar{#\u}       @"\u2534")
    (list @racket[swgfx-line-vertical-right]            @litchar{#\v}       @"\u251C")
    (list @racket[swgfx-line-diagonal-cross]            @litchar{#\w}       @"\u2573")
    (list @racket[swgfx-line-diagonal-forward]          @litchar{#\x}       @"\u2571")
    (list @racket[swgfx-line-diagonal-backward]         @litchar{#\y}       @"\u2572")
    (list @racket[swgfx-line-horizontal-top]            @litchar{#\z}       @"\u2594")
    (list @racket[swgfx-line-horizontal-bottom]         @litchar{#\@"{"}    @"\u2581")
    (list @racket[swgfx-line-vertical-left-side]        @litchar{#\|}       @"\u258F")
    (list @racket[swgfx-line-vertical-right-side]       @litchar{#\@"}"}    @"\u2595")
    (list @racket[swgfx-pilcrow]                        @litchar{#\~}       @"\u00B6"))
]
