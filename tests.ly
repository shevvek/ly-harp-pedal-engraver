%%  Add-on for GNU LilyPond: engraver that tracks, updates, and prints harp
%%  pedal markings based on accidentals as they appear.
%%
%%  Copyright (C) 2024 Saul James Tobin.
%%
%%  This program is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with this program.  If not, see <https://www.gnu.org/licenses/>.

\version "2.24.0"
\include "harp-pedal-engraver.ily"

% Examples % %

RH = \relative c'' {
  \clef treble
  <>\setHarpPedals { dis cis b e fis gis ais }
  <dis fis,>2(\p <d f,> |
  <cis e,>2 <b dis,>4\setHarpPedals { fis } <ais cis,> |
  <a fis bis,>2) r |
  <gis eis b>2 r |
  <a fis bis,>2\setHarpPedals { e } r |
  <ais e cis>4( <gis b,>\< <fis ais,> <e gis,> |
  <dis g,!>4)\! r r2 |
}

LH = \relative c {
  \clef bass
  <b b,>4 r r2 |
  R1*6 |
}

\markup\bold "from R. Strauss, Don Juan:"

\new PianoStaff \with {
  \consists #Harp_pedal_engraver
} <<
  \new Staff \RH
  \new Staff \LH
>>

% Basic usage test examples

RH = \relative c' {
  \key d \major
  % Start by completely setting the pedals
  <>_\setHarpPedals { d ces b e fis g a }
  <d fis a>1
  <d a'>1
  % Explicit and implicit pedal changes will be combined into a single marking
  <d f bes>1_\setHarpPedals { bes }
  % Simultaneous explicit pedal changes will also be combined
  <des f bes>1\setHarpPedals { des }
  R1*2
  % The order of notes entered in \setHarpPedals does not matter:
  <e g c>1\setHarpPedals { c d e fis g a b }
}

LH = \relative c {
  \clef bass
  \key d \major
  % When setting direction to CENTER, grobs will be placed underneath the top staff instead of above
  \override PianoStaff.HarpPedalChange.direction = #CENTER
  d1
  % Pitches outside the pedal setting will trigger an automatic pedal change (from either Staff)
  % Explicit pedal changes can be entered in either Staff
  f2 d2\setHarpPedals { c }
  % Explicit and implicit pedal changes will be combined into a single marking
  <c aes'>1
  % Simultaneous explicit pedal changes will also be combined
  <bes ges'>1\setHarpPedals { ges }
  % notes on the bottom fixed strings will be ignored by automatic pedal changes
  es,,1
  dis1
  ces1
}

\markup\bold "basic usage example:"

\new PianoStaff \with {
  \consists #Harp_pedal_engraver
  harpPedalStyle = #'graphical-circles
} <<
  \new Staff \RH
  \new Staff \LH
>>

% Warnings/edge cases

RH = \relative c' {
  \key bes \major
  % Incomplete initialization
  <>\setHarpPedals { d cis }
  d1
  R1-\setHarpPedals { bes c d es f g a }
  % Simultaneous contradictory explicit pedal changes
  a'1\setHarpPedals { aes }
  % Simultaneous contradictory explicit pedal changes
  g1\setHarpPedals { e }
  % An implicit pedal change is contradicted by a simultaneous explicit pedal change
  f1\setHarpPedals { cis }
  % The previous situation but order reversed triggers a different warning message
  fis1
  % Multiple pedal changes on the same side does not trigger a warning
  dis2 <e gis>2
  % No warning from duplicate or contradictory pitches in the same \setHarpPedals
  % After the first, they are just ignored
  gis2\setHarpPedals { b b } gis2\setHarpPedals { fis f }
  % Implicit or explicit accidentals other than flat/natural/sharp
  % will trigger a harpPedalSetting type warning
  gisis4 ais\setHarpPedals { beses } cisih cisih
}

LH = \relative c {
  \clef bass
  \key bes \major
  d1
  R1*2
  % Simultaneous contradictory explicit pedal changes
  bes1\setHarpPedals { eis }
  % An implicit pedal change is contradicted by a simultaneous explicit pedal change
  ces1
  % The previous situation but order reversed triggers a different warning message
  bes1\setHarpPedals { fes }
  % Multiple pedal changes on the same side does not trigger a warning
  bis2 <cis ais>2
  R1*2
}

\markup\bold "warnings and edge cases:"

\new PianoStaff \with {
  \consists #Harp_pedal_engraver
  harpPedalStyle = #'text
  \override HarpPedalChange.extra-spacing-width = #'(0 . 0)
  \override HarpPedalChart.self-alignment-X = #CENTER
} <<
  \new Staff \RH
  \new Staff \LH
>>

% Advanced use cases

testcue = \relative c' {
  s1*5
  r4 c d e
  r4 fis gis ais
}

\addQuote "test" \testcue

example = \relative c' {
  <>\setHarpPedals { d cis b e fis g a }
  <d fis a>1
  % note that you must include the context (Staff, PianoStaff, etc)
  % where Harp_pedal_engraver was consisted
  \once\set Staff.harpPedalStyle = #'graphical-circles
  <des f bes>\setHarpPedals { des c bes es f ges aes }
  \revert Staff.HarpPedalChart.format-pedal-text
  \once\override Staff.HarpPedalChange.color = #red
  b2\p_"foo" r2 |
  % unsetting harpPedalSetting can be useful before a simple
  % passage where pedal markings are not needed
  \unset Staff.harpPedalSetting
  d1 |
  \once\set Staff.harpPedalStyle = ##f
  b'1\setHarpPedals { des c b es f g aes }
  % notes in cues will trigger automatic pedal changes
  \cueDuring #"test" #DOWN {
    R1
  }
  % to avoid this, turn off autoupdate during cues
  \cueDuring #"test" #DOWN {
    \set Staff.harpPedalAutoUpdate = ##f
    R1
    \unset Staff.harpPedalAutoUpdate
  }
}

% A custom text markup function can be used to reorder the note names
% depending on the harpist's preference
% And to add formatting, enclosures, etc.
custom-pedal-text =
#(define-scheme-function (d c b e f g a)
   (markup? markup? markup? markup? markup? markup? markup?)
   "Print a text pedal marking"
   #{
     \markup\box\left-column {
       \line { $b $c $d }
       \line { $e $f $g $a }
     }
   #})

\markup\bold "advanced usage:"

% Harp_pedal_engraver functions when consisted to a Staff,
% though this is probably not the typical usage
\new Staff \with {
  \consists #Harp_pedal_engraver
  harpPedalStyle = #'text
  \override HarpPedalChart.extra-spacing-width = #'(0 . 0)
  \override HarpPedalChart.self-alignment-X = #LEFT
  % Use this syntax to set the custom formatting function
  % The same method can be used to change formatting of HarpPedalChange
  \override HarpPedalChart.format-pedal-text = #(const custom-pedal-text)
} \example
