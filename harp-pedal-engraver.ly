\version "2.24.0"

#(use-modules ((ice-9 list) #:select (rassoc))
   (ice-9 receive))

% Boilerplate (should be factored out to a library)

% taken from "scm/define-context-properties.scm"
#(define (translator-property-description symbol type? description)
   (if (not (and
             (symbol? symbol)
             (procedure? type?)
             (string? description)))
       (throw 'init-format-error))
   (if (not (equal? #f (object-property symbol 'translation-doc)))
       (ly:error (_ "symbol ~S redefined" symbol)))
   (set-object-property! symbol 'translation-type? type?)
   (set-object-property! symbol 'translation-doc description)
   (set! all-translation-properties (cons symbol all-translation-properties))
   symbol)

% from https://extending-lilypond.gitlab.io/en/extending/properties-types.html
#(define (define-event! type properties)
   (set-object-property! type
     'music-description
     (cdr (assq 'description properties)))
   (set! properties (assoc-set! properties 'name type))
   (set! properties (assq-remove! properties 'description))
   (hashq-set! music-name-to-property-table type properties)
   (set! music-descriptions
         (sort (cons (cons type properties)
                 music-descriptions)
           alist<?)))

% Event definitions

#(define-event-class 'harp-pedal-event 'music-event)

#(define-event!
  'HarpPedalEvent
  '((description . "Harp pedal change")
    (types . (post-event harp-pedal-event event))))

% Predicates

#(define (harp-pedal-alist? s)
   "Type predicate: pitch-alist with correct L-R harp pedal order and valid accidentals"
   (and (alist? s)
        (equal?
         (map car s)
         '(1 0 6 2 3 4 5))
        (every
         (lambda (el)
           (or (eqv? el -1/2)
               (eqv? el 0)
               (eqv? el 1/2)))
         (map cdr s))))

% Helper functions

% from scm/chord-name.scm
#(define (note-name-int->string pitch . language)
   "Return pitch string for @var{pitch}, without accidentals or octaves.
Current input language is used for pitch names, except if an
other @var{language} is specified."
   ;; See also note-name->lily-string if accidentals are needed.
   (let* ((pitch-alist
           (if (null? language) pitchnames
               (assoc-get (car language)
                 language-pitch-names '())))
          (result (rassoc pitch
                    (filter  (lambda (p)
                               ;; TODO: add exception for German B?
                               (eq? (ly:pitch-alteration (cdr p)) 0))
                             pitch-alist)
                    (lambda (a b)
                      (= a ; Modified to allow int argument
                         (ly:pitch-notename b))))))
     (if result (symbol->string (car result)))))

#(define (pitch-list->alist pitches)
   "Transforms a list of ly:pitch?'s into a pitch-alist"
   (map (lambda (p) `(,(ly:pitch-notename p) . ,(ly:pitch-alteration p)))
     pitches))

#(define (pitch-class->markup p)
   "Expects @var{p} to be an element of a pitch-alist. Returns as markup
note name + accidental, capitalized and including naturals. Concats accidentals
to remove the extra space before flats produced by note-name->markup."
   (if (and (pair? p) (number? (cdr p)))
       (let ((name (car p))
             (alt (cdr p)))
         (markup #:concat
           ((string-capitalize (note-name-int->string name))
            (alteration->text-accidental-markup alt))))
       empty-markup))

#(define (update-alist! alist1 alist2)
   (set! alist1
         ; Order matters if alist2 has keys missing from alist1
         (fold-right (lambda (el2 lst)
                 (assoc-set! lst (car el2) (cdr el2)))
           alist1
           alist2))
   alist1)

#(define (alteration->pedal-character alt)
   (case alt
     ((-1/2) "^")
     ((0) "-" )
     ((1/2) "v")
     (else "")))

#(define (change->circled-pedal-character pedal-alist change)
   "Expects @var{pedal-alist} to be a harp-pedal-alist?, and @var{change} to be 
a pair (notename . alteration) where alteration can be -1/2, 0, 1/2, or #f."
   (let ((new-alt (cdr change))
         (old-alt (assoc-get (car change) pedal-alist)))
     (if (and new-alt
              (not (eqv? new-alt old-alt)))
         (string-append "o" (alteration->pedal-character new-alt))
         (alteration->pedal-character old-alt))))

text-pedal-change =
#(define-scheme-function (d c b e f g a)
   (markup? markup? markup? markup? markup? markup? markup?)
   "Print a text pedal marking"
   #{
     \markup\left-column {
       \line { $e $f $g $a }
       \line { $d $c $b }
     }
   #})

% Context properties

#(translator-property-description 'harpPedalSetting harp-pedal-alist? "The current harp pedal setting. Must be a pitch-alist 
in L-R order of the harp pedals, containing only 1/2, 0, and -1/2 alterations.")
#(translator-property-description 'harpPedalStyle symbol?
   "Valid options: 'graphical, 'text, 'hybrid-circles, 'hybrid (default). With 'hybrid, incremental changes are printed as text, 
while resets of all 7 pedals are printed graphically. 'hybrid-circles is the same but prints diagrams with circles around changes.")
#(translator-property-description 'harpPedalAutoUpdate boolean?
   "If #t (default), check notes against the current pedal setting and print changes automatically.")
#(translator-property-description 'harpPedalTextMarkup procedure?
   "Scheme function accepting 7 markup arguments in L-R pedal order, returning a markup that formats text pedal markings.")
#(translator-property-description 'harpPedalGraphicMarkup procedure?
   "Scheme function accepting 1 markup and returning a markup. By default, simply returns the original markup. 
Use this for example to add an enclosure around graphical pedal markings.")

% Engraver

#(define (Harp_pedal_engraver context)
   "Listens for harp-pedal-events and prints formatted harp pedal markings. Keeps track 
of the pedal setting and automatically updates the pedal setting when new accidentals occur,
printing the changes. The pedal markings are TextScript grobs."
   (let ((change-alist '((1 . #f) (0 . #f) (6 . #f) (2 . #f) (3 . #f) (4 . #f) (5 . #f)))
         (ev #f)
         (notes '()))

     (define (pedals-graphic change->character)
       "Splits change-alist into left and right pedals, maps onto each the procedure 
change->character, then concats the resulting strings and returns the resulting harp-pedal markup. 
change->character should take (notename . alteration) as an argument and return a string."
       (receive (left right)
         (partition (lambda (el)
                      (or (= 1 (car el))
                          (= 0 (car el))
                          (= 6 (car el))))
           change-alist)
         (markup #:harp-pedal (string-append
                               (string-concatenate (map change->character left))
                               "|"
                               (string-concatenate (map change->character right))))))

     (make-engraver
      (listeners
       ((harp-pedal-event engraver event)
        (let ((new-changes (ly:event-property event 'pitch-alist)))
          (when (any
                 (lambda (el)
                   (let ((old-alt (assoc-get (car el) change-alist)))
                     (and old-alt
                          (not (eqv? old-alt (cdr el))))))
                 new-changes)
            (ly:input-warning (ly:event-property event 'origin (*location*))
              "Harp_pedal_engraver: simultaneous contradictory pedal change received."))

          (update-alist! change-alist new-changes)
          (set! ev event)))

       ((note-event engraver event)
        (when (ly:context-property context 'harpPedalAutoUpdate #t)
          (let* ((p (ly:event-property event 'pitch))
                 (name (ly:pitch-notename p))
                 (alt (ly:pitch-alteration p))
                 (setting-alist (ly:context-property context 'harpPedalSetting #f)))
            (set! notes (cons event notes))
            (when (and setting-alist
                       (not (eqv? alt
                                  (assoc-get name setting-alist))))
              (let ((pedal-event (ly:make-stream-event
                                  (ly:make-event-class 'harp-pedal-event)
                                  `((pitch-alist . ,`((,name . ,alt)))
                                    (origin . ,(ly:event-property event 'origin))
                                    (event-cause . ,event)))))
                (ly:broadcast (ly:context-event-source context) pedal-event)))))))

      ((process-music engraver)
       ; If we got a pedal event this time step, print it. The event-cause will be the most recent event received.
       (when ev
         (let* ((setting-alist (ly:context-property context 'harpPedalSetting))
                (grob (ly:engraver-make-grob engraver 'TextScript ev))
                (style (ly:context-property context 'harpPedalStyle 'hybrid))
                (reset? (every cdr change-alist))
                (text-output (ly:context-property context 'harpPedalTextMarkup text-pedal-change))
                (graphical-output (ly:context-property context 'harpPedalGraphicMarkup identity))
                (change-markup (cond
                                ((or (eqv? style 'text)
                                     (and (or (eqv? style 'hybrid)
                                              (eqv? style 'hybrid-circles))
                                          (not reset?)))
                                 ; doing it this way assumes that all the alist operations preserve order
                                 (apply text-output
                                   (map pitch-class->markup change-alist)))

                                ; If style is graphical, or hybrid-circles and this is a full reset
                                ; Circle changes as long as we have a previous pedal setting
                                ((and (pair? setting-alist)
                                      (or (eqv? style 'graphical)
                                          (and reset? (eqv? style 'hybrid-circles))))
                                 (graphical-output
                                  (pedals-graphic (lambda (change)
                                                    (change->circled-pedal-character setting-alist change)))))

                                ; In hybrid style don't circle changes for full resets
                                (else
                                 (graphical-output
                                  (pedals-graphic (compose alteration->pedal-character cdr)))))))
           (ly:grob-set-property! grob 'text change-markup)
           (ly:grob-set-property! grob 'after-line-breaking ly:side-position-interface::move-to-extremal-staff)
           (ly:grob-set-property! grob 'outside-staff-priority 500)
           (ly:grob-set-property! grob 'direction (ly:event-property ev 'direction DOWN))
           ; Update the pedal setting based on change-alist
           ; Do this after printing the changes, in case we need to print circles based on the last setting
           ; Default value of '() allows pedal setting to be initialized
           ; Check first for incomplete initialization to avoid duplicate warnings
           (if (and (not reset?)
                    (null? setting-alist))
               (ly:input-warning (ly:event-property ev 'origin (*location*))
                 "Harp_pedal_engraver: received incomplete initial pedal setting")
               (ly:context-set-property! context 'harpPedalSetting
                 (update-alist! setting-alist (filter cdr change-alist))))
           ))

       ; Don't warn about contradictory notes if the user is taking responsibility for pedal markings
       (when (ly:context-property context 'harpPedalAutoUpdate #t)
         (for-each (lambda (note)
                     (let* ((note-pitch (ly:event-property note 'pitch))
                            (pedal-change-alt (assoc-get (ly:pitch-notename note-pitch) change-alist)))
                       (when (and pedal-change-alt
                                  (not (eqv? pedal-change-alt (ly:pitch-alteration note-pitch))))
                         (ly:input-warning (ly:event-property note 'origin (*location*))
                           "Harp_pedal_engraver: note event contradicts simultaneous pedal change"))))
           notes))
       )

      ((stop-translation-timestep engraver)
       (set! ev #f)
       (set! notes '())
       (update-alist! change-alist '((1 . #f) (0 . #f) (6 . #f) (2 . #f) (3 . #f) (4 . #f) (5 . #f))))

      )))

% Music functions

setHarpPedals =
#(define-music-function (change) (ly:music?)
   (make-music
    'HarpPedalEvent
    'pitch-alist (pitch-list->alist (music-pitches change))))

% % Examples % % 

RH = \relative c'' {
  \clef treble
  <>^\setHarpPedals { dis cis b e fis gis ais }
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
  \textLengthOn % for purposes of the example
} <<
  \new Staff \RH
  \new Staff \LH
>>

% Basic usage test examples

RH = \relative c' {
  \key d \major
  % Start by completely setting the pedals
  <>\setHarpPedals { d cis b e fis g a }
  <d fis a>1
  <d a'>1
  % Explicit and implicit pedal changes will be combined into a single marking
  <d f bes>1\setHarpPedals { bes }
  % Simultaneous explicit pedal changes will also be combined
  <des f bes>1\setHarpPedals { des }
  R1 
  % The order of notes entered in \setHarpPedals does not matter:
  <e g c>\setHarpPedals { c d e fis g a b }
}

LH = \relative c {
  \clef bass
  \key d \major
  d1
  % Pitches outside the pedal setting will trigger an automatic pedal change (from either Staff)
  % Explicit pedal changes can be entered in either Staff
  f2 d2\setHarpPedals { c }
  % Explicit and implicit pedal changes will be combined into a single marking
  <c aes'>1
  % Simultaneous explicit pedal changes will also be combined
  <bes ges'>1\setHarpPedals { ges }
  R1 
  c1
}

\markup\bold "basic usage example:"

\new PianoStaff \with {
  \consists #Harp_pedal_engraver
  % see what happens when harpPedalStyle is set to 'hybrid, 'hybrid-circles, and 'graphical
  % 'graphical is probably only useful for pedagogical materials
  harpPedalStyle = #'text
  \textLengthOn % for purposes of the example
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
  <>\setHarpPedals { bes c d es f g a }
  R1
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
  % Implicit or explicit accidentals other than flat/natural/sharp will trigger a harpPedalSetting type warning
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
  \textLengthOn % for purposes of the example
} <<
  \new Staff \RH
  \new Staff \LH
>>

% Advanced use cases
% position above, custom text format, custom graphical format, tweak grob, tweak implicit grob?, persistent tweaks, disabling autoupdate during cues

testcue = \relative c' {
  s1*3
  r4 c d e
  r4 fis gis ais
}

\addQuote "test" \testcue

example = \relative c' {
  % ^ can be used to position a marking above the context
  <>^\setHarpPedals { d cis b e fis g a }
  <d fis a>1
  % style and other context properties can be updated within the music
  % note that you must include the context (Staff, PianoStaff, etc) 
  % where Harp_pedal_engraver was consisted
  \once\set Staff.harpPedalStyle = #'hybrid
  % explicit pedal changes can be tweaked
  <des f bes>-\tweak color #red \setHarpPedals { des c bes es f ges aes }
  \unset Staff.harpPedalTextMarkup
  % Pedal markings print outside normal TextScripts
  % They are just TextScripts so \override TextScript affects both them and other markings
  % Notice that if Staff is removed from the override, it does not affect the pedal markings
  \override Staff.TextScript.font-size = #-3
  b2\p_"foo" b'2\setHarpPedals { aes }
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

% A custom text markup function can be used to reorder the note names depending on the harpist's preference
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

% Similarly, enclosures can be added around the graphical pedal markings
custom-pedal-graphic =
#(define-scheme-function (pedals)
   (markup?)
   "Print a graphical pedal marking"
   #{
     \markup\box $pedals
   #})

\markup\bold "advanced usage:"

% Harp_pedal_engraver functions when consisted to a Staff, though this is probably not the typical usage
\new Staff \with {
  \consists #Harp_pedal_engraver
  harpPedalStyle = #'text
  harpPedalTextMarkup = #custom-pedal-text
  harpPedalGraphicMarkup = #custom-pedal-graphic
  \textLengthOn % for purposes of the example
} \example

% Advanced use cases example 2

% Another way to persistently modify pedal markings is to create a new function with your tweaks
tweakedSetHarpPedals = 
#(define-music-function (m) (ly:music?)
   #{
     -\tweak color #red
     \setHarpPedals $m
   #})

% Probably the best way to deal with cues is to turn off autoupdate in your cue functions
fixedCueDuring = 
#(define-music-function (cue dir rests) (string? number? ly:music?)
   #{
     \cueDuring #cue #dir {
       \set GrandStaff.harpPedalAutoUpdate = ##f
       $rests
       \set GrandStaff.harpPedalAutoUpdate = ##t
     }
   #}
   )

RH = \relative c' {
  c1\tweakedSetHarpPedals { d c b e fis g a }
  R1*2
  \fixedCueDuring #"test" #DOWN {
    R1*2
  }
}

LH = \relative c {
  \clef bass
  R1*5
}

\new PianoStaff \with {
  \consists #Harp_pedal_engraver
  \textLengthOn % for purposes of the example
} <<
  \new Staff \RH
  \new Staff \LH
>>