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

#(define (define-grob! grob-name grob-entry)
   (set! all-grob-descriptions
         (cons ((@@ (lily) completize-grob-entry)
                (cons grob-name grob-entry))
           all-grob-descriptions)))

% Event definitions

#(define-event-class 'harp-pedal-event 'music-event)

#(define-event!
  'HarpPedalEvent
  '((description . "Harp pedal change")
    (types . (post-event harp-pedal-event event))))

% Predicates

#(define (harp-pedal-alist? s)
   "Type predicate: pitch-alist with correct L-R harp pedal order 
and valid accidentals"
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

#(define-public (ly:side-position-interface::move-to-extremal-staff-or-middle grob)
   "Wraps the extremal staff callback. If direction is set to CENTER,
place grobs below the top staff instead of above it."
   (let ((dir (ly:grob-property grob 'direction))
         (out (ly:side-position-interface::move-to-extremal-staff grob)))
     (when (eqv? dir CENTER)
       (ly:grob-set-property! grob 'direction DOWN))
     out))

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

% Grob definitions

% This probably doesn't deserve its own interface.
#(ly:add-interface
  'harp-pedal-interface
  "A harp pedal marking."
  '(format-pedal-text))

#(set-object-property! 'format-pedal-text 'backend-type? procedure?)

#(define-grob! 'HarpPedalChart
   `(
      (after-line-breaking .
        ,ly:side-position-interface::move-to-extremal-staff-or-middle)
      ; we want this to return a procedure for use by the engraver,
      ; so we accept the grob as an argument then ignore it
      (format-pedal-text . ,(const text-pedal-change))
      ; keep lines close together with a tiny whitespace between accidentals
      (baseline-skip . 2.75)
      (direction . ,UP)
      ;; See \markLengthOn
      (extra-spacing-width . (+inf.0 . -inf.0))
      (outside-staff-horizontal-padding . 0.2)
      (outside-staff-priority . 500)
      (padding . 2)
      ;(parent-alignment-X . ,RIGHT)
      (self-alignment-X . ,RIGHT)
      (X-align-on-main-noteheads . #f)
      (cross-staff . #f)
      (side-axis . ,Y)
      (stencil . ,ly:text-interface::print)
      (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
      (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
      (Y-offset . ,side-position-interface::y-aligned-side)
      (Y-extent . ,grob::always-Y-extent-from-stencil)
      (meta . ((class . Item)
               (interfaces . (accidental-switch-interface
                              harp-pedal-interface
                              instrument-specific-markup-interface
                              font-interface
                              outside-staff-interface
                              self-alignment-interface
                              side-position-interface
                              text-interface))
               (description . "Harp pedal diagram.")))))

#(define-grob! 'HarpPedalChange
   `(
      (after-line-breaking .
        ,ly:side-position-interface::move-to-extremal-staff-or-middle)
      ; we want this to return a procedure for use by the engraver,
      ; so we accept the grob as an argument then ignore it
      (format-pedal-text . ,(const text-pedal-change))
      ;(avoid-slur . around)
      ;(script-priority . 200)
      ; (slur-padding . 0.5)
      ; (staff-padding . 0.5)
      (direction . ,DOWN)
      (extra-spacing-width . (+inf.0 . -inf.0))
      ; keep lines close together with a tiny whitespace between accidentals
      (baseline-skip . 2.75)
      (outside-staff-horizontal-padding . 0.2)
      (outside-staff-priority . 500)
      ;; This value reduces the relative vertical displacement of grobs
      ;; Useful one for users to tweak
      (padding . 3.5)
      ; (parent-alignment-X . ,LEFT)
      (self-alignment-X . ,LEFT)
      (X-align-on-main-noteheads . #f)
      (side-axis . ,Y)
      (cross-staff . #f)
      (stencil . ,ly:text-interface::print)
      (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
      (Y-extent . ,grob::always-Y-extent-from-stencil)
      (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
      (Y-offset . ,side-position-interface::y-aligned-side)
      (meta . ((class . Item)
               (interfaces . (accidental-switch-interface
                              font-interface
                              harp-pedal-interface
                              instrument-specific-markup-interface
                              outside-staff-interface
                              self-alignment-interface
                              side-position-interface
                              text-interface
                              ;text-script-interface
                              ))
               (description . "Incremental harp pedal change.")))))

\layout {
  \context {
    \Global
    \grobdescriptions #all-grob-descriptions
  }
}

% Context properties

#(translator-property-description 'harpPedalSetting harp-pedal-alist?
   "The current harp pedal setting. Must be a pitch-alist in L-R order 
of the harp pedals, containing only 1/2, 0, and -1/2 alterations.")
#(translator-property-description 'harpPedalStyle symbol?
   "Style of harp pedal charts. Valid options: 'graphical (default), 'text, 
'graphical-circles.")
#(translator-property-description 'harpPedalAutoUpdate boolean?
   "If #t (default), check notes against the current pedal setting and print 
changes automatically.")
#(translator-property-description 'harpPedalFixedBelow ly:pitch?
   "Ignore pitches on this string or below when auto-updating harp pedals. 
Defaults to D#1.")

% Engraver

#(define (Harp_pedal_engraver context)
   "Listens for harp-pedal-events and prints formatted harp pedal markings. 
Keeps track of the pedal setting and automatically updates the pedal setting 
when new accidentals occur, printing the changes. Creates HarpPedalChart and 
HarpPedalChange grobs."
   (let ((change-alist
          '((1 . #f) (0 . #f) (6 . #f) (2 . #f) (3 . #f) (4 . #f) (5 . #f)))
         (cached-event #f)
         (notes '()))

     (define (fixed-string? p)
       "Takes @var{p} a ly:pitch? and returns true if p is below the cutoff 
pitch for harp strings with fixed pitch. Comparisons chosen so the flatted 
string above will return #f."
       (let ((fixed-below (ly:context-property context
                            'harpPedalFixedBelow (ly:make-pitch -3 1 1/2))))
         (or (ly:pitch<? p fixed-below)
             (equal? p fixed-below))))

     (define (pedals-graphic change->character)
       "Splits change-alist into left and right pedals, maps onto each 
the procedure change->character, then concats the resulting strings and returns 
the resulting harp-pedal markup. change->character should take 
(notename . alteration) as an argument and return a string."
       (receive (left right)
         (partition (lambda (el)
                      (or (= 1 (car el))
                          (= 0 (car el))
                          (= 6 (car el))))
           change-alist)
         (markup #:harp-pedal (string-append
                               (string-concatenate 
                                (map change->character left))
                               "|"
                               (string-concatenate 
                                (map change->character right))))))

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
          ; If the cached event was explicit and this event was automatic,
          ; don't replace the cached one
          ; This is in order not to discard user tweaks
          (unless (and cached-event
                       (eqv? 'HarpPedalEvent
                             (ly:music-property
                              (ly:event-property cached-event 'music-cause)
                              'name))
                       (eqv? 'NoteEvent
                             (ly:music-property
                              (ly:event-property event 'music-cause)
                              'name)))
            (set! cached-event event))))

       ((note-event engraver event)
        (when (ly:context-property context 'harpPedalAutoUpdate #t)
          (let* ((p (ly:event-property event 'pitch))
                 (name (ly:pitch-notename p))
                 (alt (ly:pitch-alteration p))
                 (setting-alist
                  (ly:context-property context 'harpPedalSetting #f)))
            (unless (fixed-string? p)
              (set! notes (cons event notes))
              (when (and setting-alist
                         ; Ignore note events consistent with the pedal setting
                         (not (eqv? alt
                                    (assoc-get name setting-alist))))
                (let ((pedal-event (ly:make-stream-event
                                    (ly:make-event-class 'harp-pedal-event)
                                    `((pitch-alist . ,`((,name . ,alt)))
                                      (origin .
                                        ,(ly:event-property event 'origin))
                                      (music-cause .
                                        ,(ly:event-property
                                          event 'music-cause))))))
                  (ly:broadcast
                   (ly:context-event-source context) pedal-event))))))))

      ((process-music engraver)
       ; If we got a pedal event this time step, print it.
       (when cached-event
         (let ((style (ly:context-property context 'harpPedalStyle 'graphical))
               (setting-alist (ly:context-property context 'harpPedalSetting))
               (reset? (every cdr change-alist)))
           (when style
             (let* ((grob (ly:engraver-make-grob engraver
                            (if reset? 'HarpPedalChart 'HarpPedalChange)
                            cached-event))
                    (dir (ly:event-property cached-event 'direction #f))
                    (change-markup (cond
                                    ((or (eqv? style 'text)
                                         (not reset?))
                                     (apply
                                      (ly:grob-property grob 'format-pedal-text)
                                      (map pitch-class->markup change-alist)))

                                    ((and (pair? setting-alist)
                                          (eqv? style 'graphical-circles))
                                     (pedals-graphic
                                      (lambda (change)
                                        (change->circled-pedal-character
                                         setting-alist change))))

                                    (else
                                     (pedals-graphic
                                      (compose alteration->pedal-character cdr))))))
               (ly:grob-set-property! grob 'text change-markup)
               (when dir
                 (ly:grob-set-property! grob 'direction dir))))
           ; Update the pedal setting based on change-alist. Do this after printing
           ; the changes, in case we need to print circles based on the last setting
           ; Default value of '() allows pedal setting to be initialized
           ; Check first for incomplete initialization to avoid duplicate warnings
           (if (and (not reset?)
                    (null? setting-alist))
               (ly:input-warning
                (ly:event-property cached-event 'origin (*location*))
                "Harp_pedal_engraver: received incomplete initial pedal setting")
               (ly:context-set-property! context 'harpPedalSetting
                 (update-alist! setting-alist (filter cdr change-alist))))
           ))

       ; Don't warn about contradictory notes if the user is taking responsibility for pedal markings
       (when (ly:context-property context 'harpPedalAutoUpdate #t)
         (for-each (lambda (note)
                     (let* ((note-pitch (ly:event-property note 'pitch))
                            (pedal-change-alt
                             (assoc-get
                              (ly:pitch-notename note-pitch)
                              change-alist)))
                       (when (and pedal-change-alt
                                  (not (eqv? pedal-change-alt
                                             (ly:pitch-alteration note-pitch))))
                         (ly:input-warning
                          (ly:event-property note 'origin (*location*))
                          "Harp_pedal_engraver: note event contradicts simultaneous pedal change"))))
           notes))
       )

      ((stop-translation-timestep engraver)
       (set! cached-event #f)
       (set! notes '())
       (update-alist! change-alist
         '((1 . #f) (0 . #f) (6 . #f) (2 . #f) (3 . #f) (4 . #f) (5 . #f))))

      )))

% Music functions

setHarpPedals =
#(define-music-function (change) (ly:music?)
   (make-music
    'HarpPedalEvent
    'pitch-alist (pitch-list->alist (music-pitches change))))

% % Examples % %
%
% RH = \relative c'' {
%   \clef treble
%   <>\setHarpPedals { dis cis b e fis gis ais }
%   <dis fis,>2(\p <d f,> |
%   <cis e,>2 <b dis,>4\setHarpPedals { fis } <ais cis,> |
%   <a fis bis,>2) r |
%   <gis eis b>2 r |
%   <a fis bis,>2\setHarpPedals { e } r |
%   <ais e cis>4( <gis b,>\< <fis ais,> <e gis,> |
%   <dis g,!>4)\! r r2 |
% }
%
% LH = \relative c {
%   \clef bass
%   <b b,>4 r r2 |
%   R1*6 |
% }
%
% \markup\bold "from R. Strauss, Don Juan:"
%
% \new PianoStaff \with {
%   \consists #Harp_pedal_engraver
% } <<
%   \new Staff \RH
%   \new Staff \LH
% >>
%
% % Basic usage test examples
%
% RH = \relative c' {
%   \key d \major
%   % Start by completely setting the pedals
%   <>_\setHarpPedals { d ces b e fis g a }
%   <d fis a>1
%   <d a'>1
%   % Explicit and implicit pedal changes will be combined into a single marking
%   <d f bes>1_\setHarpPedals { bes }
%   % Simultaneous explicit pedal changes will also be combined
%   <des f bes>1\setHarpPedals { des }
%   R1*2
%   % The order of notes entered in \setHarpPedals does not matter:
%   <e g c>1\setHarpPedals { c d e fis g a b }
% }
%
% LH = \relative c {
%   \clef bass
%   \key d \major
%   % When setting direction to CENTER, grobs will be placed underneath the top staff instead of above
%   \override PianoStaff.HarpPedalChange.direction = #CENTER
%   d1
%   % Pitches outside the pedal setting will trigger an automatic pedal change (from either Staff)
%   % Explicit pedal changes can be entered in either Staff
%   f2 d2\setHarpPedals { c }
%   % Explicit and implicit pedal changes will be combined into a single marking
%   <c aes'>1
%   % Simultaneous explicit pedal changes will also be combined
%   <bes ges'>1\setHarpPedals { ges }
%   % notes on the bottom fixed strings will be ignored by automatic pedal changes
%   es,,1
%   dis1
%   ces1
% }
%
% \markup\bold "basic usage example:"
%
% \new PianoStaff \with {
%   \consists #Harp_pedal_engraver
%   harpPedalStyle = #'graphical-circles
% } <<
%   \new Staff \RH
%   \new Staff \LH
% >>
%
% % Warnings/edge cases
%
% RH = \relative c' {
%   \key bes \major
%   % Incomplete initialization
%   <>\setHarpPedals { d cis }
%   d1
%   R1-\setHarpPedals { bes c d es f g a }
%   % Simultaneous contradictory explicit pedal changes
%   a'1\setHarpPedals { aes }
%   % Simultaneous contradictory explicit pedal changes
%   g1\setHarpPedals { e }
%   % An implicit pedal change is contradicted by a simultaneous explicit pedal change
%   f1\setHarpPedals { cis }
%   % The previous situation but order reversed triggers a different warning message
%   fis1
%   % Multiple pedal changes on the same side does not trigger a warning
%   dis2 <e gis>2
%   % No warning from duplicate or contradictory pitches in the same \setHarpPedals
%   % After the first, they are just ignored
%   gis2\setHarpPedals { b b } gis2\setHarpPedals { fis f }
%   % Implicit or explicit accidentals other than flat/natural/sharp 
%   % will trigger a harpPedalSetting type warning
%   gisis4 ais\setHarpPedals { beses } cisih cisih
% }
%
% LH = \relative c {
%   \clef bass
%   \key bes \major
%   d1
%   R1*2
%   % Simultaneous contradictory explicit pedal changes
%   bes1\setHarpPedals { eis }
%   % An implicit pedal change is contradicted by a simultaneous explicit pedal change
%   ces1
%   % The previous situation but order reversed triggers a different warning message
%   bes1\setHarpPedals { fes }
%   % Multiple pedal changes on the same side does not trigger a warning
%   bis2 <cis ais>2
%   R1*2
% }
%
% \markup\bold "warnings and edge cases:"
%
% \new PianoStaff \with {
%   \consists #Harp_pedal_engraver
%   harpPedalStyle = #'text
%   \override HarpPedalChange.extra-spacing-width = #'(0 . 0)
%   \override HarpPedalChart.self-alignment-X = #CENTER
% } <<
%   \new Staff \RH
%   \new Staff \LH
% >>
%
% % Advanced use cases
%
% testcue = \relative c' {
%   s1*5
%   r4 c d e
%   r4 fis gis ais
% }
%
% \addQuote "test" \testcue
%
% example = \relative c' {
%   <>\setHarpPedals { d cis b e fis g a }
%   <d fis a>1
%   % note that you must include the context (Staff, PianoStaff, etc)
%   % where Harp_pedal_engraver was consisted
%   \once\set Staff.harpPedalStyle = #'graphical-circles
%   <des f bes>\setHarpPedals { des c bes es f ges aes }
%   \revert Staff.HarpPedalChart.format-pedal-text
%   \once\override Staff.HarpPedalChange.color = #red
%   b2\p_"foo" r2 |
%   % unsetting harpPedalSetting can be useful before a simple 
%   % passage where pedal markings are not needed
%   \unset Staff.harpPedalSetting
%   d1 |
%   \once\set Staff.harpPedalStyle = ##f
%   b'1\setHarpPedals { des c b es f g aes }
%   % notes in cues will trigger automatic pedal changes
%   \cueDuring #"test" #DOWN {
%     R1
%   }
%   % to avoid this, turn off autoupdate during cues
%   \cueDuring #"test" #DOWN {
%     \set Staff.harpPedalAutoUpdate = ##f
%     R1
%     \unset Staff.harpPedalAutoUpdate
%   }
% }
%
% % A custom text markup function can be used to reorder the note names 
% % depending on the harpist's preference
% % And to add formatting, enclosures, etc.
% custom-pedal-text =
% #(define-scheme-function (d c b e f g a)
%    (markup? markup? markup? markup? markup? markup? markup?)
%    "Print a text pedal marking"
%    #{
%      \markup\box\left-column {
%        \line { $b $c $d }
%        \line { $e $f $g $a }
%      }
%    #})
%
% \markup\bold "advanced usage:"
%
% % Harp_pedal_engraver functions when consisted to a Staff, 
% % though this is probably not the typical usage
% \new Staff \with {
%   \consists #Harp_pedal_engraver
%   harpPedalStyle = #'text
%   \override HarpPedalChart.extra-spacing-width = #'(0 . 0)
%   \override HarpPedalChart.self-alignment-X = #LEFT
%   % Use this syntax to set the custom formatting function
%   % The same method can be used to change formatting of HarpPedalChange
%   \override HarpPedalChart.format-pedal-text = #(const custom-pedal-text)
% } \example