\version "2.25.10"

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

% Grob definitions

% Predicates

% from https://www.mail-archive.com/lilypond-user@gnu.org/msg131484.html
#(define (staff-like? ctx)
   (eq? ctx (ly:context-find ctx 'Staff)))

#(define (pedal-setting? s)
   "Type predicate that checks pedal setting alist for correct pedal order and valid accidentals"
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
   (map (lambda (p) `(,(ly:pitch-notename p) . ,(ly:pitch-alteration p)))
     pitches))

#(define (pitch-class->markup p)
   "Expects (notename . alteration)"
   (if (and (pair? p) (number? (cdr p)))
       (let ((name (car p))
             (alt (cdr p)))
         (markup #:concat
           ((string-capitalize (note-name-int->string name))
            (alteration->text-accidental-markup alt))))
       empty-markup))

#(define (update-alist! alist1 alist2)
   (set! alist1
         (fold (lambda (el2 lst)
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

#(define (change->circled-pedal-character pedal-setting change)
   (let ((new-alt (cdr change))
         (old-alt (assoc-get (car change) pedal-setting)))
     (if (and new-alt
              (not (= new-alt old-alt)))
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

#(translator-property-description 'harpPedalSetting pedal-setting? "The current harp pedal setting.")
#(translator-property-description 'harpPedalStyle symbol?
   "Valid options: 'graphical, 'text, 'hybrid (default). With 'hybrid, incremental changes are printed as text, 
while resets of all 7 pedals are printed graphically.")
#(translator-property-description 'harpPedalAutoUpdate boolean?
   "If #t (default), check notes against the current pedal setting and print changes automatically.")
#(translator-property-description 'harpPedalTextMarkup procedure?
   "Scheme function accepting 7 markup arguments in L-R pedal order, returning a markup that formats text pedal markings.")
#(translator-property-description 'harpPedalGraphicalMarkup procedure?
   "Scheme function accepting 1 markup and returning a markup. By default, simply returns the original markup. 
Use this for example to add an enclosure around graphical pedal markings.")

% Engraver

#(define (Harp_pedal_engraver context)
   (let ((change-list '((1 . #f) (0 . #f) (6 . #f) (2 . #f) (3 . #f) (4 . #f) (5 . #f)))
         (ev #f)
         (notes '()))

     (define (pedals-graphic change->character)
       "Splits change-list into left and right pedals, maps onto each the procedure 
change->character, then concats the resulting strings and returns the resulting harp-pedal markup. 
change->character should take (notename . alteration) as an argument and return a string."
       (receive (left right)
         (partition (lambda (el)
                      (or (= 1 (car el))
                          (= 0 (car el))
                          (= 6 (car el))))
           change-list)
         (markup #:harp-pedal (string-append
                               (string-concatenate (map change->character left))
                               "|"
                               (string-concatenate (map change->character right)))
           )))

     (make-engraver
      (listeners
       ((harp-pedal-event engraver event)
        (let ((new-changes (ly:event-property event 'pitch-alist)))
          (when (any
                 (lambda (el)
                   (let ((old-alt (assoc-get (car el) change-list)))
                     (and old-alt
                          (not (equal? old-alt (cdr el))))))
                 new-changes)
            (ly:input-warning (ly:event-property event 'origin (*location*))
              "Harp_pedal_engraver: simultaneous contradictory pedal change received."))

          (update-alist! change-list new-changes)
          (set! ev event)))

       ((note-event engraver event)
        (when (ly:context-property context 'harpPedalAutoUpdate #t)
          (let* ((p (ly:event-property event 'pitch))
                 (name (ly:pitch-notename p))
                 (alt (ly:pitch-alteration p))
                 (pedal-setting (ly:context-property context 'harpPedalSetting #f)))
            (set! notes (cons event notes))
            (when (and pedal-setting
                       (not (= alt
                               (assoc-get name pedal-setting))))
              (let ((pedal-event (ly:make-stream-event
                                  (ly:make-event-class 'harp-pedal-event)
                                  `((pitch-alist . ,`((,name . ,alt)))
                                    (origin . ,(ly:event-property event 'origin))
                                    (event-cause . ,event)))))
                (ly:broadcast (ly:context-event-source context) pedal-event))))))

       )

      ((process-music engraver)
       (let ((pedal-setting (ly:context-property context 'harpPedalSetting #f)))
         ; If harpPedalSetting hasn't been initialized, do it based on the key signature
         (unless pedal-setting
           ; If this context is Staff-like, we grab the keysign here,
           ; if not, we get it from the first Staff-like child context
           ; If the engraver is consisted to a context with no direct Staff-like children,
           ; it will still work, but will not grab key alterations
           (let ((keysig (if (staff-like? context)
                             (ly:context-property context 'keyAlterations)
                             (let ((child-staves (filter staff-like? (ly:context-children context))))
                               (if (pair? child-staves)
                                   (ly:context-property (car child-staves) 'keyAlterations)
                                   '())))))
             (set! pedal-setting
                   (update-alist! '((1 . 0) (0 . 0) (6 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0))
                     keysig))))

         ; If we got a pedal event this time step, print it. The event-cause will be the most recent event received.
         (when ev
           (let* ((grob (ly:engraver-make-grob engraver 'TextScript ev))
                  (style (ly:context-property context 'harpPedalStyle 'hybrid))
                  (reset? (every cdr change-list))
                  (text-output (ly:context-property context 'harpPedalTextMarkup text-pedal-change))
                  (graphical-output (ly:context-property context 'harpPedalGraphicalMarkup identity))
                  (change-markup (cond
                                  ((or (eqv? style 'text)
                                       (and (eqv? style 'hybrid)
                                            (not reset?)))
                                   ; doing it this way assumes that all the alist operations preserve order
                                   (apply text-output
                                     (map pitch-class->markup change-list)))

                                  ; If all markings are graphical, always circle changes
                                  ((eqv? style 'graphical)
                                   (graphical-output
                                    (pedals-graphic (lambda (change)
                                                      (change->circled-pedal-character pedal-setting change)))))

                                  ; In hybrid style, don't circle changes
                                  ((and (eqv? style 'hybrid)
                                        reset?)
                                   (graphical-output
                                    (pedals-graphic (compose alteration->pedal-character cdr)))))))
             (ly:grob-set-property! grob 'text change-markup)
             (ly:grob-set-property! grob 'after-line-breaking ly:side-position-interface::move-to-extremal-staff)
             (ly:grob-set-property! grob 'outside-staff-priority 500)
             (ly:grob-set-property! grob 'direction DOWN)
             ))

         ; Update the pedal setting based on change-list
         ; Do this after printing the changes, in case we need to print circles based on the last setting
         (update-alist! pedal-setting (filter cdr change-list))
         (ly:context-set-property! context 'harpPedalSetting pedal-setting))

       ; Don't warn about contradictory notes if the user is taking responsibility for pedal markings
       (when (ly:context-property context 'harpPedalAutoUpdate #t)
         (for-each (lambda (note)
                     (let* ((note-pitch (ly:event-property note 'pitch))
                            (pedal-change-alt (assoc-get (ly:pitch-notename note-pitch) change-list)))
                       (when (and pedal-change-alt
                                  (not (= pedal-change-alt (ly:pitch-alteration note-pitch))))
                         (ly:input-warning (ly:event-property note 'origin (*location*))
                           "Harp_pedal_engraver: note event contradicts simultaneous pedal change"))))
           notes))
       )

      ((stop-translation-timestep engraver)
       (set! ev #f)
       (set! notes '())
       (update-alist! change-list '((1 . #f) (0 . #f) (6 . #f) (2 . #f) (3 . #f) (4 . #f) (5 . #f))))

      )))

% Music functions

setHarpPedals =
#(define-music-function (change) (ly:music?)
   (let ((change-list (pitch-list->alist (music-pitches change))))
     (make-music
      'HarpPedalEvent
      'pitch-alist change-list)))

% For example purposes only
graphical-pedal-markup =
#(define-scheme-function (pedals)
   (markup?)
   "Print a graphical pedal marking"
   #{
     \markup\box $pedals
   #})

% Context mod

\layout {
  \context {
    \PianoStaff
    \consists #Harp_pedal_engraver
    % harpPedalStyle = #'graphical
    %  harpPedalGraphicalMarkup = #graphical-pedal-markup
  }
}

% Test examples

testcue = {
  s1
  c'1
}

\addQuote "test" \testcue

RH = {
  \key d \major
  cis'1
  c'1
  <>\setHarpPedals { dis bes }
  fis'
}

LH = {
  \key d \major
  s1
  \cueDuring #"test" #DOWN { s1 }
  bis1\p_"foo"
  c'1
}

\new PianoStaff <<
  \new Staff \RH
  \new Staff \LH
>>