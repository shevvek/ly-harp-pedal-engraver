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

% Predicates

#(define (pitch-list? x)
   (and (pair? x)
        (every ly:pitch? x)))

#(define (complete-scale? p)
   (and (pitch-list? p)
        (lset= equal? 
          (map ly:pitch-notename p)
          '(0 1 2 3 4 5 6))))

#(define (is-note? n)
   "Returns a predicate that is #t if notename equals n"
   (lambda (p) (equal? n (ly:pitch-notename p))))

% Context properties

#(translator-property-description 'harpPedalSetting list? "The current pedal setting.")
#(translator-property-description 'harpPedalChartStyle symbol? "Valid options: 'graphical, 'text")
#(translator-property-description 'harpPedalChangeOrder integer? "If >= 0 (default), right (EFGA) over left (DCB); if <0, left over right.")
#(translator-property-description 'harpPedalAutoUpdate boolean? "If ##t (default), check notes against the current pedal setting and print changes automatically.")

% Event definitions

#(define-event-class 'harp-pedal-event 'music-event)

#(define-event!
   'HarpPedalEvent
   '((description . "Harp pedal change")
     (types . (harp-pedal-event event))))

% Grob definitions

% Helper functions

% from https://www.mail-archive.com/lilypond-user@gnu.org/msg131484.html
#(define (staff-like? ctx)
    (eq? ctx (ly:context-find ctx 'Staff)))

% from scm/chord-name.scm
#(define (accidental->markup alteration)
  "Return accidental markup for ALTERATION."
  (if (= alteration 0)
      (make-line-markup (list empty-markup))
      (alteration->text-accidental-markup alteration)))

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
             (alt (cdr p))
             (hide-nats #f))
         (markup #:concat
           ((string-capitalize (note-name-int->string name))
            (if (and hide-nats (= 0 alt))
                empty-markup
                (alteration->text-accidental-markup alt)))))
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

#(define (graphical-harp-string pedal-setting pedal-changes)
   (define (change->circled-pedal-character change)
     (let ((new-alt (cdr change))
           (old-alt (assoc-get (car change) pedal-setting)))
       (if (and new-alt
                (not (= new-alt old-alt)))
           (string-append "o" (alteration->pedal-character new-alt))
           (alteration->pedal-character old-alt))))
   (receive (left right)
     (partition (lambda (el)
                  (or (= 1 (car el))
                      (= 0 (car el))
                      (= 6 (car el))))
       pedal-changes)
     (if (every cdr pedal-changes)
         (string-concatenate
          (apply append
            (map alteration->pedal-character (map cdr left))
            '("|")
            (map alteration->pedal-character (map cdr right))
            '()))
         (string-concatenate
          (apply append
            (map change->circled-pedal-character left)
            '("|")
            (map change->circled-pedal-character right)
            '()))
         )))

#(ly:message "~a" (graphical-harp-string 
                   '((1 . 0) (0 . 0) (6 . 1/2) (2 . -1/2) (3 . 0) (4 . 0) (5 . 0))
                    '((1 . #f) (0 . #f) (6 . #f) (2 . 0) (3 . 1/2) (4 . #f) (5 . #f))
                    ))

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

% Engraver

#(define (Harp_pedal_engraver context)
   (let ((change-list '((1 . #f) (0 . #f) (6 . #f) (2 . #f) (3 . #f) (4 . #f) (5 . #f)))
         (ev #f)
         (notes '()))
     
     (make-engraver
      (listeners
       ((harp-pedal-event engraver event)
        (let ((new-changes (ly:event-property event 'pedal-changes)))
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
                                `((pedal-changes . ,`((,name . ,alt)))
                                  (origin . ,(ly:event-property event 'origin))
                                  (event-cause . ,event)))))
              (ly:broadcast (ly:context-event-source context) pedal-event)))))
        
        )

      ((process-music engraver)
       (let ((pedal-setting (ly:context-property context 'harpPedalSetting #f)))
         ; If harpPedalSetting hasn't been initialized, do it based on the key signature
         (unless pedal-setting
           ; If this context is Staff-like, we grab the keysign here, if not, we get it from the first Staff-like child context
           ; If the engraver is consisted to a context with no direct Staff-like children, it will still work, but will not grab key alterations
           (let ((keysig (if (staff-like? context)
                             (ly:context-property context 'keyAlterations)
                             (let ((child-staves (filter staff-like? (ly:context-children context))))
                               (if (pair? child-staves)
                                   (ly:context-property (car child-staves) 'keyAlterations)
                                   '())))))
           (set! pedal-setting
                 (update-alist! '((1 . 0) (0 . 0) (6 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0))
                   keysig)))
         ; Update the pedal setting based on change-list
         (update-alist! pedal-setting (filter cdr change-list))
         (ly:context-set-property! context 'harpPedalSetting pedal-setting)))
       
       (for-each (lambda (note)
                   (let* ((note-pitch (ly:event-property note 'pitch))
                          (pedal-change-alt (assoc-get (ly:pitch-notename note-pitch) change-list)))
                     (when (and pedal-change-alt
                                (not (= pedal-change-alt (ly:pitch-alteration note-pitch))))
                       (ly:input-warning (ly:event-property note 'origin (*location*))
                         "Harp_pedal_engraver: note event contradicts simultaneous pedal change"))))
         notes)
           
       ; If we got a pedal event this time step, print it. The event-cause will be the most recent event received.
       (if ev
           (let* ((grob (ly:engraver-make-grob engraver 'TextScript ev))
                 (change-markups (map pitch-class->markup change-list))
                 (change-markup (apply text-pedal-change change-markups))) ; doing it this way assumes that all the alist operations preserve order
             (ly:grob-set-property! grob 'text change-markup)
             (ly:grob-set-property! grob 'after-line-breaking ly:side-position-interface::move-to-extremal-staff)
             (ly:grob-set-property! grob 'outside-staff-priority 500)
             (ly:grob-set-property! grob 'direction DOWN)
             ))
       )
      
      ((stop-translation-timestep engraver)
        (set! ev #f)
        (set! notes '())
        (update-alist! change-list '((1 . #f) (0 . #f) (6 . #f) (2 . #f) (3 . #f) (4 . #f) (5 . #f))))
      
      )))

% Context mod

\layout {
  \context {
    \PianoStaff
    \consists #Harp_pedal_engraver
  }
}

% Music functions
% provide both music and post-event UI

setHarpPedals = 
#(define-scheme-function (change) (ly:music?)
   (let* ((change-list (pitch-list->alist (music-pitches change)))
         (pedal-event (ly:make-stream-event 
                 (ly:make-event-class 'harp-pedal-event)
                 `((pedal-changes . ,change-list))))
         (broadcast-pedals (lambda (context)
                             (ly:broadcast (ly:context-event-source context) pedal-event)))
           )
     (make-apply-context broadcast-pedals)))
     

% Test examples

RH = {
  \key d \major
  cis'1
  c'1
  \setHarpPedals { dis bes }
  fis'
}

LH = {
  \key d \major
  s1*2
  bis1\p_"foo"
}

\new PianoStaff <<
  \new Staff \RH
  \new Staff \LH
>>