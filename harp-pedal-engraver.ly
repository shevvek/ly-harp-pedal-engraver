\version "2.24.0"

#(use-modules ((ice-9 list)
              #:select (rassoc)))

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
             (show-nats #t))
         (markup #:concat
           ((string-capitalize (note-name-int->string name))
            (if (and show-nats (= 0 alt))
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
         (ev #f))
     
     (make-engraver
;       ((initialize engraver)
;         (ly:message "~a" (ly:context-property context 'keyAlterations)))
      
      (listeners
       ((harp-pedal-event engraver event)
        (let ((new-changes (ly:event-property event 'pedal-changes)))
          (when (any
               (lambda (el)
                 (let ((old-alt (assoc-get (car el) change-list)))
                   (and old-alt
                        (not (equal? old-alt (cdr el))))))
               new-changes)
              (ly:input-warning (*location*) "Harp_pedal_engraver: simultaneous contradictory pedal change received."))

          (update-alist! change-list new-changes)
          (set! ev event)))
       
       )

      ((process-music engraver)
       (ly:message "~a" (ly:context-property context 'harpPedalSetting #f))
       (let ((pedal-setting (ly:context-property context 'harpPedalSetting #f)))
         ; If harpPedalSetting hasn't been initialized, do it based on the key signature
         (unless pedal-setting
           (set! pedal-setting
                 (update-alist! '((1 . 0) (0 . 0) (6 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0))
                   (ly:context-property context 'keyAlterations))))
         ; Update the pedal setting based on change-list
         (update-alist! pedal-setting (filter cdr change-list))
         (ly:context-set-property! context 'harpPedalSetting pedal-setting))
           
       ; If we got a pedal event this time step, print it
       (if ev
           (let* ((grob (ly:engraver-make-grob engraver 'TextScript ev))
                 (change-markups (map pitch-class->markup change-list))
                 (change-markup (apply text-pedal-change change-markups))) ; doing it this way assumes that all the alist operations preserve order
             (ly:grob-set-property! grob 'text change-markup)
             (ly:grob-set-property! grob 'direction DOWN)
             ))
       )
      
      ((stop-translation-timestep engraver)
        (set! ev #f)
        (set! change-list '((1 . #f) (0 . #f) (6 . #f) (2 . #f) (3 . #f) (4 . #f) (5 . #f))))
      
      )))

% Context mod

\layout {
  \context {
    \Staff
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

{
  \key f \major
  c'1
  \setHarpPedals { f es bis des }
  \setHarpPedals { fis }
  c'1
}