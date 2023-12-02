\version "2.24.0"

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

#(translator-property-description 'harpPedalSetting complete-scale? "The current pedal setting.")
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

#(define (is-note? n)
   (lambda (p) (equal? n (ly:pitch-notename p))))

#(define (sparse-pedal-list changes)
   (let ((pedal-order '(1 0 6 2 3 4 5)))
     (map (lambda (n) (find (is-note? n) changes))
       pedal-order)))

% from scm/chord-name.scm
#(define (accidental->markup alteration)
  "Return accidental markup for ALTERATION."
  (if (= alteration 0)
      (make-line-markup (list empty-markup))
      (alteration->text-accidental-markup alteration)))

% adapted from scm/chord-name.scm
#(define (note-name->short-markup pitch)
   "Like built-in note-name->markup, but always capitalized, and concat the accidental instead of allowing space."
     (if (ly:pitch? pitch)
         (markup #:concat
             ((string-capitalize (note-name->string pitch))
              (alteration->text-accidental-markup (ly:pitch-alteration pitch))))
         empty-markup))

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

% #(define (Harp_pedal_engraver context)
%    (let (variables...)
%      (make-engraver
%       ((initialize engraver)
%        ...)
%       ((start-translation-timestep engraver)
%        ...)
%       (listeners
%        ((harp-pedal-event engraver event)
%         ...)
%        ((note-event engraver event)
%         ...)
%        ...)
%       ((pre-process-music engraver)
%        ...)
%       ((process-music engraver)
%        ...)
%       (acknowledgers
%        ((grob-interface-1 engraver grob source-engraver)
%         ...)
%        ((grob-interface-2 engraver grob source-engraver)
%         ...)
%        ...)
%       (end-acknowledgers
%        ((grob-interface-1 engraver grob source-engraver)
%         ...)
%         ((grob-interface-2 engraver grob source-engraver)
%          ...)
%        ...)
%       ((process-acknowledged engraver)
%        ...)
%       ((stop-translation-timestep engraver)
%        ...)
%       ((finalize engraver)
%        ...))))

#(define (Harp_pedal_engraver context)
   (let ((change-list '())
         (ev #f))
     
     (define (update-pedal-change new-change)
       (set! change-list new-change))
     
;      (define (print-pedal-changes)
;        
;        )
       
     
     (make-engraver
      ((initialize engraver)
        (ly:message "~a" (ly:context-property context 'keyAlterations)))
      
      (listeners
       ((harp-pedal-event engraver event)
        (update-pedal-change (ly:event-property event 'pedal-changes))
        (set! ev event))
       
       )

      ((process-music engraver)
       (ly:message "~a" (sparse-pedal-list change-list))
       (if ev
           (let* ((grob (ly:engraver-make-grob engraver 'TextScript ev))
                 (completed-list (sparse-pedal-list change-list))
                 (change-markups (map note-name->short-markup completed-list))
                 (change-markup (apply text-pedal-change change-markups)))
             (ly:grob-set-property! grob 'text change-markup)
             (ly:grob-set-property! grob 'direction DOWN)
             ))
       )
      
      ((stop-translation-timestep engraver)
        (set! ev #f)
        (set! change-list '()))
      
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
   (let* ((change-list (music-pitches change))
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
  c'1
}