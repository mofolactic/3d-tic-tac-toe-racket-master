#lang racket

;; The main function is the mark function. This is called every time
;; the board (canvas) is clicked. This association with click is made in the
;; defined-by-us canvas-with-events% class, of which gui-board is the function.

;; At the start of the application, the draw-board procedure is called,
;; as can be seen at the end of the file.

(require racket/gui)

(include "win.rkt")
(include "minimax.rkt")

(define difficulty 3)

(define main-window (new frame%
                         [label "Tic Tac Toe 3d"]
                         [min-width 700]
                         [min-height 650] ; non-fullHD monitors likely have a height of 720-768px
                         [stretchable-width #f]
                         [stretchable-height #f]))
;; Scaling and all will be needed to allow the window to be resizable

(define h-panel (new horizontal-panel% (parent main-window)))

(define canvas-with-events%
  (class canvas%
    (define/override (on-event event)
      (if (send event button-down? 'left)
          (mark (send event get-x) (send event get-y))
          #f))
    (super-new)))

; Make the drawing area
(define gui-board (new canvas-with-events%
                       [parent h-panel]))

; Make some pens and brushes
(define no-pen (make-object pen% "BLACK" 1 'transparent))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define blue-brush (make-object brush% "BLUE" 'solid))
(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-pen (make-object pen% "RED" 2 'solid))
(define yellow-pen (make-object pen% "YELLOW" 2 'solid))
(define blue-pen (make-object pen% "BLUE" 2 'solid))
(define green-pen (make-object pen% "GREEN" 2 'solid))
(define black-pen (make-object pen% "BLACK" 2 'solid))

(define z->pen
  (let* ((z-pen (make-vector 4 #f)))
    (vector-set! z-pen 0 red-pen)
    (vector-set! z-pen 1 yellow-pen)
    (vector-set! z-pen 2 blue-pen)
    (vector-set! z-pen 3 green-pen)
    (lambda (z) (vector-ref z-pen z))))

; Get the (board's) canvas's drawing context
(define dc (send gui-board get-dc))

(define (draw-2d-board dc z)
  (send dc set-pen (z->pen z))    
  (for ((y 4))
    (for ((x 4))
      (send dc draw-rectangle
            (+ 10 (* 50 x))
            (+ 10 (* 160 z) (* 30 y))
            50 30))))

(define (draw-board dc)
  (for ((z 4))
    (draw-2d-board dc z)))


;; The mark function, firstly, determines if the click is at
;; a valid location (inside a cell); if it is,
;; then it marks the location if it is unoccupied.

;; Bug: clicking on an occupied cell lets the computer play,
;; which it should not.

(define (mark x y)
  (define valid-position #t)
  (define z 0)
  (define adjusted-y
    (cond ((and (<= 10 y) (> 130 y)) y)
          ((and (<= 170 y) (> 290 y)) (set! z 1) (- y 40))
          ((and (<= 330 y) (> 450 y)) (set! z 2) (- y 80))
          ((and (<= 490 y) (>= 610 y)) (set! z 3) (- y 120))
          (else (set! valid-position #f))))
  ;(displayln corner-x)
  ;(displayln corner-y)
  (define corner-x 0)
  (define corner-y 0)
  
  (define (cross)
    (send dc draw-line (+ 20 corner-x) (+ 10 corner-y) (+ 30 corner-x) (+ 20 corner-y))
    (send dc draw-line (+ 30 corner-x) (+ 10 corner-y) (+ 20 corner-x) (+ 20 corner-y)))
  
  (define (circle)
    (send dc draw-ellipse (+ 20 corner-x) (+ 10 corner-y) 10 10))
  
  (if (>= x 210) (set! valid-position #f) (void))
  
  (define (make-turn)
    ;(displayln (list x adjusted-y))
    
    (define cell-x (floor (/ (- x 10) 50)))
    (define cell-y (floor (/ (- adjusted-y 10 (* z 120)) 30)))
    (define cell-z z)
    
    (set! last-played-pos (list cell-x cell-y cell-z))  
    (displayln last-played-pos)
    

    (if (= 0 (get-value board last-played-pos))
        (begin
          (set! corner-x (+ 10 (* cell-x 50)))
          (set! corner-y (+ 10 (* cell-y 30)  (* cell-z 160)))
          (send dc set-pen black-pen)
          (cond (myturn
                 (circle)
                 (set! board (set!-value board last-played-pos 1)))
                (else (cross) (set! board (set!-value board last-played-pos -1))))
          (if (win? board last-played-pos)
              (begin
                (send win-msg set-label (string-append "Player " (number->string (if myturn 2 1)) " has won!"))
                ;(sleep/yield 0.05)
                (send win-notif show #t))
              (void)))
        (void))
    ;(display-board board)
    (set! myturn (not myturn)))
  
  (if valid-position
      (let ()
        (make-turn)
        
        (define pc-pos (play-n-turns difficulty))
        ;(display "PC Pos:") (displayln pc-pos)
        (set! x (+ (* (car pc-pos) 50) 10))
        (set! z (caddr pc-pos))
        (set! adjusted-y (+ (* (cadr pc-pos) 30) 10 (* 120 z)))
              ;(floor (/ (- adjusted-y 10 (* z 120)) 30))))
        (make-turn))
      (void)))


;----------------------------------- End of GUI Board ------------------------------------

;;============================== Begin buttons and dialogs ===============================
(define v-panel (new vertical-panel% (parent h-panel)))

(define msg-area (new message%
                      [parent v-panel]
                      [label "Welcome!"]))

(define restart-btn
  (new button% [parent v-panel]
             [label "Restart"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         ;(send msg-area set-label "The game has been restarted."))]))
                         (send restart-confirm-window show #t))]))

;;------------------ Restart Confirmation Dialog Box --------------

(define restart-confirm-window
  (new dialog% (label "Restart?")))

(define restart-confirm-msg
  (new message% (parent restart-confirm-window)
       (label "Are you sure you want to restart?")))

(define restart-yes
  (new button%
       [parent restart-confirm-window]
       [label "Yes"]
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   (draw-board dc)
                   (set! board 0)
                   (send msg-area set-label "The game has been restarted.")
                   (send restart-confirm-window show #f))]))
;(send restart-confirm-window show #t))]))

(define restart-no
  (new button%
       [parent restart-confirm-window]
       [label "No"]
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   ;(send msg-area set-label "The game has been restarted.")
                   (send restart-confirm-window show #f))]))

(define win-notif (new dialog% (label "Game Over")
                       (width 400)
                       (height 50)))
(define win-msg (new message% (parent win-notif)
                     (label "")))


;;================================ Do this after every thing has loaded ===========================

(send main-window show #t)
(sleep/yield 0.05)
(draw-board dc)