#lang racket

;; The main function is the mark function. This is called every time
;; the board (canvas) is clicked. This association with click is made in the
;; defined-by-us canvas-with-events% class, of which gui-board is the function.

;; At the start of the application, the draw-board procedure is called,
;; as can be seen at the end of the file.

(require racket/gui)

(define difficulty 3)
;; some issue with n=5
(define n 4)
(define 1-player #t)
(define game-over #f)

(include "win.rkt")
(include "minimax.rkt")

;(set-minimax-n 3)
;(set-win-n 3)

(define main-window (new frame%
                         [label "Tic Tac Toe 3d"]
                         [min-width 750]
                         [min-height 800] ; non-fullHD monitors likely have a height of 720-768px
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
                       [parent h-panel]
                       [min-width 300]
                       [stretchable-width #f]))

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
(define pink-pen (make-object pen% "PINK" 2 'solid))

(define z->pen
  (let* ((z-pen (make-vector 5 #f)))
    (vector-set! z-pen 0 red-pen)
    (vector-set! z-pen 1 yellow-pen)
    (vector-set! z-pen 2 blue-pen)
    (vector-set! z-pen 3 green-pen)
    (vector-set! z-pen 4 pink-pen)
    (lambda (z) (vector-ref z-pen z))))

; Get the (board's) canvas's drawing context
(define dc (send gui-board get-dc))


(define (draw-2d-board dc z)
  (send dc set-pen (z->pen z))    
  (for ((y n))
    (for ((x n))
      (send dc draw-rectangle
            (+ 10 (* 50 x))
            (+ 10 (* (+ 40 (* 30 n)) z) (* 30 y))
            50 30))))

(define (draw-board dc)
  (for ((z n))
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
    (let ()
      (define (ay-h y)
        ;(display "z: ") (displayln z)
        ;(displayln n)
        (cond ((>= z n) (set! valid-position #f))
              ((and (<= (+ (* (+ 40 (* 30 n)) z) 10) y)
                    (< y (+ (* (+ 40 (* 30 n)) z) (* 30 n) 10)))
               ;(set! z (+ z 1))
               (- y (* z 40)))
              (else
               (set! z (+ z 1))
               (ay-h y))))
      (ay-h y)))
  (define corner-x 0)
  (define corner-y 0)
  
  (define (cross)
    (send dc draw-line (+ 20 corner-x) (+ 10 corner-y) (+ 30 corner-x) (+ 20 corner-y))
    (send dc draw-line (+ 30 corner-x) (+ 10 corner-y) (+ 20 corner-x) (+ 20 corner-y)))
  
  (define (circle)
    (send dc draw-ellipse (+ 20 corner-x) (+ 10 corner-y) 10 10))
  
  (if (or (>= x (+ 10 (* 50 n))) (< x 10)) (set! valid-position #f) (void))
  
  (define (make-turn)
    ;(displayln (list x adjusted-y))
    
    (define cell-x (floor (/ (- x 10) 50)))
    (define cell-y (floor (/ (- adjusted-y 10 (* z (* n 30))) 30)))
    (define cell-z z)
    
    (set! last-played-pos (list cell-x cell-y cell-z))  
    ;(displayln last-played-pos)
    

    (if (and (= 0 (send board get-value last-played-pos)) (not game-over))
        (begin
          (set! corner-x (+ 10 (* cell-x 50)))
          (set! corner-y (+ 10 (* cell-y 30)  (* cell-z (+ (* 30 n) 40))))
          (send dc set-pen black-pen)
          (cond (myturn
                 (circle)
                 (send board set!-value last-played-pos 1)
                 (send process-msg-area set-label "First player's turn: X"))
                (else
                 (cross)
                 (set! board (send board set!-value last-played-pos -1))
                 (if 1-player
                     (send process-msg-area set-label "Second player's turn: O
          Thinking...")
                     (send process-msg-area set-label "Second player's turn: O"))))
          (if (win? board last-played-pos)
              (begin
                (game-over-processes)
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
        (if 1-player
            (let ()
              (define pc-pos (play-n-turns difficulty))
              ;(display "PC Pos:") (displayln pc-pos)
              (set! x (+ (* (car pc-pos) 50) 10))
              (set! z (caddr pc-pos))
              (set! adjusted-y (+ (* (cadr pc-pos) 30) 10 (* 30 n z)))
              ;(floor (/ (- adjusted-y 10 (* z 120)) 30))))
              (make-turn))
            (void)))
      (void)))

(define (game-over-processes)
  (set! myturn #f)
  (set! game-over #t)
  (send process-msg-area set-label "")
  (send msg-area set-label "Game Over. Restart to play again."))

(define (restart-processes)
  (send dc erase)
  (draw-board dc)
  (send board reset)
  (set! game-over #f)
  (if 1-player
      (send msg-area set-label
        "The game has been restarted.
              (Player vs PC)")
      (send msg-area set-label
            "The game has been restarted.
           (Player vs Player)"))
  (send process-msg-area set-label "You play first."))

;----------------------------------- End of GUI Board ------------------------------------

;;============================== Begin buttons and dialogs ===============================
(define v-panel (new vertical-panel% (parent h-panel)))

(define msg-area (new message%
                      [parent v-panel]
                      [vert-margin 50]
                      [label "                Welcome!\n
Use the restart button to choose
between 1 player and 2 player."]
                      [min-width 250]
                      [font (make-object font% 13.5 'system)]
                      [auto-resize #t]))

(define restart-btn
  (new button% [parent v-panel]
       
       [label "RESTART"]
       [font (make-object font% 15 'system)]
       [min-width 150]
       [min-height 50]
       
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

(define restart-yes-1
  (new button%
       [parent restart-confirm-window]
       [label "Yes, 1 player (Player vs PC)"]
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   (draw-board dc)
                   (send board reset)
                   (set! game-over #f)
                   (set! 1-player #t)
                   (send msg-area set-label
                         "The game has been restarted.
              (Player vs PC)")
                   (send restart-confirm-window show #f)
                   (send process-msg-area set-label "You play first."))]))

(define restart-yes-2
  (new button%
       [parent restart-confirm-window]
       [label "Yes, 2 players (Player vs Player)"]
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   (draw-board dc)
                   (send board reset)
                   (set! game-over #f)
                   (set! 1-player #f)
                   (send msg-area set-label
                         "The game has been restarted.
           (Player vs Player)")
                   (send restart-confirm-window show #f)
                   (send process-msg-area set-label "You play first."))]))
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
                     (label "")
                     (horiz-margin 50)
                     (auto-resize #t)
                     [font (make-object font% 15 'system)]))

;;---------------- Below the restart button -------------------------------

(define process-msg-area
  (new message%
       [parent v-panel]
       [vert-margin 50]
       [label "You play first."]
       [min-width 250]
       [font (make-object font% 13.5 'system)]
       [auto-resize #t]))

;;----------------------------- Board Size --------------------------------------

(define set-board-size
  (new button% [parent v-panel]
       
       [label "Set size"]
       [font (make-object font% 12 'system)]
       [min-width 150]
       [min-height 50]
       [vert-margin 20]
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   ;(send msg-area set-label "The game has been restarted."))]))
                   (send resize-board-confirm show #t))]))

(define resize-board-confirm
  (new dialog%
       (label "Set Size")))

(define resize-confirm-msg
  (new message% (parent resize-board-confirm)
       (vert-margin 10)
       (horiz-margin 20)
       (min-width 250)
       (label "WARNING: CLICKING ON ANY OF THE BUTTONS
BELOW WILL RESTART THE GAME.

To avoid restarting, close using the X button
of this dialog box.

Note: 5x5x5 board may not be displayed on screen.")))

(define size-choice-box
  (new radio-box%
       (label "Board Size")
       (parent resize-board-confirm)
       (choices (list "3x3x3" "4x4x4" "5x5x5"))
       (selection 1)
       (callback (lambda (button event)
                   (define diff (send size-choice-box get-selection))
                   (match diff
                     (0 (set! n 3))
                     (1 (set! n 4))
                     (2 (set! n 5)))
                   (restart-processes)
                   (send resize-board-confirm show #f)))))

;;----------------------------- Difficulty --------------------------------------

(define set-board-difficulty
  (new button% [parent v-panel]
       
       [label "Change difficulty"]
       [font (make-object font% 12 'system)]
       [min-width 150]
       [min-height 50]
       [vert-margin 20]
       
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   ;(send msg-area set-label "The game has been restarted."))]))
                   (send difficulty-board-confirm show #t))]))

(define difficulty-board-confirm
  (new dialog%
       (label "Set Difficulty")))

(define difficulty-confirm-msg
  (new message% (parent difficulty-board-confirm)
       (vert-margin 10)
       (horiz-margin 20)
       (min-width 250)
       (label "WARNING: CLICKING ON ANY OF THE BUTTONS
BELOW WILL RESTART THE GAME.

To avoid restarting, use the X button of this
dialog box.

4x4x4 or 5x5x5 board will be very slow on
Hard difficulty.")))

(define difficulty-choice-box
  (new radio-box%
       (label "Difficulty")
       (parent difficulty-board-confirm)
       (choices (list "Easy" "Medium" "Hard"))
       (selection 1)
       (callback (lambda (button event)
                   (define diff (send difficulty-choice-box get-selection))
                   (match diff
                     (0 (set! difficulty 2))
                     (1 (set! difficulty 3))
                     (2 (set! difficulty 4)))
                   (restart-processes)
                   (send difficulty-board-confirm show #f)))))


;;================================ Do this after every thing has loaded ===========================

(send main-window show #t)
(sleep/yield 0.05)
(draw-board dc)