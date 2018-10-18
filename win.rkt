;#lang racket

;(require "board.rkt")
;(provide (all-defined-out))

;;; Since, in the current turn, a line can only be formed, if it includes
;;; the last played position, we take this position, and check from the
;;; corresponding beginning of every such line.

; board is an integer; 2 bits each represent the state of each cell
; unset state is 0; other states are -1 and 1.

;(define n 3)
;(define (set-win-n new-n) (set! n new-n))

(define last-played-pos '(0 0 0))

;; Bug: This does not actually modify the board, but returns the copy of the board

(define (board-copy board) board)

(define board%
  (class object%
    (super-new)
    (init-field (board-state 0))
    ;(init-field)
    (define/public (copy-board)
      (new board% (board-state board-state)))
   
    (define/public (display-board)
      (display "(")
      (for ((i n))
        (display "(")
        (for ((j n))
          (display "(")
          (for ((k n))
            (display (get-value (list k j i))))
          (display ")"))
        (displayln ")"))
      (displayln ")"))
    
    ;; pos - a list of x y z coordinates, in that order
    (define/public (get-value pos)
      ;(displayln pos)
      (define state (remainder (quotient board-state (expt 4 (+ (car pos)
                                                                (* n (cadr pos))
                                                                (* (* n n) (caddr pos)))))
                               4))
      (cond ((= state 2) 1)
            ((= state 3) -1)
            (else 0)))
    
    (define/public (set!-value pos val)
      (cond ((= 0 (get-value pos))
             (define part (expt 4 (+ (car pos)
                                     (* n (cadr pos))
                                     (* (* n n) (caddr pos)))))
             (set! board-state
                   (+ (* (+ (* (quotient board-state (* 4 part)) 4)
                            (cond ((= 1 val) 2)
                                  ((= -1 val) 3)
                                  (else "Invalid val: " val))) part)
                      (remainder board-state part)))
             this)
            (else (error "Position is already set: " pos))))

    (define/public (reset)
      (set! board-state 0))))

(define board (new board%))

;; Inside this, the main function is helper
(define (win? board lpp) ; lpp is for last played position
  ; returns #t if a player has won.

  (define n1 (- n 1)) ; increase n to increase size of board (board-size - 1)
  
  (define line-found #f)

  (define (get-value1 pos)
    (send board get-value pos))
  ;(define (set!-value1 pos)
   ; (send board set!-value pos))
  
  (define (get-line init-update)
    (define gl-init (car init-update))
    (define gl-update (cadr init-update))
    ;(displayln gl-init)
    ;(displayln gl-update)
    (while/list border-not-reached? gl-update gl-init))

  (define (in-a-line? l)
    ; takes in a list of coordinates and checks if they form a complete line
    ; currently does not check if the position is unset.
    ;(displayln "In in-a-line?")
    (and (if (null? (map get-value1 l)) #f
             (apply = (map get-value1 l)))
         (not (= 0 (get-value1 (car l))))))
  
  (define (border-not-reached? pos)
    ; Returns #t if pos is an invalid (in a greater sense) coordinate
    ;(display "In border-not-reached: ") (displayln pos)
    (and (>= n1 (car pos)) (>= n1 (cadr pos)) (>= n1 (caddr pos))))

  ;;; CAN BE A BOTTLENECK WITHOUT VECTORS
  (define (update x y z)
        ; returns a procedure that tells how the init coordinates should be chamged
        ; used in check eqv method
        (lambda (pos) (list (+ x (car pos)) (+ y (cadr pos)) (+ z (caddr pos)))))
  
            
  (define (update-methods)
    ;;; returns a list of (list init and update methods) corresponding to last played position
    (define x (car lpp))
    (define y (cadr lpp))
    (define z (caddr lpp))
    
    (define (gen-axes)
      ; works out the same for x y and z directions
      (let* ((init-x (list 0 y z))
             (init-y (list x 0 z))
             (init-z (list x y 0))
             (update-x (update 1 0 0))
             (update-y (update 0 1 0))
             (update-z (update 0 0 1)))
        (list (list init-x update-x) (list init-y update-y) (list init-z update-z))))
    
    (define (gen-plane-diag)
      (append* (if (= x y) (list (list (list 0 0 z) (update 1 1 0))) '())
               (if (= z y) (list (list (list x 0 0) (update 0 1 1))) '())
               (if (= x z) (list (list (list 0 y 0) (update 1 0 1))) '())
               (if (= n1 (+ x y)) (list (list (list 0 n1 z) (update 1 -1 0))) '())
               (if (= n1 (+ z y)) (list (list (list x 0 n1) (update 0 1 -1))) '())
               (if (= n1 (+ x z)) (list (list (list (list n1 y 0) (update -1 0 1)))) '())))
    
    (define (gen-body-diag)
      (append* (if (= x y z) (list (list (list 0 0 0) (update 1 1 1))) '())
               (if (and (= x y) (= n1 (+ x z)))
                   (list (list (list 0 0 n1) (update 1 1 -1))) '())
               (if (and (= x z) (= n1 (+ x y)))
                   (list (list (list 0 n1 0) (update 1 -1 1))) '())
               (if (and (= z y) (= n1 (+ x z)))
                   (list (list (list (list n1 0 0) (update -1 1 1)))) '())))
    
    (append (gen-axes) (gen-plane-diag) (gen-body-diag)))

  
  (define (helper)
    ; the main function of win?
    (define init-update-list (update-methods))
    ;(displayln init-update-list)
    (define (check-all-lines l) ; l would be a list of init-updates
      (cond (line-found #t)
            ((null? l) #f)
            (else
             (begin
               (set! line-found (in-a-line? (get-line (car l))))
               (check-all-lines (cdr l))))))
    (check-all-lines init-update-list)
    line-found)
    
  (helper))
      
;; returns a list containing f applied increasing number of times
;; the values are accumulated
(define (while/list p f x)
  ;(displayln x)
  ;(displayln f)
  (if (p x) (cons x (while/list p f (f x))) '()))