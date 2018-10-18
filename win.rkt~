;#lang racket

;(require "board.rkt")
;(provide (all-defined-out))

;;; Since, in the current turn, a line can only be formed, if it includes
;;; the last played position, we take this position, and check from the
;;; corresponding beginning of every such line.

(define board 0) ; board is an integer; 2 bits each represent the state of each cell
; unset state is 0; other states are -1 and 1.

(define last-played-pos '(0 0 0))
(define pcturn #t)

;; pos - a list of x y z coordinates, in that order
(define (get-value board pos)
  (define state (remainder (quotient board (expt 4 (+ (car pos)
                                                      (* 4 (cadr pos))
                                                      (* 16 (caddr pos)))))
                           4))
  (cond ((= state 2) 1)
        ((= state 3) -1)
        (else 0)))

;; Bug: This does not actually modify the board, but returns the copy of the board
(define (set!-value board pos val)
  (cond ((= 0 (get-value board pos))
         (define part (expt 4 (+ (car pos)
                                 (* 4 (cadr pos))
                                 (* 16 (caddr pos)))))
         (+ (* (+ (* (quotient board (* 4 part)) 4)
                        (cond ((= 1 val) 2)
                              ((= -1 val) 3)
                              (else "Invalid val: " val))) part)
                  (remainder board part)))
        (else (error "Position is already set: " pos))))

(define (board-copy board) board)


;; Inside this, the main function is helper
(define (win? board lpp) ; lpp is for last played position
  ; returns #t if a player has won.

  (define n 3) ; increase n to increase size of board (board-size - 1)
  
  (define line-found #f)

  (define (get-value1 pos)
    (get-value board pos))
  (define (set!-value1 pos)
    (set!-value board pos))
  
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
    (and (apply = (map get-value1 l)) (not (= 0 (get-value1 (car l))))))
  
  (define (border-not-reached? pos)
    ; Returns #t if pos is an invalid (in a greater sense) coordinate
    ;(display "In border-not-reached: ") (displayln pos)
    (and (>= n (car pos)) (>= n (cadr pos)) (>= n (caddr pos))))

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
               (if (= n (+ x y)) (list (list (list 0 n z) (update 1 -1 0))) '())
               (if (= n (+ z y)) (list (list (list x 0 n) (update 0 1 -1))) '())
               (if (= n (+ x z)) (list (list (list (list n y 0) (update -1 0 1)))) '())))
    
    (define (gen-body-diag)
      (append* (if (= x y z) (list (list (list 0 0 0) (update 1 1 1))) '())
               (if (and (= x y) (= n (+ x z)))
                   (list (list (list 0 0 3) (update 1 1 -1))) '())
               (if (and (= x z) (= n (+ x y)))
                   (list (list (list 0 3 0) (update 1 -1 1))) '())
               (if (and (= z y) (= n (+ x z)))
                   (list (list (list (list 3 0 0) (update -1 1 1)))) '())))
    
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