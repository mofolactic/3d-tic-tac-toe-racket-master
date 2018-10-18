;#lang racket

;(require racket/set)
;(include "win.rkt") ; comment out this line later

;; play-n-turns is the main function
;; it uses the all-plays function to generate all the moves the
;; current board can lead to.
;; It then calls the minimax function to evaluate the moves
;; and decide the best possible move.

;; The win? function can be modified, perhaps,
;; coupled with the score function to evaluate partial success


;(define n 3)
;(define (set-minimax-n new-n) (set! n new-n))

(define myturn #f)
;; While dubugging this file myturn should be true.
;; While using this file, from board.rkt, it should be false.

(define (sign)
  (if myturn 1 -1))


;(send board set!-value '(2 0 0) -1)
;(send board set!-value '(2 1 0) -1)
;(send board set!-value '(2 2 0) 1)
;(send board set!-value '(1 2 0) 1)
;(send board set!-value '(0 2 0) 1)

(define (all-plays b-lpp)
  ;; denoting all possible moves
  ;; lpp is for last-played position, used in win? function.
  (define n-cube (* n n n))
  (define (ap-h i)
    (cond ((= i n-cube) '())
          (else
           (define z (quotient i (* n n)))
           (define y (quotient (remainder i (* n n)) n))
           (define x (remainder (remainder i (* n n)) n))
           (define new-board (send (car b-lpp) copy-board))
           (if (= 0 (send (car b-lpp) get-value (list x y z)))
               (cons (cons (send new-board set!-value (list x y z) (sign)) i)
                     (ap-h (+ i 1)))
               (ap-h (+ i 1))))))
  (ap-h 0))

(define (score board-lpp)
  ;(displayln board-lpp)
  (if (apply win? (list (car board-lpp)
                        (let ((i (cdr board-lpp)))
                          (define z (quotient i (* n n)))
                          (define y (quotient (remainder i (* n n)) n))
                          (define x (remainder (remainder i (* n n)) n))
                          (list x y z))))
      (if myturn 10 -10)
      0))
(define (minimax n1 b-lpp) ; b-lpp is a pair of board and lpp
  (define orig-turn myturn)
  (define res
    (cond ((= n1 0)
           (score b-lpp))
          (else
           (let ((points (score b-lpp)))
             (set! myturn (not myturn))
             (define all-moves (all-plays b-lpp))
             (define all-scores
               (map (lambda (b-l)
                      (minimax (- n1 1) b-l))
                    all-moves))
             (if (= 0 points)
                 (if myturn
                       ;(displayln all-moves)
                       ;(displayln all-scores)
                     (apply max all-scores)
                       ;(displayln all-moves)
                       ;(displayln all-scores)
                     (apply min all-scores))
                 points)))))
  (set! myturn orig-turn)
  res)

(define (play-n-turns n1)
  (define all-moves (all-plays (cons board last-played-pos)))
  ;(set! myturn (not myturn))
  ;(displayln all-moves)
  (define all-scores (map (lambda (b-l) (minimax (- n1 1) b-l)) all-moves))
  (define best-score (apply max all-scores))
  
  ;(displayln all-scores)
  (define (search-best m-l s-l)
    (cond ((null? s-l) '()) ;(error "No such move with score: " best-score))
          ((= best-score (car s-l))
           (cons (cdar m-l) (search-best (cdr m-l) (cdr s-l))))
          (else (search-best (cdr m-l) (cdr s-l)))))
  (define best-moves (search-best all-moves all-scores))
  (define move (list-ref best-moves (random (length best-moves))))
  (define z (quotient move (* n n)))
  (define y (quotient (remainder move (* n n)) n))
  (define x (remainder (remainder move (* n n)) n))
  ;(set! myturn (not myturn))
  (list x y z))