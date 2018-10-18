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

(define move-tree #f)

(struct gnode (val subtrees)); #:transparent)
(struct leaf (val)); #:transparent)
(define (get-val node)
  (match node
    ((leaf val) val)
    ((gnode val subtrees) val)))

(define myturn #f)
(define (sign)
  (if myturn 1 -1))

;(set! last-played-pos '(2 2 0))
;
;(set! board (set!-value board '(2 0 0) -1))
;(set! board (set!-value board '(2 1 0) -1))
;;(set! board (set!-value board '(2 2 0) -1))
;;(set! board (set!-value board '(2 3 0) 1))
;(set! board (set!-value board '(0 2 0) -1))
;(set! board (set!-value board '(1 2 0) -1))

(define (all-plays b-lpp)
  ;; denoting all possible moves
  ;; lpp is for last-played position, used in win? function.
  (define (ap-h i)
    (cond ((= i 64) '())
          (else
           (define z (quotient i 16))
           (define y (quotient (remainder i 16) 4))
           (define x (remainder (remainder i 16) 4))
           (if (= 0 (get-value (car b-lpp) (list x y z)))
               (cons (cons (set!-value (car b-lpp) (list x y z) (sign)) i)
                     (ap-h (+ i 1)))
               (ap-h (+ i 1))))))
  (ap-h 0))

(define (score board-lpp)
  (if (apply win? (list (car board-lpp)
                        (let ((i (cdr board-lpp)))
                          (define z (quotient i 16))
                          (define y (quotient (remainder i 16) 4))
                          (define x (remainder (remainder i 16) 4))
                          (list x y z))))
      (if myturn 10 -10)
      0))

(define (remove-all ele l)
  (cond ((null? l) '())
        ((equal? ele (car l)) (remove-all ele (cdr l)))
        (else (cons (car l) (remove-all ele (cdr l))))))

(define (minimax n b-lpp) ; b-lpp is a pair of board and lpp
  (define orig-turn myturn)
  (define res
    (cond ((= n 0)
           (score b-lpp))
          (else
           (let ((points (score b-lpp)))
             (set! myturn (not myturn))
             (define all-moves (all-plays b-lpp))
             (define all-scores
               (map (lambda (b-l)
                      (minimax (- n 1) b-l))
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

(define (play-n-turns n)
  (define all-moves (all-plays (cons board last-played-pos)))
  ;(set! myturn (not myturn))
  (define all-scores (map (lambda (b-l) (minimax (- n 1) b-l)) all-moves))
  (define best-score (apply max all-scores))
  ;(displayln all-moves)
  ;(displayln all-scores)
  (define (search-best m-l s-l)
    (cond ((null? s-l) (error "No such move with score: " best-score))
          ((= best-score (car s-l)) (cdar m-l))
          (else (search-best (cdr m-l) (cdr s-l)))))
  (define move (search-best all-moves all-scores))
  (define z (quotient move 16))
  (define y (quotient (remainder move 16) 4))
  (define x (remainder (remainder move 16) 4))
  ;(set! myturn (not myturn))
  (list x y z))