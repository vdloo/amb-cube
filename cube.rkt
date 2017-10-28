#!/usr/bin/env racket
#lang racket

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

;; cubie representation:
;; cubie with 3 colors in the list is a corner piece
;; cubie with 2 colors in the list is an edge piece
;; cubie with 1 color in the list is a center piece
;;
;; lists are composed of colors of the current visible 
;; tiles on the cube looking at it from the front 
;; where the top face is white in the finished position 
;; and the front face is blue. 

;; c1 is the bottom left cubie which has no top color
;; because the top is not visible (that would be c4))
;; the left color is 'r for red and the front color 
;; 'b for blue. There is no right color because the
;; right of this cubie is c2. The bottom color is 'y
;; for yellow.

(define (get-tile cube cubie face) 
  (define m 
    (filter 
      (λ(cubie) (eq? face (cdr cubie))) 
      (hash-ref cube cubie)))
  (if (empty? m)
    (raise (format "Cubie-~a-NotFacing-~a-Error" cubie face))
    (caar m)))

(define (create-cube)
  (define c1 
    (list 
      (cons 'r 'left)
      (cons 'b 'front)
      (cons 'y 'bottom)))
  (define c2 
    (list
      (cons 'b 'front)
      (cons 'y 'bottom)))
  (define c3 
    (list
      (cons 'b 'front)
      (cons 'o 'right)
      (cons 'y 'bottom)))
  (define c4 
    (list
      (cons 'r 'left)
      (cons 'b 'front)))
  (define c5 
    (list
      (cons 'b 'front)))
  (define c6 
    (list
      (cons 'b 'front)
      (cons 'o 'right)))
  (define c7 
    (list
      (cons 'w 'top)
      (cons 'r 'left)
      (cons 'b 'front)))
  (define c8 
    (list
      (cons 'w 'top)
      (cons 'b 'front)))
  (define c9 
    (list
      (cons 'w 'top)
      (cons 'b 'front)
      (cons 'o 'right)))
  (define c10 
    (list
      (cons 'r 'left)
      (cons 'y 'bottom)))
  (define c11 
    (list
      (cons 'y 'bottom)))
  (define c12 
    (list
      (cons 'o 'right)
      (cons 'y 'bottom)))
  (define c13 
    (list
      (cons 'r 'left)))
  (define c14 
    (list
      (cons 'o 'right)))
  (define c15 
    (list
      (cons 'w 'top)
      (cons 'r 'left)))
  (define c16 
    (list
      (cons 'w 'top)))
  (define c17 
    (list
      (cons 'w 'top)
      (cons 'o 'right)))
  (define c18 
    (list
      (cons 'g 'back)
      (cons 'r 'left)
      (cons 'y 'bottom)))
  (define c19 
    (list
      (cons 'g 'back)
      (cons 'y 'bottom)))
  (define c20 
    (list
      (cons 'g 'back)
      (cons 'o 'right)
      (cons 'y 'bottom)))
  (define c21 
    (list
      (cons 'g 'back)
      (cons 'r 'left)))
  (define c22 
    (list
      (cons 'g 'back)))
  (define c23 
    (list
      (cons 'g 'back)
      (cons 'o 'right)))
  (define c24 
    (list
      (cons 'w 'top)
      (cons 'g 'back)
      (cons 'r 'left)))
  (define c25 
    (list
      (cons 'w 'top)
      (cons 'g 'back)))
  (define c26 
    (list
      (cons 'w 'top)
      (cons 'g 'back)
      (cons 'o 'right)))
  (hasheq
    'c1 c1 'c2 c2 'c3 c3 'c4 c4 'c5 c5 'c6 c6 'c7 c7 'c8 c8 'c9 c9 
    'c10 c10 'c11 c11 'c12 c12 'c13 c13 'c14 c14 'c15 c15 'c16 c16
    'c17 c17 'c18 c18 'c19 c19 'c20 c20 'c21 c21 'c22 c22 'c23 c23 
    'c24 c24 'c25 c25 'c26 c26))

(define (validate-cube cube)
  (define tiles (flatten (map (λ (cubie) (map car cubie)) (hash-values cube))))
  (unless 
    (eq? 54 (length tiles))
    (raise 'missingtiles))
  (unless 
    (eq? 26 (length (flatten (hash-keys cube))))
    (raise 'missingCubies))
  #t)

(define (valid-cube? cube)
  (with-handlers ([symbol? (thunk* #f)])
    (validate-cube cube)))

(define (print-cube cube) 
  (validate-cube cube)
  (define (get cubie face) 
    (get-tile cube cubie face))

  (display "    front         180° ↺         180° ↶\n")
  (display (format "   +~a-~a-~a---+  " (get 'c24 'top) (get 'c25 'top) (get 'c26 'top)))
  (display (format "   +~a-~a-~a---+  " (get 'c9 'top) (get 'c8 'top) (get 'c7 'top)))
  (display (format "   +~a-~a-~a---+ \n" (get 'c20 'bottom) (get 'c19 'bottom) (get 'c18 'bottom)))
  (display (format "  / ~a ~a ~a  /|  " (get 'c15 'top) (get 'c16 'top) (get 'c17 'top)))
  (display (format "  / ~a ~a ~a  /|  " (get 'c17 'top) (get 'c16 'top) (get 'c15 'top)))
  (display (format "  / ~a ~a ~a  /| \n" (get 'c12 'bottom) (get 'c11 'bottom) (get 'c10 'bottom)))
  (display (format " /  ~a ~a ~a / |  " (get 'c7 'top) (get 'c8 'top) (get 'c9 'top)))
  (display (format " /  ~a ~a ~a / |  " (get 'c26 'top) (get 'c25 'top) (get 'c24 'top)))
  (display (format " /  ~a ~a ~a / | \n" (get 'c3 'bottom) (get 'c2 'bottom) (get 'c1 'bottom)))
  (display (format "+--------+~a~a~a  " (get 'c9 'right) (get 'c17 'right) (get 'c26 'right)))
  (display (format "+--------+~a~a~a  " (get 'c24 'left) (get 'c15 'left) (get 'c7 'left)))
  (display (format "+--------+~a~a~a \n" (get 'c1 'left) (get 'c10 'left) (get 'c18 'left)))
  (display (format "|        |~a~a~a  " (get 'c6 'right) (get 'c14 'right) (get 'c23 'right)))
  (display (format "|        |~a~a~a  " (get 'c21 'left) (get 'c13 'left) (get 'c4 'left)))
  (display (format "|        |~a~a~a \n" (get 'c4 'left) (get 'c13 'left) (get 'c21 'left)))
  (display (format "|~a  ~a  ~a |~a~a~a  " (get 'c7 'front) (get 'c8 'front) (get 'c9 'front) 
                                           (get 'c3 'right) (get 'c12 'right) (get 'c20 'right)))
  (display (format "|~a  ~a  ~a |~a~a~a  " (get 'c26 'back) (get 'c25 'back) (get 'c24 'back) 
                                           (get 'c18 'left) (get 'c10 'left) (get 'c1 'left)))
  (display (format "|~a  ~a  ~a |~a~a~a \n" (get 'c3 'front) (get 'c2 'front) (get 'c1 'front) 
                                            (get 'c7 'left) (get 'c15 'left) (get 'c24 'left)))
  (display (format "|~a  ~a  ~a | /   " (get 'c4 'front) (get 'c5 'front) (get 'c6 'front)))
  (display (format "|~a  ~a  ~a | /   " (get 'c23 'back) (get 'c22 'back) (get 'c21 'back)))
  (display (format "|~a  ~a  ~a | /  \n" (get 'c6 'front) (get 'c5 'front) (get 'c4 'front)))
  (display (format "|~a  ~a  ~a |/    " (get 'c1 'front) (get 'c2 'front) (get 'c3 'front)))
  (display (format "|~a  ~a  ~a |/    " (get 'c20 'back) (get 'c19 'back) (get 'c18 'back)))
  (display (format "|~a  ~a  ~a |/   \n" (get 'c9 'front) (get 'c8 'front) (get 'c7 'front)))
  (display "+--------+     ")
  (display "+--------+     ")
  (display "+--------+    \n"))

(define (translate-tile tile orientation)
  (define tile-orientation (cdr tile))
  (define (retag face) 
    (cons (car tile) face))
  (cond 
    [(eq? orientation 'down)
     (cond [(eq? tile-orientation 'top)
            (retag 'front)]
           [(eq? tile-orientation 'front)
            (retag 'bottom)]
           [(eq? tile-orientation 'bottom)
            (retag 'back)]
           [(eq? tile-orientation 'back)
            (retag 'top)]
           [else tile])]
    [(eq? orientation 'up)
     (cond [(eq? tile-orientation 'top)
            (retag 'back)]
           [(eq? tile-orientation 'back)
            (retag 'bottom)]
           [(eq? tile-orientation 'bottom)
            (retag 'front)]
           [(eq? tile-orientation 'front)
            (retag 'top)]
           [else tile])]
    [(eq? orientation 'left)
     (cond [(eq? tile-orientation 'left)
            (retag 'back)]
           [(eq? tile-orientation 'back)
            (retag 'right)]
           [(eq? tile-orientation 'right)
            (retag 'front)]
           [(eq? tile-orientation 'front)
            (retag 'left)]
           [else tile])]
    [(eq? orientation 'right)
     (cond [(eq? tile-orientation 'left)
            (retag 'front)]
           [(eq? tile-orientation 'front)
            (retag 'right)]
           [(eq? tile-orientation 'right)
            (retag 'back)]
           [(eq? tile-orientation 'back)
            (retag 'left)]
           [else tile])]
    [else raise 'InvalidOrientation]))

(define (reorient-in-cube cube orientation cubie) 
  (define reoriented-cubie 
    (map 
      (λ (tile) (translate-tile tile orientation)) 
      (hash-ref cube cubie)))
  (hash-set cube cubie reoriented-cubie))

(define (reorient-tiles cube orientation to-reorient)
  (if (empty? to-reorient)
    cube
    (reorient-tiles 
      (reorient-in-cube 
        cube 
        orientation
        (car to-reorient))
      orientation
      (cdr to-reorient))))

(define (swap-in-cube cube cubie1 cubie2)
  (define tmp (hash-ref cube cubie1))
  (hash-set 
    (hash-set cube cubie1 (hash-ref cube cubie2)) 
    cubie2 tmp))

(define (swap-pairs cube to-swap)
  (if (empty? to-swap)
    cube
    (swap-pairs
      (swap-in-cube cube 
        (caar to-swap) 
        (cdar to-swap)) 
      (cdr to-swap))))

(define (swap-cubies cube orientation inverse cubies)
  (define cubies-to-swap (if inverse (reverse cubies) cubies))
  (define newcube 
    (swap-pairs cube cubies-to-swap))
  (reorient-tiles 
    newcube 
    orientation
    (remove-duplicates (flatten cubies-to-swap))))

(define (swap-L cube inverse)
  (swap-cubies cube (if inverse 'up 'down) inverse
    (list 
      ;; move corner pieces one face down
      (cons 'c1 'c7)
      (cons 'c7 'c24)
      (cons 'c24 'c18)
      ;; move edge pieces 1 face down
      (cons 'c4 'c15)
      (cons 'c15 'c21)
      (cons 'c21 'c10))))

(define (swap-M cube inverse)
  (swap-cubies cube (if inverse 'up 'down) inverse
    (list 
      ;; move corner pieces one face down
      (cons 'c2 'c8)
      (cons 'c8 'c25)
      (cons 'c25 'c19)
      ;; move edge pieces 1 face down
      (cons 'c5 'c16)
      (cons 'c16 'c22)
      (cons 'c22 'c11))))

(define (swap-R cube inverse)
  (swap-cubies cube (if inverse 'up 'down) inverse
    (list 
      ;; move corner pieces one face down
      (cons 'c3 'c9)
      (cons 'c9 'c26)
      (cons 'c26 'c20)
      ;; move edge pieces 1 face down
      (cons 'c6 'c17)
      (cons 'c17 'c23)
      (cons 'c23 'c12))))

(define (swap-U cube inverse)
  (swap-cubies cube (if inverse 'left 'right) inverse
    (list 
      ;; move corner pieces one face left
      (cons 'c7 'c24)
      (cons 'c24 'c26)
      (cons 'c26 'c9)
      ;; move edge pieces 1 face left
      (cons 'c8 'c15)
      (cons 'c15 'c25)
      (cons 'c25 'c17))))

(define (swap-E cube inverse)
  (swap-cubies cube (if inverse 'left 'right) inverse
    (list 
      ;; move corner pieces one face left
      (cons 'c4 'c21)
      (cons 'c21 'c23)
      (cons 'c23 'c6)
      ;; move edge pieces 1 face left
      (cons 'c5 'c13)
      (cons 'c13 'c22)
      (cons 'c22 'c14))))

(define (swap-D cube inverse)
  (swap-cubies cube (if inverse 'left 'right) inverse
    (list 
      ;; move corner pieces one face left
      (cons 'c1 'c18)
      (cons 'c18 'c20)
      (cons 'c20 'c3)
      ;; move edge pieces 1 face left
      (cons 'c2 'c10)
      (cons 'c10 'c19)
      (cons 'c19 'c12))))

(define (singmaster-rotation cube pattern)
  (if 
    (empty? pattern) cube
    (singmaster-rotation 
      ((car pattern) cube) 
      (cdr pattern))))

(define (L cube) (swap-L cube #f))
(define (Li cube) (swap-L cube #t))
(define (M cube) (swap-M cube #f))
(define (Mi cube) (swap-M cube #t))
(define (R cube) (swap-R cube #t))
(define (Ri cube) (swap-R cube #f))
(define (U cube) (swap-U cube #t))
(define (Ui cube) (swap-U cube #f))
(define (E cube) (swap-E cube #f))
(define (Ei cube) (swap-E cube #t))
(define (D cube) (swap-D cube #f))
(define (Di cube) (swap-D cube #t))
(define (S cube) (singmaster-rotation cube (list U Ei Di M Ui E D)))
(define (Si cube) (singmaster-rotation cube (list U Ei Di Mi Ui E D)))
(define (F cube) (singmaster-rotation cube (list U Ei Di L Ui E D)))
(define (Fi cube) (singmaster-rotation cube (list U Ei Di Li Ui E D)))
(define (B cube) (singmaster-rotation cube (list U Ei Di R Ui E D)))
(define (Bi cube) (singmaster-rotation cube (list U Ei Di Ri Ui E D)))
(define (X cube) (singmaster-rotation cube (list R Mi Li)))
(define (Xi cube) (singmaster-rotation cube (list Ri M L)))
(define (Y cube) (singmaster-rotation cube (list Di Ei U)))
(define (Yi cube) (singmaster-rotation cube (list D E Ui)))
(define (Z cube) (singmaster-rotation cube (list F S Bi)))
(define (Zi cube) (singmaster-rotation cube (list Fi Si B)))

(define (superflip cube)
  (singmaster-rotation cube 
    (list U R R F B R B B R U U L B B R Ui Di R R F Ri L B B U U F F)))

(define (solve-superflip cube)
  (singmaster-rotation cube 
    (list Ri U U B Li F Ui B D F U Di L D D Fi R Bi D Fi Ui Bi U Di)))

(define (match-cube cube pattern)
  (define (check-cubie pattern-cubie)
    (andmap 
      (λ (tile) (member tile (hash-ref cube pattern-cubie))) 
      (hash-ref pattern pattern-cubie)))
  (if (andmap check-cubie (hash-keys pattern)) #t #f))

;; ambiguous evaluator from SICP
;; implementation from http://community.schemewiki.org/?amb
;; https://mitpress.mit.edu/sicp/full-text/sicp/book/node91.html

(define fail (void))
(set! fail 
  (λ () 
    (error "Amb tree exhausted")))
 
(define-syntax amb 
  (syntax-rules () 
    ((amb) (fail))
    ((amb expression) expression)
    ((amb expression ...)
     (let ((fail-save fail)) 
       ((call-with-current-continuation
          (λ (k-success)
            (call-with-current-continuation 
              (λ (k-failure)
                (set! fail
                  (λ () (k-failure #f))) 
                (k-success
                 (λ ()
                   expression))))
            ... 
            (set! fail fail-save)
            fail-save)))))))
 
(define (require condition) 
  (when (not condition) (fail))) 

(define (combinations-with-repetitions combi-set k)
  (cond [(= k 0 ) '(())]
	[(empty? combi-set) '()]
	[(append (combinations-with-repetitions (rest combi-set) k)
		 (map (λ (x) (cons (first combi-set) x))
		      (combinations-with-repetitions combi-set (- k 1))))]))

(define all-rotations (list L Li M Mi R Ri U Ui E Ei D Di S Si F Fi B Bi))

(define (moves-to-try-n n)
  (combinations-with-repetitions all-rotations n))

(define moves-to-try (apply append (map moves-to-try-n (range 0 5))))

(define (build-rotations-iter rotations result)
  (if (false? result) 
    ; if result is False, return the list of rotations
    rotations
    (if (empty? rotations) 
      result
      (build-rotations-iter 
        (cdr rotations) 
        ((car rotations) result)))))

(define (build-rotations rotations)
  (λ (cube) (build-rotations-iter rotations cube)))

(define random-rotation (list-ref all-rotations (random (length all-rotations))))

(define rotations-to-try (map build-rotations moves-to-try))

(define (shuffle-cube-iter cube left)
  (define move (list-ref all-rotations (random (length all-rotations))))
  (display "Shuffling with random move")
  (displayln move)
  (if (< left 0)
    cube
    (shuffle-cube-iter 
      (move cube)
      (- left 1))))

(define (shuffle-cube cube)
  (shuffle-cube-iter cube (random 3)))

(define shuffled-cube (shuffle-cube (create-cube)))

(displayln "The shuffled cube looks like")
(print-cube shuffled-cube)

;; inline list because we can't use 'apply' here, amb is a macro
(define solution (let ((f (eval `(amb ,@rotations-to-try) ns)))
  (require 
    (begin
    (display "trying ")
    (displayln (f #f))
    (match-cube 
      (f shuffled-cube)
      (create-cube))))
  f))

(displayln "The shuffled cube looks like")
(print-cube shuffled-cube)
(display (format "The move to solve the cube is ~a\n" (solution #f)))
(define solved-cube (solution shuffled-cube))
(print-cube solved-cube)

