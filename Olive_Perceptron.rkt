#lang racket

(require math/matrix
         math/array
         plot)

(provide (all-defined-out))

;included 10 "fake" data for each of the classes (-1.0 and 1.0). To ensure that these data points
; were linearly seperable, I graphed out each of the x1 and x2 on desmos to visualize this
(define my-data (matrix [[0.3 0.7 -1.0]
                         [0.7 0.3 -1.0]
                         [-0.5 2.0 -1.0]
                         [0.1 0.3 -1.0]
                         [0.3 0.6 -1.0]
                         [2.0 3.0 1.0]
                         [4.9 3.2 1.0]
                         [5.0 0.2 1.0]  ;; I don't think this function is necessary since you are going to be testing the two sets seperately. -tee
                         [3.0 1.2 1.0]
                         [3.1 0.2 1.0]
                         [1.0 1.1 -1.0]
                         [-0.3 0.4 -1.0]
                         [-0.5 0.3 -1.0]
                         [-0.6 0.5 -1.0]
                         [0.2 0.6 -1.0]
                         [2.5 4.0 1.0]
                         [2.6 3.1 1.0]
                         [2.7 0.5 1.0]
                         [2.3 0.3 1.0]
                         [3.2 4.0 1.0]
                             ]))

; manually separated the data into training-sets and testing-set
(define trainingset (matrix [[0.3 0.7 -1.0]
                             [0.7 0.3 -1.0]
                             [-0.5 2.0 -1.0]
                             [0.1 0.3 -1.0]
                             [0.3 0.6 -1.0]
                             [2.0 3.0 1.0]
                             [4.9 3.2 1.0]
                             [5.0 0.2 1.0]
                             [3.0 1.2 1.0]
                             [3.1 0.2 1.0]
                             ]))

(define testingset (matrix [[1.0 1.1 -1.0]
                            [-0.3 0.4 -1.0]
                            [-0.5 0.3 -1.0]
                            [-0.6 0.5 -1.0]
                            [0.2 0.6 -1.0]
                            [2.5 4.0 1.0]
                            [2.6 3.1 1.0]
                            [2.7 0.5 1.0]
                            [2.3 0.3 1.0]
                            [3.2 4.0 1.0]
                            ]))


;; original code was left untouched
(define (get-new-wts inp ws input-class #:threshold [threshold 0])
  (let* ([est-class (if (>= (matrix-ref
                            (matrix* inp ws) 0 0)
                           threshold)
                        1 -1)]
         [beta (* est-class input-class)])
    (matrix+ (matrix-scale (matrix-transpose inp) (* beta est-class ))
             ws)))
  
(define (wt-update ins wts)
  (let* ([num-cols (matrix-num-cols ins)]
         [inp (submatrix ins (list 0) (in-range (- num-cols 1)))]
         [input-class (matrix-ref ins 0 (- num-cols 1))])
    (get-new-wts inp wts input-class)))

(define (one-loop-through-data data starting-wts)
  (let ([first-row (submatrix data (list 0) (in-range (matrix-num-cols data)))])
    (for*/fold
     ([in-data first-row]
     [upd-wts (list starting-wts)]
    #:result (reverse (cons (wt-update
                             (submatrix data (list (- (matrix-num-rows data) 1))
                                        (in-range (matrix-num-cols data)))
                             (first upd-wts)) upd-wts)))
      ([r (in-range 1 (matrix-num-rows data))])
      (values (submatrix data (list r) (in-range (matrix-num-cols data)))
              (cons (wt-update in-data (first upd-wts)) upd-wts)))))

(define (parse-points lrnd)
  (for/list ([p lrnd])
    (list (list 0 0) (matrix->list p))))

(define (wt-plot lrnd)
  (plot (for/list ([pp (parse-points lrnd)]
                    [i (in-range 100)])
          (arrows pp #:color i
                  #:width (* i 1.5)))))
                
;; (1) we want to test the training-set by setting an initial-weight. We can update this weight
;; by running it through the one-loop-through-data function. Using these new set of weights,
;; we will input it into the perceptron function that we'll 

(define initial-weights (matrix [[0.1] [0.1]]))

(define updated-weights (one-loop-through-data trainingset initial-weights))

(display "Final updated weights: ")
(display updated-weights)

;; I'm struggling with the matrix functions for this assignment. To make it simpler for myself, I
;; will be putting everything as a list.
(define (matlis set)
  (matrix->list* set))
(define mydatal (matlis my-data))
(define trainingsetl (matlis trainingset))
(define testingsetl (matlis testingset))

;; creating a manual list of the updated weights from trainingset
(define new-weights
  (list -0.6 -0.8999999999999999
        -2.9 -3.1999999999999997
        -3.8 -0.7999999999999998
        2.4000000000000004 2.6000000000000005
        3.8000000000000007 4.000000000000001))


;; Creating the perceptron: add circuit + activation function

(define (activation scl #:threshold [th 0])
  (if (>= scl th) 1 -1))

(define (perceptron data wts #:bias[bias 1])
 (for ([item data]
       [ws wts]
       [acc '()])
   (display (cons (activation (foldr + 0 (map (lambda (x) (* x ws)) item)))  acc))))
       
