(use-modules
 (srfi srfi-64)

 (ice-9 match)
 (ice-9 pretty-print)

 (quad-tree)
 )

(test-begin "QUAD-TREE")

(define region (make-region 0 1 0 1))

(define empty-quad-tree (make-quad-tree region))

(define position-x (/ 2))
(define position-y (/ 2))
(define value 'poop)

(define quad-tree (quad-tree-insert empty-quad-tree
                                    position-x
                                    position-y
                                    'poop))

(define (make-item denominator)
  (let* ((enumerator-x (random (1+ denominator)))
         (enumerator-y (random (1+ denominator)))
         (x (/ enumerator-x
               denominator))
         (y (/ enumerator-y
               denominator)))
    (list x
          y
          (list (exact->inexact x)
                (exact->inexact y)))))

(define fifty-items
  (let loop ((n 50)
             (items '()))
    (cond
     ((= 0 n)
      items)
     (else
      (loop
       (1- n)
       (cons (make-item 100) items))))))

(define fifty-item-tree
  (let loop ((quad-tree empty-quad-tree)
             (items fifty-items))
    (match items
      ('() quad-tree)
      (((x y value) . rest-of-items)
       (format #t "quad-tree:~%")
       (pretty-print quad-tree)
       (format #t "x: ~S
y: ~S
value: ~S
"
               x
               y
               value)
       (loop (quad-tree-insert quad-tree
                               x
                               y
                               value)
             rest-of-items)))))

(format #t "empty tree: ~S~%" empty-quad-tree)
(format #t "location in empty tree: ~S~%" (quad-tree-locate-position empty-quad-tree
                                                                     position-x
                                                                     position-y))
(format #t "tree: ~S~%" quad-tree)
(format #t "existing location in tree: ~S~%" (quad-tree-locate-position quad-tree
                                                                        position-x
                                                                        position-y
                                                                        ))

(format #t "fifty item tree: ~S~%" fifty-item-tree)

(test-equal #f
  (quad-tree-locate-position empty-quad-tree
                             position-x
                             position-y))

(test-equal value
  (quad-tree-locate-position quad-tree
                             position-x
                             position-y))

(for-each (lambda (item)
            (test-equal (caddr item)
              (quad-tree-locate-position fifty-item-tree
                                         (car item)
                                         (cadr item))))
          fifty-items)

(test-end)
