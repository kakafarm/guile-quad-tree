(use-modules
 (srfi srfi-64)

 (quad-tree no-boxes)
 )

(test-begin "QUAD-TREE")

(define (pair->xy pair)
  (values (car pair) (cdr pair)))

(define region (make-region 0 1 0 1))

(define empty-quad-tree (make-quad-tree region))
(define quad-tree (quad-tree-insert empty-quad-tree
                                    (cons 0.5 0.5)
                                    pair->xy
                                    ))

(format #t "empty tree: ~S~%" empty-quad-tree)
(format #t "tree: ~S~%" quad-tree)

(test-equal #f
  (quad-tree-locate-position empty-quad-tree 0.5 0.5 pair->xy))

(test-equal '((0.5 . 0.5))
  (quad-tree-locate-position quad-tree 0.5 0.5 pair->xy))

(test-end)
