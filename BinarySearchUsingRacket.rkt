#lang racket
(provide (all-defined-out))

(struct Tree (l v r) #:transparent)
;;left(l) is a node or a Tree
;;right(r) is a node or a Tree
;;v is a number


;;Sometimes called a leaf
(struct Node (v) #:transparent)
;;Containts a number or the boolean #f

#|

(Tree? x): checks if x is a Tree
• (Tree-l x): gets the value of the field l from the tree x
• (Tree-v x): gets the value of the field v from the tree x
• (Tree-r x): gets the value of the field r from the tree x

|#

(define (make-bst lst)
  (cond [(empty? lst) (Node #f)]
        [else (Node (car lst))]))
(define null-tree (make-bst '()))
(define tree1 (make-bst '(5)))
null-tree
tree1

(define (insert-bst v bst)
  (cond [(Node? bst) (if (Node-v bst)
                         ;;contains value
                         (if (< v (Node-v bst))
                             (Tree (Node v)
                                   (Node-v bst)
                                   (Node #f))
                             (Tree (Node #f)
                                   (Node-v bst)
                                   (Node v)))
                         ;;false
                         (Node v))]
        [else (if (< v (Tree-v bst))
                  (Tree (insert-bst v (Tree-l bst));;l
                        (Tree-v bst);;v
                        (Tree-r bst));;r
                  (Tree (Tree-l bst);;l
                        (Tree-v bst);;v
                        (insert-bst v (Tree-r bst)));;r
                  )]))
(insert-bst 4 null-tree)

(define tree2 (insert-bst 4 (insert-bst 12 (insert-bst 10 tree1))))

(define (member-bst? v bst)
  (cond [(Node? bst) (eq? v (Node-v bst))]
        [(eq? (Tree-v bst) v) #t]
        [(< v (Tree-v bst)) (member-bst? v (Tree-l bst))]
        [else (member-bst? v (Tree-r bst))]))

(member-bst? 10 tree2)
(member-bst? 5 tree2)
(member-bst? 9 tree2)

(define tree3 (insert-bst 13 (insert-bst 6 (insert-bst 5 (make-bst '(9))))))

#|
(define (merge bst1 bst2)
  (cond [(and (Node? bst1)
              (not (Node-v bst1)))
         bst2]
        [(and (Node? bst2)
              (not (Node-v bst2)))
         bst1]
|#
(define (bst->list bst)
  (cond [(Node? bst) (if (Node-v bst)
                         (list (Node-v bst))
                         (list))]
        [else (append (bst->list (Tree-l bst))
                      (list (Tree-v bst))
                      (bst->list (Tree-r bst)))]))
(bst->list tree2)

(define (insert-lst-bst lst bst)
  (cond [(empty? lst) bst]
        [else (insert-lst-bst (cdr lst)
                              (insert-bst (car lst) bst))]))

(define (merge bst1 bst2)
  (insert-lst-bst (bst->list bst1) bst2))

(define tree4 (merge tree2 tree3))

(bst->list tree4)

(define (height bst)
  (cond [(Node? bst) 1]
        [else (+ 1 (max (height (Tree-l bst))
                        (height (Tree-r bst))))]))

(height tree2)
(height tree3)

(define (treemax bst)
  (last (bst->list bst)))

(treemax tree2)
(treemax tree3)

;;FIX NAMES LATER ON SINCE WE ALREADY DEFINED THIS FUNCTION
(define (tree-to-list bst)
  (cond [(Node? bst) (if (Node-v bst)
                         (list (Node-v bst))
                         (list))]
        [else (append (tree-to-list (Tree-l bst))
                      (list (Tree-v bst))
                      (tree-to-list (Tree-r bst)))]))

;;Q4
(define (check-bst bst)
  (define (check lst)
    (cond [(empty? (cdr lst)) #t]
          [(not (< (car lst) (cadr lst))) #f]
          [else (check (cdr lst))]))
  (check (tree-to-list bst)))

(check-bst tree2)
(check-bst tree3)

(define (treemap f bst)
  (cond [(Node? bst) (Node (f (Node-v bst)))]
        [else (Tree (treemap f (Tree-l bst))
                    (f (Tree-v bst))
                    (treemap f (Tree-r bst)))]))

(define (add-one x)
  (if x
   (+ x 1)
   #f))

(tree-to-list (treemap add-one tree4))

(define (treefilter f bst)
  (cond [(Node? bst) (if (f (Node-v bst))
                         bst
                         (Node #f))]
        [(f (Tree-v bst)) (Tree (treefilter f (Tree-l bst))
                                (Tree-v bst)
                                (treefilter f (Tree-r bst)))]
        [else (treefilter f (merge (Tree-l bst)
                                  (Tree-r bst)))]))

(define (tree-even? x)
  (if x
      (even? x)
      #f))

(treefilter tree-even? (treemap add-one tree4))


;;ANSWERS TO 4.4

;;1 The tree returned by the tree map is not guarnteed tree because we do not know
;; the function being passed on to the BST
;; FOR EXAMPLE
;; IF THE BST contains negatives and positive numbers
;;IF the function were to inverst sign of all the values in the tree
;; we would would with negatives numbers on the right side of the tree
;; and positive numbers numbers on the left side of the tree
;; which would be an improper bst

;;2 Yes because it is only removing nodes that do not pass the function
;; this preserves the numerical structure present in the tree originally
                        
              
