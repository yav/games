; Colors
(declare-datatypes () ((Color  Red Green Blue)))

; Shapes
(declare-datatypes () ((Shape Circle Triangle Square)))

; Locations

(declare-datatypes () ((Loc (loc (row Int) (col Int)))))

(define-fun loc-max () Int 4)

(define-fun loc-valid ((x Loc)) Bool
  (and (<= 0 (row x)) (<= (row x) loc-max)
       (<= 0 (col x)) (<= (col x) loc-max))
)

(define-fun loc-left-of ((x Loc) (y Loc)) Bool
  (and (= (+ 1 (col x)) (col y))
       (= (row x) (row y)))
)

(define-fun loc-right-of ((x Loc) (y Loc)) Bool
  (loc-left-of y x)
)

(define-fun loc-below ((x Loc) (y Loc)) Bool
  (and (= (+ 1 (row x)) (row y))
       (= (col x) (col y)))
)

(define-fun loc-above ((x Loc) (y Loc)) Bool
  (loc-below y x))

(define-fun loc-touching ((x Loc) (y Loc)) Bool
  (or (loc-left-of x y) (loc-right-of x y)
      (loc-above x y)   (loc-below x y)))


; Items
(declare-datatypes () ((Item (item (color Color)
                                   (shape Shape)
                                   (at    Loc))
                      )))

(define-fun item-valid ((x Item)) Bool
  (loc-valid (at x)))

(define-fun item-touching ((x Item) (y Item)) Bool
  (loc-touching (at x) (at y)))













