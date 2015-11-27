;проверява дали един елемент се повтаря в списък
(define ( isRepeated? elem lst)
  (member elem lst))

;извежда първата низходяща редица започвайки от началото
(define (find-down lst)
  (define (help previous res lst)
  ( if ( null? lst)
       res
       ( if( >= previous ( car lst))
           (help (car lst) ( cons (car lst) res) (cdr lst))
           res)))
  (reverse (help (car lst) (list (car lst)) (cdr lst))))
  
;намира максимален елемент от списък съдържащ празни списъци
( define (help max lst)
     (if( null? lst)
        max
     ( if ( < (length max) (length (car lst) ) )
          (help (car lst) (cdr lst))
          (help max (cdr lst)))))

; извеждаме първа колона на матрица
(define (show-column matrix)
    ( map car matrix))
