(define (ends-with suffix str)


   (define (str-lists-equal lst1 lst2) 
     ( cond
      ((not (= (length lst1) (length lst2))) #f)
      ((and (null? lst1) (null? lst2)) #t)
      (else (and (char=? (car lst1) (car lst2)) (str-lists-equal (cdr lst1) (cdr lst2) )) )
     )
   )

  (define (ends-with-lists lst1 lst2)
     (cond
      ((str-lists-equal lst1 lst2) #t)
      ((null? lst2) #f)
      (else (ends-with-lists lst1 (cdr lst2)) )
     )
   )
  
  (ends-with-lists (string->list suffix) (string->list str))
)
  
  

(define (map1 f ls) 
	(if (null? ls) ls
	(cons (f (car ls)) (map1 f (cdr ls))))
  ) 
		

(define (filter2 p ls)
  (if (null? ls)
      ls
      (let*
      (               
           (hd (car ls))
           (rest (cdr ls))
       )     
      (if (p hd) (cons hd (filter2 p rest)) (filter2 p rest))
       )
      )
 )


(define (reduce binFunc u)
  (define (reduce-helper ls)
    (if (null? ls)
        u
        (binFunc (car ls) (reduce-helper (cdr ls)))))
  reduce-helper)


(define (mul-of-pairs suffix ls)

 (define (get_numbers lst_of_pairs)
   (if (= (length lst_of_pairs) 0) '() (cons (cdr (car lst_of_pairs)) (get_numbers (cdr lst_of_pairs)) ) )
   )

  (define (tuple_ends_with_suffix tuple)
    (ends-with suffix (car tuple)))

  
  (define mult (reduce (lambda (x y) (* x y)) 1))
  
 (mult (get_numbers (filter2 tuple_ends_with_suffix ls)))
 
  )

;merging two lists

(define (merge ls1 ls2)
  (cond
    ((null? ls1) ls2)
    ((null? ls2) ls1)
    (else (append ( cons (car ls1) (cons (car ls2) '() ))  (merge (cdr ls1) (cdr ls2))  ) ))
  )

;rotating a list

(define (rotate ls n)

  
  (define (list-cut lst k) ;;remove k last elements from lst.
   (define (remove-first-k partial-list i k)
     (if (= i k) partial-list
     (remove-first-k (cdr partial-list) (+ 1 i) k)
       )
      )
     (reverse (remove-first-k (reverse lst) 0 k) )
     )

  (define (rortate-one ls)
     (if (= (length ls) 0) ls
      
      (let*
      (               
           (n (length ls ))
           (tail (list-ref ls (- n 1)      ) )
           (tail-as-list (cons tail '()))

           )
       (append tail-as-list (list-cut ls 1))
      )
    )
    )

  (define (rotate-helper ls i)
    (if (= i n) ls
     (rotate-helper (rortate-one ls) (+ i 1)))  
  )

  (rotate-helper ls 0)
  )


;quicksort

(define (quicksort comp)
  
   (define (get-bigger ls comp k)
    (filter2 (lambda (x) (> (comp x k) 0)) ls)
  )

     (define (get-smaller ls comp k)
    (filter2 (lambda (x) (< (comp x k) 0)) ls)
  )

       (define (get-equal ls comp k)
    (filter2 (lambda (x) (= (comp x k) 0)) ls)
  )
  

  (define (sort-func ls)
    
   (if (null? ls) '()
   
    (let*
       (
        (first (car ls))
        (smaller (get-smaller  ls comp first))
        (bigger (get-bigger  ls comp first))
         (equal (get-equal  ls comp first))
      )
      (append (sort-func smaller) equal (sort-func bigger))
     )
    
   )
  )
  sort-func

)


; fucntions for sequences

(define (tail s)(
                (cdr s)
         )
)

(define (hd s)
  (car s))

(define (seq-gen n g)
  (define (h) (seq-gen (g n) g) )
  (cons n h)
  )

(define (seq n)
  (seq-gen n (lambda (n)(+ n 1)))
  )

(define (cyclic-seq ls)
  (define (h) (cyclic-seq (rotate ls (- (length ls) 1))))
  (cons (car ls) h)
  )

  

; a basic functional dictionary (not efficient.)

(define (make-dictionary)
  
  (define (dictionary ls)
    
    (define (addValue lst key val rest) ; a function for adding new values (overriding exiting ones)

      (cond ((null? lst) (append  rest (list (cons key val)))) ;if lst is null, the value doesn't exist - add it.
            ((string=?  (car (car lst)) key) (append (cdr lst) (list (cons key val)) rest)) ;if the value exists, override it.
            (else (addValue (cdr lst) key val (append rest (list (car lst))  )  ))) ; continue recursively
      )

    (define (find lst key) ;returns the value for a given key.

      (cond ((null? lst) '()) ; if the dictionary is empty, no value exists.
            ((string=? (car (car lst)) key) (cdr (car lst))  ) ; if found, return the value
            (else (find (cdr lst) key))      ;continue recursively
       )
      )

    (define (dic_funcs e) ; this function offers the different "methods" available in the dictionary
      (cond ((null? e) ls) ; empty input -> print the dictionary
      ((pair? e) (dictionary (addValue ls (car e) (cdr e) '())  )) ;tuple input - add the tuple to the dictionary
      (else (find ls e)) ; otherwise, received a key - return its value
      )
    )
    
    dic_funcs ; the dictionary returns a reference for the dic_funcs function
    
    )
  (dictionary '()) ;create an empty dictionary
  )


