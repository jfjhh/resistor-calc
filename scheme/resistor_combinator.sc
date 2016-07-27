;
; Calculates ways to obtain resistor values.
; Alex Striff
;
; This program is licensed under the MIT License,
; following the spirit of MIT Scheme.
;

; The target resistor value.
(define target 1337)

; The usable set of resistor values.
(define (next_value value)
  (define multiplier 10)
  (* value multiplier))
(define base_values (list 1.0 2.2 4.7))
(define sets 5)
(define (repmap base func count)
  (cond ((< count 1)
   (error "Cannot repeat an operation " count " times!"))
  ((= count 1)
   base)
  (else
    (append base (repmap (map func base) func (- count 1))))))
(define base_rvalues
  (append (repmap (map next_value base_values) next_value sets)
          (cons 1000000 '())))

; Ways to combine resistors.
(define (series a b)
  (+ a b))
(define (parallel a b)
  (/ 1.0 (+ (/ 1.0 a) (/ 1.0 b))))

; 
; Proc functor
;

; Returns the underlying function
(define (proc_return proc)
  (car proc))

; Returns the proc name
(define (proc_name proc)
  (cdr proc))

; Applies func to the underlying function, returning the modified functor.
(define (proc_bind func proc)
  (cons (func (proc_return proc)) (proc_name proc)))

; Turns a function and a name into a procedure functor that
; contains the function and the name, promoting the base function to a
; higher-order function that will generate a value: i.e. the primitive value,
; the arguments, and the procedure name.
; i.e. func name => (func name), where (func is "fmap'd" to produce values).
(define (proc_new func name)
  (cons (lambda (m n)
    (list (func (car m) (car n))
          (cons m n)
          name))
        name))

; Resistor combination functions as (proc => value) functors.
(define resistorprocs
  (list (proc_new series "Series")
        (proc_new parallel "Parallel")))

; Create a list of "partially applied" procs, with the first argument being the
; value m
(define (partialprocs procs values)
  (define (pprocs m)
    (define (partbin proc x)
      (proc_bind (lambda (p)
                   (lambda (y) (p x y)))
                 proc))
    (map (lambda (proc) (partbin proc m)) procs))
  (collapse (map (lambda (l) (pprocs l)) values)))

; Apply the partially-applied proc to n, resulting in a value list
;(define (resolveprocs partials values)
(define (resolveprocs partials values)
  (define (resolvevalue n)
    (map (lambda (proc)
         	(car (proc_bind
                  (lambda (p) (p n)) proc)))
         partials))
  (collapse (map (lambda (n) (resolvevalue n)) values)))

; Generate all pairs of values in a list.
(define (permutations values)
  (define (pairs values)
    (define (pairs-iter head values)
      (if (null? values) values
            (cons (cons head (car values))
                        (pairs-iter head (cdr values)))))
    (pairs-iter (car values) (cdr values)))
    (if (null? values) values
          (append (pairs values) (permutations (cdr values)))))

;
; Value datatype
;

; Promotes an underlying number `n` to be a value
(define (value_unit n)
  (list n '() "Resistor"))
(define base_acc (value_unit 0))

; Applies a function to the underlying number of a value.
(define (value_bind f v)
	(list (f (value_return v)) (value_args v) (value_proc v)))

; Gets the underlying number value
(define (value_return v)
  (car v))

; Gets the arguments (other resistors or combinations) that created the value.
(define (value_args v)
  (car (cdr v)))

; Gets the name of the proc that created the value.
(define (value_proc v)
  (car (cdr (cdr v))))
 
; Checks if two values are equal
(define (value_equal? a b)
	(= (value_return a) (value_return b)))

; Prints a value.
; NOTE: Depending on the implementation of append, the list may need to be reversed.
(define (value_display v)
  (define empty_msg (list ""))
  (define (value_display-iter v depth msg)
    (if (null? v)
        (list "NULL")
        (let ((proc (value_proc v))
              (num (value_return v))
              (args (value_args v)))
          (append (list "[" depth "]: " num " <" proc ">\n")
                  (if (null? args)
                      empty_msg
                      (append (value_display-iter (car args) (+ depth 1) empty_msg)
                              (value_display-iter (cdr args) (+ depth 1) empty_msg)))
                  (if (null? msg)
                      "EOT"
                      msg)))))
  (map display (reverse (value_display-iter v 0 empty_msg)))
  (newline))

;
; List functions
;

; Applies a function to every value in a list.
;(define (map f xs)
;	(if (null? xs)
;	    '()
;	    (cons (f (car xs)) (map f (cdr xs)))))

; Collapses nested lists into one long list.
(define (collapse xs)
	(if (null? xs)
	    '()
	    (let ((head (car xs))
	                (tail (cdr xs)))
	               (append head (collapse tail)))))

; Removes elements that do not match a filter function from a list.
(define (filter f xs)
	(if (null? xs)
	    '()
	    (let ((head (car xs))
	          (tail (filter f (cdr xs))))
	      (if (f head)
	          (cons head tail)
	          tail))))

; Removes duplicate values (that match the binary function) from a list.
(define (uniq f values)
	(if (null? values)
	    '()
	    (let ((head (car values))
	          (tail (cdr values)))
	         (cons head
	               (uniq f (filter (lambda (x) (not (f x head))) tail))))))

; Finds the least element in a list, according to a function.
(define (least f xs)
	(define (bin f x y)
		(if (f x y)
		    x
		    y))
	(cond ((null? xs) 0)
	      ((= (length xs) 1) (car xs))
	      (else (bin f (car xs) (least f (cdr xs))))))

;
; Main combination list manipulation
;

; Make the list of combined resistors, where n is the number of resistors.
(define (combine_resistors n)
  (define (combine procs values n)
    (if (< n 2)
        values
        ;(let ((nextvalues (uniq value_equal? (resolveprocs (partialprocs procs values) values))))
        (let ((nextvalues (resolveprocs (partialprocs procs values) values)))
  		  (combine procs nextvalues (- n 1)))))
	(combine resistorprocs
	         (map value_unit base_rvalues)
	         n))

; Get the closest value.
(define (closest_resistor values)
	(let ((diffs
	      	(map (lambda (v) (value_bind (lambda (x) (abs (- target x))) v)) values)))
	      (least (lambda (m n)
	       	(< (value_return m) (value_return n)))
	       diffs)))

;==============================================================================;

(define resistor_space 3)
(map display (reverse (list "Searching with a space of " resistor_space " resistors...")))
(newline)
(value_display (closest_resistor (combine_resistors resistor_space)))

