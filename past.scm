#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

;define the symbol table and its function ----------------------------------------------
(define *symbol-table* (make-hash))
(define (symbol-put! key value) (hash-set! *symbol-table* key value))

(for-each
   (lambda (pair)(symbol-put! (car pair) (cadr pair)))
   `(
      (+ , +)
      (- , -)
      (* , *)
      (/ , /)
      (= , =)
      (<> ,(lambda (x y) (not (= x y))))
      (> , >)
      (< , <)
      (<= , <=)
      (>= , >=)
      (^ , expt)
      (exp , exp)
      (abs , abs)
      (sin , sin)
      (asin , asin)
      (cos , cos)
      (acos , acos)
      (tan , tan)
      (atan , atan)
      (log , log)
      (sqrt , sqrt)
      (round , round)
      (ceil , ceiling)
      (floor , floor)
      (pi 3.1415926)
      (e 2.7182818)
      (log10_2 0.3010299)
      (log10   ,(lambda (num) (/ (log num) (log 10.0))))
      ;(sqrt_2 1.4142135)-----------------------------------------------------------
      ;(div     ,(lambda (x y) (floor (/ x y))))
      ;(mod     ,(lambda (x y) (- x (* (div x y) y))))
      ;(quot    ,(lambda (x y) (truncate (/ x y))))
      ;(rem     ,(lambda (x y) (- x (* (quot x y) y))))
   )
)

; define the function table to hold all of the functions ----------------------------------
(define *function-table* (make-hash))

; label table to hold addresses of each line, one level up from statements --------------------------------------------
(define *label-table* (make-hash))

;--------------------------------------------------------------
; evaluate expression
; deal with the input content
; if input is number or string, just keep with them.
; if input is complex, divide them.
; otherwise, output error
(define (evaluate input)
   (cond
      ((number? input) (if (= input 0) 0.0 input))
      ((string? input) input)
      ((hash-has-key? *symbol-table* input) (hash-ref *symbol-table* input))
      ((list? input)
         (if (hash-has-key? *symbol-table* (car input))
            (let((key (hash-ref *symbol-table* (car input))))
               (cond
                  ((procedure? key) (apply key (map (lambda (element) (evaluate element)) (cdr input))))
                  ((vector? key) (vector-ref key (cadr input)))
                  ((number? key) key)
                  (else (die "symbol type is wrong"))))
            (die "no current type now \n" )))))

            
; implemet the print function in sbi file --------------
(define (sbi-print content)
   (map (lambda (element) (display (evaluate element))) content)(newline))

    
; implemet the dim function in sbi file ------------
(define (sbi-dim input)
   (set! input (car input))
   (let((newvector (make-vector (evaluate (cadr input)) (car input))))
      (symbol-put! (car input) (+ (evaluate (cadr input)) 1))))

; implement the let function in sbi file -----------------------
(define (sbi-let input)
   (symbol-put! (car input) (evaluate (cadr input))))





; a helper function for sbi-input
(define (sbi-input2 expression count)
    (if (null? expression) count
        (let ((in (read)))
            (if (eof-object? in) -1
                (begin
                    (symbol-put! (car expression) in)
                    (set! count (+ 1 count))
                    (sbi-input2 (cdr expression) count))))))

; INPUT statement
(define (sbi-input expression)
    (symbol-put! 'count 0)
    (if (null? (car expression))
        (symbol-put! 'count -1)
        (begin
        (symbol-put! 'count (sbi-input2 expression 0)))))

; execute a line passed by function parse-line
(define (execute-line instruction program line-number)
    ; die if the key is invalid
    (when (not (hash-has-key? *function-table* (car instruction)))
        (die "~s is a invalid instruction." (car instruction)))
    (cond
        ((eq? (car instruction) 'goto)
         (parse-line program (hash-ref *label-table*
            (cadr instruction))))
        ((eq? (car instruction) 'if)
         (if (evaluate (car (cdr instruction)))
             (parse-line program (hash-ref *label-table*
                (cadr (cdr instruction))))
             (parse-line program (+ line-number 1))))
        ((eq? (car instruction) 'print)
         (if (null? (cdr instruction))
             (newline)
             (sbi-print (cdr instruction)))
             (parse-line program (+ line-number 1)))
        (else
             ((hash-ref *function-table* (car instruction))
              (cdr instruction))
             (parse-line program (+ line-number 1)))))

; parse a line
(define (parse-line program line-number)
    (when (> (length program) line-number)
        (let((line (list-ref program line-number)))
        
        (cond
            ((= (length line) 3)
             (set! line (cddr line))
             (execute-line (car line) program line-number))
            ((and (= (length line) 2) (list? (cadr line)))
             (set! line (cdr line))
             (execute-line (car line) program line-number))
            (else
              (parse-line program (+ line-number 1)))))))

; get the length of a list
(define length
    (lambda (l) (if (null? l) 0 (+ (length (cdr l)) 1))))

; store labels into hash table
(define (get-labels program)
    (map (lambda (l)
         (when (not (null? l))
            (when (or (= 3 (length l))
                (and (= 2 (length l))
                    (not (list? (cadr l)))))
                    (hash-set! *label-table* (cadr l)
                               (- (car l) 1 ))
                 ))) program))

; main function
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
                (program (readlist-from-inputfile sbprogfile)))
              ; store labels into hash table
              (get-labels program)

              ; execute program
              (parse-line program 0))))

(for-each
    (lambda (pair) (hash-set! *function-table* (car pair) (cadr pair)))
    `(
        (print ,sbi-print)
        (input ,sbi-input)
        (dim   ,sbi-dim)
        (let   ,sbi-let)
        (if    (void))
        (goto  (void))))

(main (vector->list (current-command-line-arguments)))

