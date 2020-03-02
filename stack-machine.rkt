#lang racket
;(require racket/trace)
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack '())
(define (make-stack) empty-stack)

(define (push element stack) (cons element stack))
(define (top stack) (if (empty? stack) '() (car stack)))
(define (pop stack) (if (empty? stack) '() (cdr stack)))

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (list stack co-varnames co-consts co-names co-code IC))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine) (cadr stack-machine))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine) (caddr stack-machine))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine) (cadddr stack-machine))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine) (cadr (reverse stack-machine)))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine) (car stack-machine))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine) (car (reverse stack-machine)))



(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define current car)
(define next cdr)

(define (get-symbol-index symbol)
  (cond
    ((equal? symbol 'STACK) 0)
    ((equal? symbol 'CO-VARNAMES) 1)
    ((equal? symbol 'CO-CONSTS) 2)
    ((equal? symbol 'CO-NAMES) 3)
    ((equal? symbol 'CO-CODE) 4)
    ((equal? symbol 'INSTRUCTION-COUNTER) 5)
    )
    )

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-stack-machine item symbol stack-machine)
  (list-set stack-machine (get-symbol-index symbol) item)
  ;(list (take stack-machine (get-symbol-index symbol)) item (cdr (drop stack-machine (get-symbol-index symbol))))
  )

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (update-stack-machine (cons value (car stack-machine)) 'STACK stack-machine))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (update-stack-machine (pop (car stack-machine)) 'STACK stack-machine))


;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.

(define (load-const stack-machine params)
  (push-exec-stack (hash-ref (get-consts stack-machine) params) stack-machine))

(define (load-fast stack-machine params)
  (push-exec-stack (hash-ref (get-varnames stack-machine) params) stack-machine))

(define (store-fast stack-machine params)
  (update-stack-machine (hash-set (get-varnames stack-machine) params (top (get-stack stack-machine))) 'CO-VARNAMES (pop-exec-stack stack-machine)))

(define (bin-modulo stack-machine)
  (update-stack-machine (push (modulo (top (get-stack (pop-exec-stack stack-machine))) (top (get-stack stack-machine)))
                              (get-stack (pop-exec-stack (pop-exec-stack stack-machine)))) 'STACK stack-machine))

(define (bin-add stack-machine)
  (update-stack-machine (push (+ (top (get-stack (pop-exec-stack stack-machine))) (top (get-stack stack-machine)))
                              (get-stack (pop-exec-stack (pop-exec-stack stack-machine)))) 'STACK stack-machine))

(define (bin-substract stack-machine)
  (update-stack-machine (push (- (top (get-stack (pop-exec-stack stack-machine))) (top (get-stack stack-machine)))
                              (get-stack (pop-exec-stack (pop-exec-stack stack-machine)))) 'STACK stack-machine))

(define (jmp-abs stack-machine params)
  (update-stack-machine (quotient params 2) 'INSTRUCTION-COUNTER stack-machine))

(define (compare-op stack-machine params)
  (update-stack-machine (push ((get-cmpop params) (top (get-stack (pop-exec-stack stack-machine))) (top (get-stack stack-machine)))
                              (get-stack (pop-exec-stack (pop-exec-stack stack-machine)))) 'STACK stack-machine))

(define (pop-jmp-true stack-machine params)
  (if (top (get-stack stack-machine)) (pop-exec-stack (jmp-abs stack-machine params))
      (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine))
      )
  )

(define (pop-jmp-false stack-machine params)
  (if (not (top (get-stack stack-machine))) (pop-exec-stack (jmp-abs stack-machine params))
      (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine))
      )
  )

(define (for-iter stack-machine delta params)
  (if (not (empty? params))
      (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                            (update-stack-machine (push (car params) (push (cdr params) (get-stack (pop-exec-stack stack-machine))))
                                                  'STACK stack-machine))
      (update-stack-machine (+ (get-IC stack-machine) (+ (quotient delta 2) 1)) 'INSTRUCTION-COUNTER
                            (update-stack-machine (get-stack (pop-exec-stack stack-machine)) 'STACK stack-machine))
      )
   )

(define (load-global stack-machine params)
  (push-exec-stack (hash-ref (get-names stack-machine) params) stack-machine))

(define (call-function stack-machine params)
  (update-stack-machine
   (push ((get-function (car (drop (get-stack stack-machine) params))) (take (get-stack stack-machine) params)) (pop (drop (get-stack stack-machine) params)))
   'STACK stack-machine)
   ) 




(define (run-stack-machine stack-machine)
  (cond
    ;; RETURN_VALUE
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'RETURN_VALUE)
     stack-machine)

    ;; POP_TOP
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'POP_TOP)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine))))

    ;; LOAD_CONST
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'LOAD_CONST)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                           (load-const stack-machine (cdr (list-ref (get-code stack-machine) (get-IC stack-machine)))))))

    ;; LOAD_FAST
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'LOAD_FAST)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                           (load-fast stack-machine (cdr (list-ref (get-code stack-machine) (get-IC stack-machine)))))))

    ;; STORE_FAST
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'STORE_FAST)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                           (store-fast stack-machine (cdr (list-ref (get-code stack-machine) (get-IC stack-machine)))))))

    ;; BINARY_MODULO
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'BINARY_MODULO)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                           ( bin-modulo stack-machine))))

    ;; BINARY_ADD
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'BINARY_ADD)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                           (bin-add stack-machine))))

    ;; BINARY_SUBTRACT
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'BINARY_SUBTRACT)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                           (bin-substract stack-machine))))

    ;; INPLACE_MODULO
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'INPLACE_MODULO)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                           ( bin-modulo stack-machine))))

    ;; INPLACE_ADD
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'INPLACE_ADD)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                           (bin-add stack-machine))))

    ;; INPLACE_SUBTRACT
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'INPLACE_SUBTRACT)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                           (bin-substract stack-machine))))

    ;; JUMP_ABSOLUTE
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'JUMP_ABSOLUTE)
     (run-stack-machine (jmp-abs stack-machine (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))))))

    ;; COMPARE_OP
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'COMPARE_OP)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                           (compare-op stack-machine (cdr (list-ref (get-code stack-machine) (get-IC stack-machine)))))))

    ;; POP_JUMP_IF_FALSE
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'POP_JUMP_IF_FALSE)
     (run-stack-machine (pop-jmp-false stack-machine (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))))))

    ;; POP_JUMP_IF_TRUE
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'POP_JUMP_IF_TRUE)
     (run-stack-machine (pop-jmp-true stack-machine (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))))))

    ;; GET_ITER
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'GET_ITER)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                           stack-machine)))

    ;; POP_BLOCK
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'POP_BLOCK)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                           stack-machine)))

    ;; SETUP_LOOP
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'SETUP_LOOP)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                           stack-machine)))

    ;; FOR_ITER
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'FOR_ITER)
     (run-stack-machine (for-iter stack-machine (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))) (top (get-stack stack-machine)))))

    ;; LOAD_GLOBAL
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'LOAD_GLOBAL)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                           (load-global stack-machine (cdr (list-ref (get-code stack-machine) (get-IC stack-machine)))))))

    ;; CALL_FUNCTION
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'CALL_FUNCTION)
     (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                           (call-function stack-machine (cdr (list-ref (get-code stack-machine) (get-IC stack-machine)))))))
      
  )
  )

;(trace run-stack-machine)