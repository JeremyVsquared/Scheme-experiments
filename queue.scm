;;; Queue install package
(define (make-install-queue)
  ; core operations
  (define (front-ptr queue) (car queue))
  (define (rear-ptr queue) (cdr queue))
  (define (set-front-ptr! queue item) (set-car! queue item))
  (define (set-rear-ptr! queue item) (set-cdr! queue item))

  ; public operations
  (define (empty-queue? queue) (null? (front-ptr queue)))
  (define (make-queue) (cons '() '()))
  (define (front-queue queue)
    (if (empty-queue? queue)
        (error "FRONT called with an empty queue" queue)
        (car (front-ptr queue))))
  (define (insert-queue! queue item)
    (let ((new-pair (cons item '())))
      (cond ((empty-queue? queue)
             (set-front-ptr! queue new-pair)
             (set-rear-ptr! queue new-pair)
             queue)
            (else
             (set-cdr! (rear-ptr queue) new-pair)
             (set-rear-ptr! queue new-pair)
             queue))))
  (define (delete-queue! queue)
    (cond ((empty-queue? queue)
           (error "DELETE! called with an empty queue" queue))
          (else
           (set-front-ptr! queue (cdr (front-ptr queue)))
           queue)))
  (define (dispatch m)
    (cond ((eq? m 'empty?) empty-queue?)
          ((eq? m 'make) make-queue)
          ((eq? m 'front-queue) front-queue)
          ((eq? m 'insert-queue!) inser-queue!)
          ((eq? m 'delete-queue!) delete-queue!)
          (else (error "Unknow method -- QUEUE-DISPATCH" m))))
  dispatch)

;;;Associative table implementation
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

;;; Two dimensional associative table implementation
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

;;;Two dimensional associative local  table
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))