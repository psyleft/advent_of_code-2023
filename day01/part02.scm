(use-modules
 (srfi srfi-1)
 (ice-9 textual-ports)
)

(define (process-string str)
  (define (modify-prefix str)
    (fold
     (lambda (num word str)
       (if (string-prefix? word str)
           (string-append (number->string num)
                          (substring str 1))
           str))
     str
     (iota 9 1)
     '("one" "two" "three" "four" "five"
       "six" "seven" "eight" "nine")))
  (let process-string* ((str str)
                        (index 0))
    (if (>= index (string-length str))
        str
        (let ((head (substring str 0 index))
              (tail (substring str index)))
          (process-string* (string-append head (modify-prefix tail))
                           (1+ index))))))

(define (solve input-file)
  (let solve* ((port (open-input-file input-file))
               (acc '()))
    (define (solve-line line)
      (let* ((str (process-string line))
             (first (string-index str char-set:digit))
             (last (string-rindex str char-set:digit)))
        (string->number
         (string (string-ref str first)
                 (string-ref str last)))))
    (let ((line (get-line port)))
      (if (eof-object? line)
          (begin
            (close-port port)
            (apply + acc))
          (solve* port
                  (cons (solve-line line) acc))))))

(display (solve "./resources/input.txt"))
