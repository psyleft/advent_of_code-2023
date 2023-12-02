(use-modules
 (ice-9 textual-ports)
)

(define (solve input-file)
  (let solve* ((port (open-input-file input-file))
               (acc '()))
    (define (solve-line line)
      (let ((first (string-index line char-set:digit))
            (last (string-rindex line char-set:digit)))
        (string->number
         (string (string-ref line first)
                 (string-ref line last)))))
    (let ((line (get-line port)))
      (if (eof-object? line)
          (apply + acc)
          (solve* port
                  (cons (solve-line line) acc))))))

(display (solve "./resources/input.txt"))
