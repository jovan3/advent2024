(use-modules (srfi srfi-1)
             (ice-9 textual-ports)
             (ice-9 match)
             (ice-9 regex)
             (ice-9 receive)
             (ice-9 rdelim))

(define (input-lines file-path)
  (let ((file-string (call-with-input-file file-path get-string-all)))
    (string-split file-string #\newline)))

(define (input-lines-numbers input-lines-str)
  (map (lambda (line-str)
         (map string->number (string-split line-str #\ )))
         input-lines-str))

(define (adjecent-diffs lst)
  (drop
   (map (lambda (a) (- (first a) (second a)))
         (zip lst (cons 0 lst)))
   1))

(define (safe? lst)
  (let ((diffs (adjecent-diffs lst)))
    (or
     (every (lambda (el)
              (and (< -4 el) (> 0 el))) diffs)
     (every (lambda (el)
              (and (< 0 el) (> 4 el))) diffs))))

(display (string-append
          "day 2 part 1: "
          (number->string
           (let ((rows (input-lines-numbers (input-lines "inputs/day2"))))
            (length
             (filter safe? rows))))))
(newline)

(define (lst-variants lst)
  (cons lst
        (map (lambda (index)
               (receive (first last)
                   (split-at lst index)
                 (concatenate (list (drop-right first 1) last))))
             (iota (length lst) 1))))

(define (part2-safe? lst)
  (filter
   (lambda (item)
     (let ((variants (lst-variants item)))
       (any safe? variants)))
     lst))

(display (string-append
          "day 2 part 2: "
          (number->string
           (let ((rows (input-lines-numbers (input-lines "inputs/day2"))))
            (length
             (part2-safe? rows))))))
(newline)
