(use-modules (srfi srfi-1)
             (ice-9 textual-ports)
             (ice-9 match)
             (ice-9 regex)
             (ice-9 rdelim))

(define (input-lines file-path)
  (let ((file-string (call-with-input-file file-path get-string-all)))
    (string-split file-string #\newline)))

(define (parse-pairs input-lines)
  (let ((pairs-str (map
                    (lambda (line) (map match:substring (list-matches "[0-9]+" line)))
                            input-lines)))
    ;; convert the pair elements to numbers
    (map (lambda (pair)
           (map (lambda (item) (string->number item))
                pair))
         pairs-str)))

(define (read-input)
  (letrec ((lines (input-lines "inputs/day1"))
           (pairs (parse-pairs lines))
           (left-column (map first pairs))
           (right-column (map second pairs)))
    (list left-column right-column)))

(let ((sum-distances (match (read-input)
                       ((left-column right-column)
                        (letrec ((left-sorted (sort left-column <))
                                 (right-sorted (sort right-column <))
                                 (zip-pairs (zip left-sorted right-sorted))
                                 (distances (map (lambda (pair) (abs (- (first pair) (second pair))))
                                                 zip-pairs)))
                          (apply + distances))))))
  (display (string-append "day 1 part 1: " (number->string sum-distances)))
  (newline))

(define (count-occurences lst el)
  (length
   (filter (lambda (element)
             (= element el)) lst)))

(let ((part-2-sum (match (read-input)
                    ((left-column right-column)
                     (apply +
                            (map (lambda (el)
                                   (* el (count-occurences right-column el)))
                                 left-column))))))
  (display (string-append "day 1 part 2: "  (number->string part-2-sum)))
  (newline))
