(in-package #:ninety-nine)

(defvar *testing-context* nil)

(defmacro testing (message &body body)
  `(let ((*testing-context* (concatenate 'list *testing-context* (list ,message))))
     ,@body))

(defun print-testing-context ()
  (labels ((print* (context)
             (when context
               (println (car context))
               (print* (cdr context)))))
    (print* *testing-context*)))

(defmacro assert-equal (expected actual)
  (let ((expected-symb (gensym "expected"))
        (actual-symb (gensym "actual"))
        (pass-symb (gensym "result")))
    `(let* ((,expected-symb ,expected)
            (,actual-symb ,actual)
            (,pass-symb (equal ,expected-symb ,actual-symb)))
       (if ,pass-symb
           (format t "OK~%")
           (progn
             (format t "FAIL~%")
             (print-testing-context)
             (println ',actual)
             (format t "Expected: ~a~%" ,expected-symb)
             (format t "Actual: ~a~%" ,actual-symb)
             (format t "EQUAL => ~a~%~%" ,pass-symb)))
       ,pass-symb)))

(defun my-last (list)
  "1. Find the last box of a list."
  (if-let ((cdr (cdr list)))
      (my-last cdr)
      list))

(defun my-last-test ()
  (assert-equal '(d)
                (my-last '(a b c d))))

(defun my-butlast (list)
  "2. Find the last but one box of a list."
  (if-let ((cddr (cddr list)))
    (my-butlast cddr)
    list))

(defun my-butlast-test ()
  (assert-equal '(c d)
                (my-butlast '(a b c d))))

(defun element-at (list n)
  "3. Find the K'th element of a list."
  (cond
    ((= n 1) (car list))
    ((> n 1) (element-at (cdr list) (1- n)))))

(defun element-at-test ()
  (assert-equal 'c
                (element-at '(a b c d e) 3)))

(defun list-size (list)
  "4. Find the number of elements of a list."
  (labels ((size* (list acc)
           (if list
             (size* (cdr list) (1+ acc))
             acc)))
    (size* list 0)))

(defun list-size-test ()
  (assert-equal 5
                (list-size '(a b c d e)))
  (assert-equal 0 (list-size nil))
  (assert-equal 1 (list-size '(nil)))
  (assert-equal 2 (list-size '(nil nil))))


(defun my-reverse (list)
  "5. Reverse a list."
  (labels ((reverse* (list new-list)
             (if (null list)
                 new-list
                 (reverse* (cdr list) (cons (car list) new-list)))))
    (reverse* list nil)))

(defun my-reverse-test ()
  (assert-equal nil
                (my-reverse nil))
  (assert-equal '(a)
                (my-reverse '(a)))
  (assert-equal '(a b)
                (my-reverse '(b a)))
  (assert-equal '(a b c)
                (my-reverse '(c b a)))
  (assert-equal '(a b c d)
                (my-reverse '(d c b a)))
  (testing "Should be non-destructive"
    (let ((list '(a b c d)))
      (assert-equal '(d c b a)
                    (my-reverse list))
      (testing "Check original list"
        (assert-equal '(a b c d)
                      list)))))

(defun palindrome-p (list)
  "6. Find out whether a list is a palindrome."
  (labels ((palindrome-p* (list reversed n)
             (or (zerop n)
                 (and
                  (equal (car list) (car reversed))
                  (palindrome-p* (cdr list) (cdr reversed) (1- n))))))
    (palindrome-p* list (reverse list) (floor (/ (length list) 2)))))

(defun palindrome-p-test ()
  (assert-equal t
                (palindrome-p '()))
  (assert-equal t
                (palindrome-p '(a)))
  (assert-equal t
                (palindrome-p '(a b b a)))
  (assert-equal t
                (palindrome-p '(a b c b a)))
  (assert-equal t
                (palindrome-p '(a b c c b a)))
  (assert-equal nil
                (palindrome-p '(a b c a)))
  (assert-equal nil
                (palindrome-p '(a b b b)))
  (assert-equal nil
                (palindrome-p '(a b c)))
  (assert-equal nil
                (palindrome-p '(a b))))

(defun my-flatten (list)
  "P07 (**) Flatten a nested list structure.

  Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its
  elements (recursively).

  Hint: Use the predefined functions list and append."
  (labels ((flatten* (list acc)
             (if (null list)
                 acc
                 (destructuring-bind (x . more) list
                   (flatten* more (if (listp x)
                                      (flatten* x acc)
                                      (append acc (list x))))))))
    (flatten* list nil)))

(defun my-flatten-test ()
  (assert-equal nil
                (my-flatten nil))
  (assert-equal '(a)
                (my-flatten '(a)))
  (assert-equal '(a b c d e)
                (my-flatten '(a (b (c d) e)))))

(defun compress (list)
  "P08 (**) Eliminate consecutive duplicates of list elements.

  If a list contains repeated elements they should be replaced with a single copy of the element. The order of the
  elements should not be changed."
  (labels ((compress* (list acc last-seen)
             (if (null list)
                 acc
                 (destructuring-bind (x . more) list
                   (if (equal x last-seen)
                       (compress* more acc last-seen)
                       (compress* more (append acc (list x)) x))))))
    (compress* list nil nil)))

(defun compress-test ()
  (assert-equal '(A B C A D E)
                (compress '(a a a a b c c a a d e e e e))))

(defun pack (list)
  "P09 (**) Pack consecutive duplicates of list elements into sublists.

  If a list contains repeated elements they should be placed in separate sublists."
  (labels ((append-group (acc group)
             (if group
                 (append acc (list group))
                 acc))
           (pack (list acc current-group last-seen)
             (if (null list)
                 (append-group acc current-group)
                 (destructuring-bind (x . more) list
                   (if (equal x last-seen)
                       (pack more acc (cons x current-group) last-seen)
                       ;; time to start a new group -- element is different from last
                       (pack more
                             (append-group acc current-group)
                             (list x)
                             x))))))
    (pack list nil nil nil)))

(defun pack-test ()
  (assert-equal '((A A A A) (B) (C C) (A A) (D) (E E E E))
                (pack '(a a a a b c c a a d e e e e))))

(defun encode (list)
  "P10 (*) Run-length encoding of a list.

  Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive
  duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E."
  (let ((packed (pack list)))
    (loop for x in packed collect (list (length x) (car x)))))

(defun encode-test ()
  (assert-equal '((4 A) (1 B) (2 C) (2 A) (1 D) (4 E))
                (encode '(a a a a b c c a a d e e e e))))

(defun encode-modified (list)
  "P11 (*) Modified run-length encoding.

  Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the
  result list. Only elements with duplicates are transferred as (N E) lists."
  (let ((encoded (encode list)))
    (loop for (len x) in encoded collect (if (= len 1)
                                             x
                                             (list len x)))))

(defun encode-modified-test ()
  (assert-equal '((4 A) B (2 C) (2 A) D (4 E))
                (encode-modified '(a a a a b c c a a d e e e e))))

(defun decode-modified ()
  "P12 (**) Decode a run-length encoded list.

  Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.")
