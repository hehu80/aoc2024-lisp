(defun get-values-from-line (line)
  (let ((*read-eval* nil))
    (with-input-from-string (in line)
      (loop for number = (read in nil)
            while number collect number))))

(defun get-values-from-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line collect (get-values-from-line line))))

(defvar lines (get-values-from-file "values.txt"))
(defvar first (sort (mapcar (lambda (pair) (nth 0 pair)) lines) #'>))
(defvar second (sort (mapcar (lambda (pair) (nth 1 pair)) lines) #'>))

(format t "Part 1: ~W~%" (reduce #'+ (mapcar #'(lambda (a b) (abs (- a b))) first second)))
(format t "Part 2: ~W~%" (reduce #'+ (mapcar #'(lambda (a) (* a (count a second))) first)))