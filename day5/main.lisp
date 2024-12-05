(defun get-numbers-from-line (line)
  (let ((*read-eval* nil))
    (with-input-from-string (in line)
      (loop for number = (read in nil)
            while number collect number))))

(defun get-numbers-from-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line collect (get-numbers-from-line line))))

(defun get-orders-from-file (filename)
  (setq orders (make-hash-table))
  (loop for (page before) in (get-numbers-from-file filename)
        do (push before (gethash page orders))
        finally (return orders)))

(defun get-valid-page (pages orders)
  (let (before invalid)
    (loop for page in pages
          do (setf invalid (append (intersection before (gethash page orders)) invalid))
             (push page before)
             (if invalid (return-from get-valid-page 0))))
  (nth (floor (/ (length pages) 2)) pages))

(defun sort-page (pages orders)
  (sort pages #'(lambda (p1 p2) (find p2 (gethash p1 orders)))))

(defvar pages (get-numbers-from-file "pages.txt"))
(defvar orders (get-orders-from-file "orders.txt"))

(defvar part1 (reduce #'+ (mapcar #'(lambda (page) (get-valid-page page orders)) pages)))
(format t "Part 1: ~W~%" part1)
(defvar part2 (reduce #'+ (mapcar #'(lambda (page) (get-valid-page page orders)) (mapcar #'(lambda (page) (sort-page page orders)) pages))))
(format t "Part 2: ~W~%" (- part2 part1))