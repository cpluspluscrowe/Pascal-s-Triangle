                                        ; nth row and rth column, index starts at 0
                                        ; pascal's triangle value = n!/(r! *(n - r)!)

(defun factorial (n) 
  (cond ((< n 2) 1) 
        (t (* n (factorial (- n 1))))))

(defun pascal-triangle-value (n r) 
  (/ (factorial n) 
     (* (factorial r) 
        (factorial (- n r)))))

(defun print-level-helper (level r) 
  "print value r and go to next value"
  (cond ((eq r (+ level 1)) nil)
                                        ; logic for printing the level
        (t (progn (let ((triangle-value (pascal-triangle-value level r))) 
                    (princ triangle-value) 
                    (princ " ") 
                    (print-level-helper level (+ r 1)))))))


(defun print-level (level) 
  "prints out level + 1 items that represents the level for pascal's triangles"
  (print-level-helper level 0) 
  (princ "\n"))

(defun print-n-levels-helper (n current-level) 
  (cond ((eq n current-level) t) 
        (t (progn (print-level current-level) 
                  (print-n-levels-helper n (+ current-level 1))))))

(defun print-n-levels (n) 
  "print out each of the first n levels in pascal's triangle"
  (print-n-levels-helper n 0))
