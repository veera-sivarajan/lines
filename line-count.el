;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.
(defun get-selected-text (start end)
  (let ((regionp (buffer-substring-no-properties start end)))
    (split-string regionp "\n"))) 

(defun my_filter (seq pred)
  (cond ((null seq) '())
        ((funcall pred (car seq)) (cons (car seq)
                                        (my_filter (cdr seq) pred)))
        (t (my_filter (cdr seq) pred)))) 

(defun isNotEmptyStr (input_str)
  (not (string= input_str ""))) 

(defun line-count (start end)
  (interactive "r")
  (if (use-region-p)
      (let* ((text (get-selected-text start end))
             (total-len (length text))
             (filter-len (length (my_filter text 'isNotEmptyStr)))
             (empty-len (- total-len filter-len)))
        (message "Lines: %d, Empty: %d" filter-len (- empty-len 1)))
    (message "No lines selected"))) 
