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

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n"))) 

(defun other-line-count ()
  (interactive)
  (let* ((text (read-lines (buffer-file-name (current-buffer))))
         (total-len (length text))
         (filter-len (length (my_filter text 'isNotEmptyStr)))
         (empty-len (- total-len filter-len)))
    (message "Lines: %d, Empty: %d" filter-len (- empty-len 1)))) 
