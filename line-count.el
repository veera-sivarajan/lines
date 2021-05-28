(defun text-from-region (start end)
  "Return a list of lines from selected region"
  (let ((regionp (buffer-substring-no-properties start end)))
    (split-string regionp "\n"))) 

(defun my_filter (seq pred)
  (cond ((null seq) '())
        ((funcall pred (car seq)) (cons (car seq)
                                        (my_filter (cdr seq) pred)))
        (t (my_filter (cdr seq) pred)))) 

(defun isNotEmptyStr (input_str)
  "Check if input_str is not empty"
  (not (string= input_str ""))) 

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n")))

(defun print-result (text)
  (let* ((total-len (length text))
         (filter-len (length (my_filter text 'isNotEmptyStr)))
         (empty-len (- total-len filter-len)))
    (message "Lines: %d, Empty: %d" filter-len (- empty-len 1)))) 

(defun line-count (start end)
  "Display the number of non empty lines and empty lines"
  (interactive "r")
  (save-buffer)
  (if (use-region-p)
      (print-result (text-from-region start end))
    (print-result (read-lines (buffer-file-name (current-buffer)))))) 
