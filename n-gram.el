(defun flatten2(l)
  (if l (append (car l) (flatten2 (cdr l))) nil))

(defun print-hash (hash)
  (princ "{")
  (maphash '(lambda (key value)
              (pp key)
              (princ "=>")
              (if (hash-table-p value) (print-hash value) (pp value))
              (princ ",")) hash)
  (princ "}")
  nil
  )

(defun traverse-tree (tree function &optional prefix)
  (let ((hoge
         (mapcar
          (lambda (x)
            (let ((pre (vconcat prefix (vector (car x)))))
              ;;(char-to-string (car x)))))
              (if (keymapp (cdr x))
                  (traverse-tree (cdr x) function pre)
                (car (cdr x)))
              ))
          (cdr tree))))
    (apply function prefix hoge)))

(defun traverse-tree-node (tree function &optional prefix)
  ;;(if (keymapp tree)
  (let ((hoge
         (mapcar
          (lambda (x)
            (let ((pre (vconcat prefix (vector (car x)))))
              ;;(char-to-string (car x))))
              (if (keymapp (cdr x))
                  (flatten2 (traverse-tree-node (cdr x) function pre))
                (list (funcall function prefix (cons (car x) (cadr x))))
                )))
          (cdr tree))))
    (if prefix hoge (apply 'append hoge))))

(defun my-lookup-key (keymap key)
  (let ((found (lookup-key keymap key)))
    (if (numberp found) nil (car found))
    ))

(defun my-define-key (keymap key def)
  (define-key keymap key (list def))
  )

(defun n-gram-internal (list n tree)
  (dotimes (i (1+ (- (length list) n)))
    (let* ((sub (substring (vconcat list) i (+ i n)))
           (count (or (my-lookup-key tree sub) 0)))
      (my-define-key tree sub (1+ count))))
  tree)

;;(next-property-change)

(defun split-word ()
  (let ((list))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((pre (point)))
          (skip-syntax-forward (char-to-string (char-syntax (char-after))))
          (cond ((and (eq ?- (char-after))
                      (eq ?_ (char-syntax (char-after))))
                 (forward-symbol 1)))
          (push
           (if (eq ?  (char-syntax (char-after pre)))
               (intern " ")
             (intern (buffer-substring-no-properties pre (point))))
           list)))
      (nreverse list))))

(defun my-pp-to-string (string)
  (let((string1 (pp-to-string string)))
    (substring string1 0 (1- (length string1)))
  ))

(defun n-gram (n)
  (interactive "nInput n of n-gram: ")
  (let ((tree (make-sparse-keymap))
        (my-hash (make-hash-table :test 'equal))
        (my-list nil)
        (max-lisp-eval-depth 1000))
    ;;treem
    (n-gram-internal (split-word) n tree)
    (traverse-tree
     tree
     (lambda (prefix &rest list)
       (let ((ret (apply '+ list)))
         (puthash prefix ret my-hash) ret)))
    ;;(print-hash my-hash)
    (setq my-list
          (traverse-tree-node
           tree
           (lambda (prefix x)
             (list
              (my-pp-to-string
               (vconcat
                (mapcar (lambda (x)
                          (substring-no-properties(symbol-name x )))
                        prefix)));;0 pre
              (pp-to-string (symbol-name (car x)))
              (if (eq 0 (cdr x)) 0
                (/ (* (cdr x) 100) (gethash prefix my-hash))) ;;2 %
              (cdr x);; 3 n/
              (gethash prefix my-hash)))));;/n
    (setq my-list
          (delete-if (lambda (x)
                       (or (< (nth 3 x) 3);;count
                           ;;(eq ?w (char-syntax (nth 1 x)))
                           )) my-list);;count
          );;filter
    (setq my-list
          (sort my-list
                (lambda (x y)(< (nth 3 x) (nth 3 y)))));;%
    (setq my-list
          (sort my-list
                (lambda (x y)(< (nth 2 x) (nth 2 y)))));;%
    (mapcar (lambda (x)
              (apply 'message "%s->%s %3.1f%% %d/%d" x))
            my-list)
    ;;my-list
    )
  )
;;(n-gram 3)