(require 'dash)

;; FLOW OPTIONS
(defvar defkey-flow-cycle nil)

;; UTILITIES
(defun defkey-group-keydef (keydef)
  "Group KEYDEF elements into alist of :keywrods, :lists, :vectors and :cycle."
  (let (kval)
    (--group-by (cond ((keywordp it) (setq kval :opts))
		      (kval (setq kval nil) :opts)
		      ((vectorp it) :interleave)
		      ((listp it) :forward)
		      (t :cycle))
		keydef)))

;; (-let [(&alist :cycle kcycle
;; 	       :forward kforward
;; 	       :interleave kinterleave
;; 	       :opts kopts)
;;        (defkey-group-keydef '("sdff" :a 1 :b 2 "sfa"
;; 			  ("=" "===")
;; 			  :c 3 lalala
;; 			  ("+" "%%%%")
;; 			  [("=" " = ") ("+" " + ")]
;; 			  [("-" " 0 ") ("+" " + ")]
;; 			  343))]
;;   kcycle)


(defvar defkey-last-undo-length nil)
(defvar defkey-last-undo-was-t nil)

(defun defkey-undo-boundary ()
  (setq defkey-last-undo-was-t (eq buffer-undo-list t))
  (when defkey-last-undo-was-t
    (setq buffer-undo-list nil))
  (undo-boundary)
  (setq defkey-last-undo-length (length buffer-undo-list)))

(defun defkey-undo-last-command ()
  (while (> (length buffer-undo-list) defkey-last-undo-length)
    (let ((n (length (--take-while it buffer-undo-list))))
      (setq buffer-undo-list (primitive-undo 1 buffer-undo-list))))
  (when defkey-last-undo-was-t
    (setq buffer-undo-list t)))

(defun defkey-command-name (prefix key)
  (let* ((prefix (or (and prefix
			  (symbolp prefix)
			  (symbol-name prefix))
		     prefix
		     (symbol-name major-mode)))
	 (first (not (string-match-p "^dk:" prefix))))
    (intern
     (concat (and first "dk:")
	     (->> prefix
		  (replace-regexp-in-string "-mode\\(-map\\)?$" ""))
	     (and first ":")
	     (->> key
		  (key-description)
		  (replace-regexp-in-string " +" ""))))))

(defun defkey-opt (name plist)
  (let ((global (intern (concat "defkey-flow-" name)))
	(kwd (intern (concat ":" name))))
    (if (plist-member plist kwd)
	 (plist-get plist kwd)
      (symbol-value global))))



;;; CORE
(defun defkey--make-forward-commands (kforward prefix)
  (--map (cons (vconcat (car it))
	       (defkey-make-command (car it) (cdr it) prefix))
	 kforward))

(defun defkey--make-interleave-commands (kinterleave-1 prefix)
  (let ((interleave-with
	 (--map (cons (car it)
		      (defkey-command-name prefix (car it)))
		kinterleave-1)))
    (--map (cons (vconcat (car it))
		 (defkey-make-command (car it) (cdr it) prefix interleave-with))
	   kinterleave-1)))

(defun defkey--make-command (command-name key kcycle kforward kinterleave kopts interleave-with)
  (let ((kcycle-remain (or kcycle '(key-combo-execute-original))))
    (fset command-name
	  (lambda ()
	    "sdfdsfds"
	    (interactive)
	    (let ((repeat (and (eq command-name real-last-command)
			       (> (length kcycle) 1)
			       (or kcycle-remain
				   (defkey-opt "cycle" kopts)))))
	      (unless kcycle-remain
		(setq kcycle-remain kcycle))
	      (if repeat
		  (defkey-undo-last-command)
		(setq kcycle-remain kcycle))
	      (defkey-undo-boundary)
	      (let ((cmd (pop kcycle-remain)))
		(key-combo-execute cmd)
		(let ((tmap (make-sparse-keymap)))
		  (define-key tmap key command-name)
		  (--each kforward
		    (define-key tmap (car it) (cdr it)))
		  (--each interleave-with
		    (define-key tmap (car it) (cdr it)))
		  (--each kinterleave
		    (--each it
		      (define-key tmap (car it) (cdr it))))
		  (set-transient-map tmap))))))))

(defun defkey-make-command (key def &optional prefix interleave-with)
  (-let* ((command-name (defkey-command-name prefix key))
	  (key (vconcat key))
	  ((&alist :cycle kcycle
		   :forward kforward
		   :interleave kinterleave
		   :opts kopts)
	   (defkey-group-keydef def))
	  (kinterleave (--map (defkey--make-interleave-commands it command-name)
			      kinterleave))
	  (kforward (defkey--make-forward-commands kforward command-name)))
    (defkey--make-command command-name key
			       kcycle kforward kinterleave
			       kopts interleave-with)
    command-name))

;; =3 +++ +++ ||| || bbb bbb aaa aaa 
;; (local-set-key "=" (defkey-make-command  "=" '("=1" "=2" "=3"
;; 					   :cycle t
;; 					   ("-" " -- " " --- ")
;; 					   ;; ("+" " ++ " :cycle t)
;; 					   [("+" " ++ " " +++ ")
;; 					    ("|" " || " " ||| ")]
;; 					   [("a" " aa " " aaa ")
;; 					    ("b" " bb " " bbb ")])))
