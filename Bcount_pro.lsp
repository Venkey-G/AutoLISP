(defun c:BCOUNT_pro ()
  (setq ss (ssget)
        i 0
        lst (list)
  )
  (repeat (sslength ss)
    (setq e (ssname ss i))
    (repeat (setq p (fix (vlax-curve-getEndParam e)))
      (setq p (1- p))
      (if (and (/= lst nil) (< (distance (vlax-curve-getPointAtParam e p) (car lst)) 0.01))
          (princ "\nOne duplicate vertex found")
          (setq lst (cons (vlax-curve-getPointAtParam e p) lst))
      )
	  )
    (setq ss1 (ssget "_CP" lst '((0 . "INSERT"))))
    (if (> (sslength ss) 1) (princ (strcat "\n\nSet - " (itoa (+ i 1)))))
    (bns_count1 ss1)
    (setq i (1+ i))
  )
  (textscr)
  (princ)
)
(defun bns_count1 ( ss / bna lst na e1 n a mx )
(setq mx 1)
(setq n 0)
(repeat (sslength ss)
(setq  na (ssname ss n)
       e1 (entget na)
      bna (cdr (assoc 2 e1))
       mx (max mx (strlen bna))
);setq
(if (not (assoc bna lst))
    (setq lst (cons (cons bna 1) lst))
    (setq   a (cdr (assoc bna lst))
            a (+ a 1)
          lst (subst (cons bna a) (assoc bna lst) lst)
    );setq
);if
(setq n (+ n 1));setq
);repeat
(if lst
    (progn
     (setq mx (+ mx 5));setq
     (princ (bns_count_format1 "Block" "Count" mx))
     (setq a "\n")
     (while (< (strlen a) (+ mx 7))
;;      (setq a (strcat a "-"))
      (setq a (acet-str-format "%1-" a))
     );while
     (princ a)
    );progn then print header
);if
(setq n 0)
(repeat (length lst)
(setq a (nth n lst));setq
 (princ (bns_count_format1 (car a) (itoa (cdr a)) mx))
(setq n (+ n 1));setq
);repeat
);defun bns_count
(defun bns_count_format1 ( a b mx / )
 (while (<= (strlen a) mx)
;;  (setq a (strcat a "."))
  (setq a (acet-str-format "%1." a))
 );while
;; (setq a (strcat "\n" a b))
 (setq a (acet-str-format "\n%1%2" a b))
);defun bns_count_format