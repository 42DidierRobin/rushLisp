(defun give_neighbors (x y)
	(if (zerop x)
		(if (zerop y)
			(list
			(list (+ x 1) y) (list x (+ y 1)) (list (+ x 1) (+ y 1)))
			(list
			(list x (- y 1)) (list x (+ y 1))
			(list (+ x 1) (- y 1)) (list (+ x 1) y) (list (+ x 1) (+ y 1))))
	(if (zerop y)
		(list
			(list (- x 1) y) (list (- x 1) (+ y 1))
			(list x (+ y 1))
			(list (+ x 1) y) (list (+ x 1) (+ y 1)))
		(list 
			(list (- x 1) (- y 1)) (list (- x 1) y) (list (- x 1) (+ y 1))
			(list x (- y 1)) (list x (+ y 1))
			(list (+ x 1) (- y 1)) (list (+ x 1) y) (list (+ x 1) (+ y 1))))))

(defun elements_in_commun (list1 list2)
	(if list1
		(if (find (car list1) list2 :test #'equal)
			(+ 1 (elements_in_commun (cdr list1) list2))
			(+ 0 (elements_in_commun (cdr list1) list2)))
	0))

(defun life_rule4 (list_live i j width height)
	(if (< j height)
		(if (< i width)
			(if (find '(i j) list_live :test #'equal)
				(life_rule4 list_live (+ i 1) j)
				(let ((nbr (elements_in_commun list_live (give_neighbors i j))))
				(if (= nbr 3)
					(cons (list i j) (life_rule4 list_live (+ i 1) j))
					(life_rule4 list_live (+ i 1) j))))
			(life_rule4 list_live 0 (+ j 1)))))

(defun life_rule123 (list_live list_live_rec)
	(if list_live_rec
		(let ((nbr (elements_in_commun list_live (give_neighbors (car (car list_live_rec)) (second (car list_live_rec))))))
			(if (> nbr 1)
				(if (< nbr 4)
					(cons (car list_live_rec) (life_rule123 list_live (cdr list_live_rec)))
				(life_rule123 list_live (cdr list_live_rec)))
			(life_rule123 list_live (cdr list_live_rec))))))

(defun do_life (list_live)
	(append (life_rule4 list_live 0 0) (life_rule123 list_live list_live)))
