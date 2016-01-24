(ql:quickload 'lispbuilder-sdl)

;;ALGO
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

(defun life_rule4 (list_live i j zoom)
  (if (< j zoom)
	(if (< i zoom)
	  (if (find (list i j) list_live :test #'equal)
		(life_rule4 list_live (+ i 1) j zoom)
		(let ((nbr (elements_in_commun list_live (give_neighbors i j))))
		  (if (= nbr 3)
			(cons (list i j) (life_rule4 list_live (+ i 1) j zoom))
			(life_rule4 list_live (+ i 1) j zoom))))
	  (life_rule4 list_live 0 (+ j 1) zoom))))

(defun life_rule123 (list_live list_live_rec)
  (if list_live_rec
	(let ((nbr (elements_in_commun list_live (give_neighbors (car (car list_live_rec)) (second (car list_live_rec))))))
	  (if (> nbr 1)
		(if (< nbr 4)
		  (cons (car list_live_rec) (life_rule123 list_live (cdr list_live_rec)))
		  (life_rule123 list_live (cdr list_live_rec)))
		(life_rule123 list_live (cdr list_live_rec))))))

(defun do_life (list_live zoom)
  (append (life_rule4 list_live 0 0 zoom) (life_rule123 list_live list_live)))
;;ALGO

(defun print_grid (list_live width height zoom)
  (let ((i 0) (w (/ width zoom)) (l (/ height zoom)))
	(loop while (< i zoom) do
		  (let ((j 0))
			(loop while (< j zoom) do
				  (sdl:draw-box (sdl:rectangle-from-edges-* (* i w) (* j l) (* (+ i 1) w) (* (+ j 1) l)) :color sdl:*black*)
				  (sdl:draw-box (sdl:rectangle-from-edges-* (+ (* i w) 1) (+ (* j l) 1) (- (* (+ i 1) w) 2) (- (* (+ j 1) l) 2))
								:color (if (find (list i j) list_live :test #'equal) sdl:*red* sdl:*white*))
				  (setq j (+ j 1)))
			(setq i (+ i 1))))))

(defun game_set (width height)
  (let ((play-game 0) (list_live (list)) (zoom 10) (sspeed 5) (compt 20) (lshift 0))
	(sdl:with-init ()
				   (sdl:window width height)
				   (setf (sdl:frame-rate) 120)
				   (sdl:with-events ()
									(:quit-event () t)
									(:key-down-event (:key key)
													 (case key (:sdl-key-escape (progn (sdl:push-quit-event))))
													 (case key (:sdl-key-p
																 (if (= play-game 1)
																   (setq play-game 0)
																   (setq play-game 1))))
													 (case key (:sdl-key-equals
																 (setq zoom (- zoom 1))))
													 (case key (:sdl-key-minus
																 (setq zoom (+ zoom 1))))
													 (case key (:sdl-key-r 
																 (setf list_live (list))
																 (setq play-game 0)))
													 (case key (:sdl-key-period
																 (if (/= sspeed 1)
																   (setq sspeed (- sspeed 1)))))
													 (case key (:sdl-key-comma
																 (setq sspeed (+ sspeed 1))))
													 (case key (:sdl-key-lshift
																 (setq lshift 1))))
									(:key-up-event (:key key2)
												   (case key2 (:sdl-key-lshift
																(setq lshift 0))))
									(:mouse-button-down-event (:button button :x x :y y)
															  (if (= button 4)
																(if (= lshift 1)
																  (if (/= sspeed 1)
																	(setq sspeed (- sspeed 1)))
																  (setq zoom (+ zoom 1))))
															  (if (= button 5)
																(if (= lshift 1)
																  (setq sspeed (+ sspeed 1))
																  (setq zoom (- zoom 1))))
															  (when (sdl:mouse-left-p)
																(let ((i (floor (/ x (/ width zoom)))) (j (floor (/ y (/ height zoom)))))
																  (if (find (list i j) list_live :test #'equal)
																	(if (equal (car list_live) (list i j))
																	  (setf list_live (cdr list_live))
																	  (setf list_live (delete (list i j) list_live :test #'equal)))
																	(setf list_live (append list_live (list (list i j))))))))
									(:idle ()
										   (if (> compt sspeed)
											 (progn (print_grid list_live width height zoom)
													(if (= play-game 1)
													  (setf list_live (do_life list_live zoom)))
													(sdl:update-display)
													(setq compt 0))
											 (setq compt (+ 1 compt))))))))

(defun usage ()
  (format t "usage: sbcl --load game_of_life.lsp [-h] width height~%~%")
  (format t "positional arguments:~%")
  (format t "  width			width of the grid~%~%")
  (format t "  height		height of the grid~%~%")
  (format t "optional arguments:~%")
  (format t "  -h, --help		show this help message and exit")
  )

(defun main (argl)
  (if (/= (length argl) 3)
	(usage)
	(game_set (if (< (parse-integer (nth 1 argl)) 100)
				100
				(parse-integer (nth 1 argl)))
			  (if (< (parse-integer (nth 2 argl)) 100)
				100
				(parse-integer(nth 2 argl)))))
  (exit))

(sb-int:with-float-traps-masked (:invalid :inexact :overflow) (main *posix-argv*))
