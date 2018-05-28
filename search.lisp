#|

Name:        Alex Elizalde
Class:	     CSCE 3210
Instructor:  Dr. Swigger
Program:     Search Program

General Description:
The program uses a coordinate system that's similar to how math functions are graphed 
(x, y). Positive x values correspond to the character going "East" and positive y values correspond
to the character going "North". The grid will be 7 x 7, The range of the grid will be
from (0 0) to (9 9).

|#


;  initialize the gold, hole, and water squares
(defun initialize ()
  (setf gold  '((5 3)) )
  (setf hole  '((2 3) (1 3) (0 3) (4 0 ) (4 1) (4 2)))
  (setf water '((0 0) (1 0) (2 0)) )
  (setf places-already-visited nil )

  (setf which-search-to-use nil)
  (setf start-state nil)
  (setf total-cost 0)
  (setf nodes-expanded 0)
  (setf final-list-of-nodes nil)
	
	
  (princ "Input breadth-first or cost-first -->>")
  (setf which-search-to-use (read))
  (princ "Input a start state eg. (3 5) -->>")
  (setf start-state (read))
	
  (cond ((equal which-search-to-use 'breadth-first)
	 (breadth-first start-state)))

  (format t "The total cost is ->~A ~%" total-cost);(princ "The total cost is ->")
	;(write total-cost)
	;(princ "Total nodes expanded ->")
  (format t "Total nodes expanded ->~A ~%" nodes-expanded) 
	;(write nodes-expanded)
	;(write final-list-of-nodes))
  (format t "Final path ->~A ~%" final-list-of-nodes)) 
	
;calculate the cost by checking if the squares that have been visited are water tiles or not.
(defun calculate-cost (complete-list)

  (cond ((null complete-list) 0)
	((have-i-found-water (first complete-list) water)
            (+ 2 (calculate-cost (rest complete-list))))
         (t (+ 1 (calculate-cost (rest complete-list))))))				



;     the next two functions make it easy to get the first x and y value from the first list of lists
;   ex. ( (3 5) (6 7) (8 9)) ---  input into get-first-x-value function would return 3
;   and the same list input into the get-first-y-value function
;   will return 5. The example input is impossible (I forgot why) and is only like that
;   so that its easier to understand what the function does.
(defun get-first-x-value (	list-of-lists-of-coords)
	(first (first list-of-lists-of-coords)))
	
(defun get-first-y-value (	list-of-lists-of-coords)
	(second (first list-of-lists-of-coords)))
	
;The function remove-invalid-moves goes through the list of moves like for example ((0 1) (1 0) (-1 0) (0 -1))
;and removes the lists that contain invalid coordinates. The function would return ((0 1) (1 0)).
;The function also removes square that have a hole or have been visited.
(defun remove-invalid-moves (list-of-future-coordinates)

  (cond ((null list-of-future-coordinates) nil)

	((OR  (minusp (get-first-x-value list-of-future-coordinates))        ;test if either x or y value is negative
              (minusp (get-first-y-value list-of-future-coordinates))
              (> (get-first-x-value list-of-future-coordinates) 7)	     ;test if either x or y is greater than 7
	      (> (get-first-y-value list-of-future-coordinates) 7)
              (have-i-visited-this-square (first list-of-future-coordinates) places-already-visited)  ;test if any future square has been visited
	      (have-i-found-hole          (first list-of-future-coordinates) hole))		      ;test if there's a hole in a future square
	        (remove-invalid-moves (rest list-of-future-coordinates)))    ;calls the same function without passing the first thing in the list. Which is not valid
																						
	      (t							     ;else the coordinate at the beginning is valid and keep it, pass the rest of the list
	        (cons (first list-of-future-coordinates) (remove-invalid-moves (rest list-of-future-coordinates))))))     ;to the same function 
																										
	
	
	
;   sample input (generate-moves '(3 5) is used throughout the function
;   mc stands for main character
(defun generate-moves (coordinate-of-mc)

  ;make the variable to hold all the possible coordinates
  (let* (list-of-possible-coordinates)
   
  ;;generate the 4 possible coordinates
   
  ;     do (x + 1, y) and add it to the list-of-possible-coordinates
  ;	input of '(3 5)  will result in ((4 5))
  ;	x + 1	y
    (setq list-of-possible-coordinates
	  (cons (list (+ 1 (first coordinate-of-mc))  (second coordinate-of-mc))
		list-of-possible-coordinates)) 
   
   ;	do (x, y + 1)
   ;	add (3 6) to the list. The new list is ((3 6) (4 5))
   (setq list-of-possible-coordinates
	 (cons (list (first coordinate-of-mc) (+ 1 (second coordinate-of-mc)))
	       list-of-possible-coordinates)) 

   ;	do (x - 1, y)
   ;	add (2 5) to the list. The new list is ((2 5) (3 6) (4 5))
   (setq list-of-possible-coordinates
	 (cons (list (- (first coordinate-of-mc) 1) (second coordinate-of-mc))
	       list-of-possible-coordinates))
	
   ;	do (x , y - 1)
   ;	add (3 4) to the list. The new list is ((3 4) (2 5) (3 6) (4 5))
   (setq list-of-possible-coordinates
	 (cons (list (first coordinate-of-mc) (- (second coordinate-of-mc) 1))
	       list-of-possible-coordinates))
			
			
   ;    now input the list to the function remove-invalid-moves to get rid of the invalid moves
   (setq list-of-possible-coordinates
	 (remove-invalid-moves list-of-possible-coordinates))   ))
	
	
;the function compares two things
(defun match (element pattern)
   (equal element pattern))

#|
This function calls the generate-moves function
The generate-moves function then filters out coordinates that are out of bounds
, spots that have been visited, or holes. This is for the breadth first search.
|#
(defun expanpaths (path)
   (mapcar (lambda (nextpath) (cons nextpath path))   
      (generate-moves(first  path))))  


;The next three functions do the same thing and test if gold, a hole, or the square has been visited
;I made three functions to make it easier to read when they're called but I think just using one function
;with a more generic name would've been better.
(defun have-i-visited-this-square (current-square list-of-past-squares)
	(cond ((null list-of-past-squares)
				nil)
				
			((match current-square (first list-of-past-squares))
				t)
				
			(t (have-i-visited-this-square current-square (rest list-of-past-squares)))))

			
(defun have-i-found-gold (curr-square list-of-gold-squares)
	(cond ((null list-of-gold-squares) nil)
				
	      ((match curr-square (first list-of-gold-squares))
		      t)
				
	       (t (have-i-found-gold curr-square (rest list-of-gold-squares)))))			

(defun have-i-found-hole (current-location list-of-hole-squares)
	(cond ((null list-of-hole-squares) nil)
				
	      ((match current-location (first list-of-hole-squares))
		      t)
				
	       (t (have-i-found-hole current-location (rest list-of-hole-squares)))))

			 
(defun have-i-found-water (past-locations list-of-water-squares)
	(cond ((null list-of-water-squares) nil)
				
	      ((match past-locations (first list-of-water-squares))
		      t)
				
	       (t (have-i-found-water past-locations (rest list-of-water-squares)))))

			 

;The function that does the breadth-first search
(defun breadth-first (start)
   (let* ((paths   (list (list start)))	
          (current (first paths))
          (counter 0))
      (loop
         (setq current (first paths))
         (setq places-already-visited
		   (cons current   places-already-visited))
         
			
         (cond ((null paths) (return nil))
               ((have-i-found-gold   (first current)   gold)
                  (setf nodes-expanded counter)
						(setf total-cost (calculate-cost current))
						(setf final-list-of-nodes (reverse current))
						(return  (reverse current))))
						
	      (setq counter (+ counter 1))   
	      (setq paths
            (append (rest paths) (expanpaths current))))))
	
#|
(defun cost (paths)
   (let ((bestpath nil)
      (temppath 0))
      (setf bestpath (cons (apply '+ (mapcar 'rest (first paths))) (first paths)))
      (setf paths (rest paths))
         (loop
            (if (null paths) (return (rest bestpath)))
            (if (< (setf temppath (apply '+ (mapcar 'rest (first paths)))) (first bestpath))
                (setf bestpath (cons temppath (first paths))))
                (setf paths (rest paths)))))

					 

(defun cost-first (start)
   (let* ((current  nil) 
	       (paths nil)
	       (counter 0))
	
      (setq paths (list (list (cons start 0))))
      (loop
         ;(print 'paths) (princ paths)
         ;(setq current (cost  paths))  ; (cost paths)
         (cond
            ((null paths) (return nil))
            ((have-i-found-gold   (first (first current))   gold)
             (return (cons counter  (reverse current)) ))  ;added (cons x counter)
            (t (setq counter (+ counter 1))(setq paths (append (remove current paths :test #'equal) (expanpaths current))))))))   |#
				


