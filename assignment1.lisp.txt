; CMPUT 325 Winter 2019 Assignment 1
; CCID nbombard Student name Nicholas Bombardieri
;
; Question 1 issorted
;
; issorted(L)
;	if null (cdr L) then T					#if the next element is nil, then list is sorted
;	else if (> (car L) (car (cdr L))) then NIL		#if the current front element is > next element, list is not sorted. return nil.
;	else issorted(cdr L)					#recursive call on list excluding first element
;
(defun issorted (L)
	(if (null (cdr L))
		T
		(if (>= (car L) (cadr L))
			NIL
			(issorted (cdr L))
		)
	)
)
;
;Question 2 numbers
;
; numbers(N)
;	if (>= 0 N) then NIL				#N is 0 or less
;	else (append (numbers(N - 1)) (cons N NIL))	#Adds N to the end of the return list. Decrements N and makes recursive call.
;
(defun numbers(N)
	(if (>= 0 N)
		NIL
		(append (numbers (- N 1)) (cons N NIL))
	)
)
;
;Question 3 palindrome
;#reverse the list using helper function defined as myreverse, then compare to the original list using equals function
;
;myreverse(L)							#reverses the order of a list
;	if (null L) then L					#if at end of list, stop recursing
;	else (append (myreverse (cdr L)) (cons (car L) NIL))	#add current first element to end of list, and recurse on remaining elements
;
(defun myreverse(L)
	(if (null L)
		L
		(append (myreverse (cdr L)) (cons (car L) NIL))
	)
)
;
;palindrome(L)
;	equal (L myreverse(L))					#sees if the list is identical to itself when reversed.	
;
(defun palindrome(L)
	(equal L (myreverse L))
)
;
;Question 4 replace1
;
;replace1(A B L)			#replaces all instances of A with B in given List L, ignoring sublists
;	if (null L) then L		#ceases recursive call at end of list
;	else 
;		(if (eq (car L) A) then (append B (replace1 A B (cdr L)) NIL)	#First element in L is A. replace with B, and recurse on (cdr L)
;		else (append (cons (car L) NIL) (replace1 A B (cdr L)) NIL)	#First element in L isnt A. add it to list, and recurse on (cdr L)
;
(defun replace1(A B L)
	(if (null L)
		L
		(if (eq (car L) A)
			(append (cons B NIL) (replace1 A B (cdr L)))
			(append (cons (car L) NIL) (replace1 A B (cdr L)))
		)
	)
)
;
;Question 4 replace2
;
;replace2(A B L)			#replaces all instances of A with B in given List L, including sublists
;	if (null L) then L		#ceases recursive call at end of list
;	else 
;		(if (atom (car L)		#if the first element of L is an atom, proceed as in replace1. Else, recurse on that list
;			(if (eq (car L) A) then (append B (replace2 A B (cdr L)) NIL)	#First element in L is A. replace with B, and recurse on (cdr L)
;			else (append (cons (car L) NIL) (replace2 A B (cdr L)) NIL)		#First element in L isnt A. add it to list, and recurse on (cdr L)
;		else (append (cons (replace2(A B (car L) NIL) (replace2 A B (cdr L)) NIL))	#recurses on first element of list, 
;																					#then proceeds accordingly
;
(defun replace2(A B L)
	(if (null L)
		L
		(if (atom (car L))
			(if (eq (car L) A)
				(append (cons B NIL) (replace2 A B (cdr L)))
				(append (cons (car L) NIL) (replace2 A B (cdr L)))
			)
			(append (cons (replace2 A B (car L)) NIL) (replace2 A B (cdr L)))
		)
	)
)
;
;Question 5 common
;
;common(A B)				#counts how many atoms in list A are also in list B. Atoms in a list never repeat.
;	if (null A) then 0		#A has no more elements, so no elements in common with B
;	else if (null (member (car A) B)) then (common (cdr A) B))	#returns true if car(A) is not in B, and recurses on cdr(A)
;	else 1 + (common (cdr A) B))					#if car(A) was in B, then adds 1 and recurses on cdr(A)
;
(defun common(A B)
	(if (null A)
		0
		(if (null (member (car A) B))
			(common (cdr A) B)
			(+ 1 (common (cdr A) B))
		)
	)
)
;
;Question 6 setcover
;
;This function solution should implement the following steps:
;	1) Transform N into a list U				
;	2) Repeat until U is empty:
;		a) Find the number of overlapping values between U and each subset of S, and put them in a list O	
;		b) Traverse O to find the index of the highest number.							
;		c) Locate the subset A in S with the most overlapping values with U. 					
;		d) remove A from S											
;		e) remove overlapping values between U and A from U							
;		f) Store A in SOL (solution)										
;		g) recurse on new U and S lists										
;	3) return SOL						
;
;
;FROM PREVIOUS QUESTIONS
;common(A B)														#counts how many atoms in list A are also in list B. Atoms in a list 
;																		#never repeat.
;	if (null A) then 0												#A has no more elements, so no elements in common with B
;	else if (null (member (car A) B)) then (common (cdr A) B))		#returns true if car(A) is not in B, and recurses on cdr(A)
;	else 1 + (common (cdr A) B))									#if car(A) was in B, then adds 1 and recurses on cdr(A)
;
;
;FROM PREVIOUS QUESTIONS
;numbers(N)
;	if (>= 0 N) then NIL							#N is 0 or less
;	else (append (numbers(N - 1)) (cons N NIL))		#Adds N to the end of the return list. Decrements N and makes recursive call.
;
;
;removeElement(E U)		#removes all instances of element E from list U 
;	if (null U) then U
;	else then (if (eq (car U) E) then (removeElement E (cdr U))
;	else (append (cons (car U) NIL) (removeElement E (cdr U)))
;
;
(defun removeElement(E U)
	(if (null U) 
		U
		(if (eq (car U) E)
			(removeElement E (cdr U))
			(append (cons (car U) NIL) (removeElement E (cdr U)))
		)	
	)
)
;
;
;removeoverlap(A U)		#removes all elements from U which also appear in A
;	if (null A) then U
;	else then (removeoverlap((cdr A) (removeElement (car A) U)))
;
;
(defun removeoverlap(A U)
	(if (null A)
		U
		(removeoverlap (cdr A) (removeElement (car A) U))
	)
)
;
;
;getind(ind S)			# returns the element of S at index ind
;	if (<= ind 0) then (car S)
;	else (getind((ind - 1) (cdr S)))
;
;
(defun getind(ind S)
	(if (<= ind 0)
		(car S)
		(getind (- ind 1) (cdr S))
	)
)
;
;
;getoverlap(S U)		#returns list of overlaps between each subset of S and U
;	if (null S) then S	#no more subsets of S left. stop recursing
;	else (append (cons (common (car S) U) NIL) (getoverlap((cdr S) U)))		#returns overlap of current element as well as recurses onto next element
;
;
(defun getoverlap(S U)
	(if (null S)
		S
		(append (cons (common (car S) U) NIL) (getoverlap (cdr S) U))
	)
)
;
;
;valind(val O)			#returns the index of the first instance of val in O
;	if (null O) then O
;	else if (eq (car O) val) then 0
;	else 1 + valind(val (cdr O))
;
;
(defun valind(val O)
	(if (null O)
		O
		(if (eq (car O) val)
			0
			(+ 1 (valind val (cdr O)))
		)
	)
)
;
;
;maxinlist(O)			#returns the value of the largest element in the list O
;	if (null O) then 0
;	let (best maxinlist(cdr O))
;	if (< best (car O)) then (car O)
;	else then best
;
;
(defun maxinlist(O)
	(if (null O)
		0
		(let*
			(
				(best (maxinlist (cdr O)))
			)
			(if (< best (car O))
				(car O)
				best
			)
		)
	)
)
;
;
;maxind(O)			#returns the index of the largest value in list O. this is 2b in solution outline
;	let*
;	(
;		(maxval maxinlist(O)) 
;		(valind(maxval O))
;	)
;	
;
(defun maxind(O)
	(let*
		(
			(maxval (maxinlist O))
		)
	(valind maxval O)
	)
)
;
;
;getcover(U S)				#returns the set of subsets of S which span list of numbers N. 
;	if (null U) then NIL		#N is already spanned. Stop recursion. This is step 2 in solution outline.
;	let*(
;		(O getoverlap(S U))		#returns list O of the number of overlapping characters for each subset of S and U. This is part 2a of solution.
;		(ind maxind(O))			#stores the index of the subset with the highest number of overlaps in "ind". This is part 2b of solution.
;		(A getind(ind S))		#retrieves the list A with the most overlap with U from S
;		(newU removeoverlap(A U))		#removes all elements in A from U so we can recurse on the remaining elements.
;		getcover(newU S)			#recurses on list of remaining values.
;	)
;
;
(defun getcover(U S)
	(if (null U)
		U
		(let*
			(
			(O (getoverlap S U))
			(ind (maxind O))
			(A (getind ind S))
			(newU (removeoverlap A U))
			)
			(append (cons A NIL) (getcover newU S))
		)
	)
)
;
;
;setcover(N S)					#returns a set of subsets of S which span all numbers <= N greedily
;	getcover((numbers N) S)		#puts N into a list and calls getcover which actually does the recursion. This is step 1 in solution outline
;
;
(defun setcover(N S)
	(getcover (numbers N) S)
)