(defstruct state
	;;structure for holding information of states
	node 
	g_val
	h_val
	f_val
	parent
)
(defvar *missionary_count* 15)    							;; declare missionary count as a global variable
(defvar *cannibal_count* 15)								;; declare cannibal count as a global variable
(defvar *boat_capacity* 6)									;; declare boat capacity as a global variable
(defvar *goal_node* (list '0 '0 '0 '0 '1))					;; declare goal node as a global variable



(defvar *visited_nodes* nil)
(defun calc_h_val (curr_node)
	(if(eq curr_node NIL)
		(return-from calc_h_val 9999)
	)
	(return-from calc_h_val (float (/ (+ (first curr_node) (second curr_node)) *boat_capacity*)))
)

(defun set_h_val(temp_node)
;;	helper function for the function calc_h_val
	(setf h (calc_h_val temp_node))
)

(defun init_state(temp_node)
;;	function that is used to initialise the values of a newly generated state
	(setq new_state (make-state :node temp_node
		:g_val 0
		:h_val ((lambda (temp_node) (setf h (set_h_val temp_node))) temp_node)
		:f_val 9999
		:parent NIL ))

	(return-from init_state new_state)
)

(defun is_safe_left(given_node)
;;	function which checks whether left bank is safe
	(setf m_left (first given_node))
	(setf c_left (second given_node))
	(if(or (< m_left 0) (< c_left 0))(return-from is_safe_left nil))

	(if(and (or (>= m_left c_left) (eq m_left 0)) (<= m_left *missionary_count*) (<= c_left *cannibal_count*))
		(return-from is_safe_left T)
		)

)

(defun is_safe_boat(given_node)
;;	function which checks whether boat is safe
	(setf m_boat (third given_node))
	(setf c_boat(fourth given_node))
	(if(or (< m_boat 0) (< c_boat 0))(return-from is_safe_boat nil))
	;;return true if there is atleast 1 creature on boat and total number of creatures is less than boat capacity
	;;                                                   1. number of missionaries are greater than or equal to number of cannibals on boat or
	;;				  									 2. number of cannibals on boat is zero
	;;															

	(if(and (or (>= m_boat c_boat) (equal m_boat 0)) (<= (+ m_boat c_boat) *boat_capacity*) (>= (+ m_boat c_boat) 1))
		(return-from is_safe_boat T)
	)
)

(defun is_safe_right(given_node)
;;	function which checks if right bank is safe
	(setf m_right (- *missionary_count* (+ (first given_node) (third given_node))))
	(setf c_right (- *cannibal_count* (+ (second given_node) (fourth given_node))))
	(if(or (< m_right 0) (< c_right 0))(return-from is_safe_right nil))
	(if(and (or (>= m_right c_right) (equal m_right 0)) (<= m_right *missionary_count*) (<= c_right *cannibal_count*))
		(return-from is_safe_right T)
	)
	
)

(defun is_safe(given_node)
;;	function which checks whether the given state satisfies the given constraints in the problem
	(if(and (is_safe_left given_node) (is_safe_boat given_node) (is_safe_right given_node))
		(return-from is_safe T)
		)
)

(defun is_present(given_node node_list)
;;	function which checks whether a given node is present in a node list
	(setf first_four (subseq given_node 0 4))

	(loop for n in node_list do
		(progn
			(setf f_four (subseq n 0 4 ))
			(if(equal first_four f_four)(return-from is_present T))
		)

	)

	(return-from is_present nil)

)

(defun is_member (given_state state_list)
;;	function which checks whether a given state is a member of the state list
	(setf first_four (subseq (state-node given_state) 0 4))
	(loop for n in state_list do
		(progn
			(setf f_four (subseq (state-node n) 0 4 ))
			(if(equal first_four f_four)(return-from is_member T))
		)

	)
	(return-from is_member nil)
)


(defun generate_Successors(given_state)
;;	function that generates successor states for a given state
	(setf boat_pos (fifth (state-node given_state)))
	(setf m_l (first (state-node given_state)))
	(setf c_l (second (state-node given_state)))
	(setf m_b (third (state-node given_state)))
	(setf c_b (fourth (state-node given_state)))
	(setf m_r (- *missionary_count* (+ m_l m_b)))
	(setf c_r (- *cannibal_count* (+ c_l c_b)))
	(setf sons nil)
	(if(eq boat_pos 0)
		( ;;if the boat is at the left bank
			progn
			(
				loop for i from 0 to (+ m_l m_b) do
				(progn
				(
					loop for j from 0 to (+ c_l c_b) do 
					(progn
						(setf son_node (list (- (+ m_l m_b) i) (- (+ c_l c_b) j) i j '1))

						( if(and (is_safe son_node) (not(is_present son_node *visited_nodes*)))
							(progn
								(setf son (init_state son_node))
								(setf (state-parent son) given_state)
								(push son sons)
							)
						)
					)
				)
			)
			)
			
			(return-from generate_Successors sons)
		)
		( ;;if the boat is at the right bank
			progn
			
				(loop for i from 0 to (+ m_b m_r)do
				(progn
				(
					loop for j from 0 to (+ c_b c_r) do
					(progn
						
							(if(not(eq (+ i j) 0)) ;;atleast 1 creature should get down from the boat

							(progn
								(setf son_node (list m_l c_l (- (+ m_b m_r) i) (- (+ c_b c_r) j) '0))
								( if(and (is_safe son_node) (not (is_present son_node *visited_nodes*)))
						 			(progn
										(setf son (init_state son_node))
										(setf (state-parent son) given_state)
										(push son sons)
									)
								)
							)
							)

					)
				)	
				)
				)				
			(return-from generate_Successors sons)
			)
	)
)

(defun best_state (open_list) 
;; 	function that returns best state among list of states present in the open list

	(setf min_el (init_state (state-node (nth 0 open_list))))
	(setf (state-h_val min_el) 9999)
	(block list_loop
		(loop for s in open_list do
			(if(< (state-h_val s) (state-h_val min_el))(setf min_el (copy-state s)) () )
		)
		(return-from list_loop min_el)
	)
)

(defun is_near_goal(curr_state)
;; 	function that checks whether goal state can be reached from the current state
	(if(eq (state-h_val curr_state) 0.0)
		(return-from is_near_goal T)
	)
	
)

(defun remove_element (el s_list)
;;	function which removes the given element from the given list
	(setq m_list nil)
	(loop for states in s_list do
		(progn
			(if(not(equal (state-node states) (state-node el)))
				(
					push states m_list
				)
			)
			
		)
		collect m_list
	)
	(return-from remove_element m_list)
)


(defun print_path(curr_state states_expanded)
;;	 function that prints path from start state to goal state
	(format t "~%~%Search successful!!~%")
	(format t "~v@{~A~:*~}~%~%" 70 "-")
	(format t "Generated states are: ~% ~%")
	(setf node_list nil)
	(if(not(equal (state-node curr_state) *goal_node*))(push *goal_node* node_list))
	(loop while(not(equal curr_state nil)) do
		(progn
			(push (state-node curr_state) node_list)
			(setf curr_state (state-parent curr_state))
		)
	)

	(loop for n in node_list do
		(progn
			(setf left_bank_config (list (first n) (second n) ))
			(setf boat_config (list (third n) (fourth n)))
			(setf right_bank_config (list (- *missionary_count* (+ (first n) (third n))) (- *cannibal_count* (+ (second n) (fourth n))) ) )
			;(format t "left_bank : ~a boat: ~a  right_bank: ~a  boat_position: ~a ~%" left_bank_config boat_config right_bank_config (fifth n))
			(format t "m_left: ~a  c_left: ~a  m_boat: ~a  c_boat: ~a  m_right: ~a  c_right: ~a  boat_position: ~a ~%" (first left_bank_config)
												(second left_bank_config) (first boat_config) (second boat_config) 
												(- *missionary_count* (+ (first n) (third n))) (- *cannibal_count* (+ (second n) (fourth n)))
												(fifth n) )

		)
		
	)
	(format t "~%~v@{~A~:*~}~%~%" 70 "-")


)


(defun MCP_SOLVER (start_state goal_node)
;;	main function that executes the a-star algorithm using helper functions
	(format t "~v@{~A~:*~}~%" 70 "-")
	(format t "Executing Missionary Cannibal problem solver: ~%")
	(format t "~v@{~A~:*~}~%~%" 70 "-")
	(setf s_node (state-node start_state))
	(setf g_node goal_node)
	(format t "~%Start state: ~a" s_node)
	(format t "~%Goal state: ~a ~% ~%" goal_node)
	(format t "~v@{~A~:*~}~%~%" 70 "-")
	(setf states_expanded 0)
	(setf counter 0)
	(if(null start_state)
		(progn
			;;returns failure if the open list is null at any point of time of execution
			(format t "~%Failure!!~%")
			(return-from MCP_SOLVER "Failure")
		)
		
	)
	(setf open_list (list start_state))
	(setf counter 0)


	(loop while (not(null open_list)) do
	(progn

		(setf counter (+ 1 counter))
		(format t "loop count: ~a " counter)
		(setf current (best_state open_list)) ;;select the best state from open list
		(incf states_expanded)
		(if(is_near_goal current) ;check whether current state is equal to goal state
			(progn
					
				(print_path current states_expanded)
				(return-from MCP_SOLVER "success")
			)
			
		)
		
		(setf sons (generate_Successors current)) ;generate all successor states of the current best node
		

		(loop for son in sons do
			(progn
				(if(is_near_goal son) ;check whether current state is equal to goal state
				(progn
					
					(print_path son states_expanded)
					(return-from MCP_SOLVER "success")
				)
			
				)

				( if(state-node son)
					(progn
						(if(and (not(is_present (state-node son) *visited_nodes*)) (not (is_member son open_list)))
							(push son open_list))
						)						
					)
				)
			)
		)
		(push (state-node current) *visited_nodes*) ;add the current element in the closed list
		(setf open_list (remove_element current open_list)) ;remove the current element from the open list
		(format t "open list count: ~a ~%" (list-length open_list))
	)

	(return-from MCP_SOLVER "failure")

	)


(defvar *start_state* (init_state (list *missionary_count* *cannibal_count* '0 '0 '0))) ;declare start state as a global variable


(defun missionary_cannibal_solver()
;; calls the MCP_SOLVER for the given configuration
(MCP_SOLVER *start_state* *goal_node*) ;call MCP_SOLVER function with appropriate parameters
)
