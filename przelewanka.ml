(** returns the gcd(a, b), assumes a >= b *)
let rec gcd (a, b) = if b = 0 then a else gcd (b, (a mod b))


(** removes cups with (volume = 0) from the array *)
let remove_zeros arr = 
	let copy = Array.to_list arr in
	let no_zeros = List.filter (fun (vol, _) -> vol <> 0) copy in
	Array.of_list no_zeros


(** returns a pair containing (max(a, b), min(a, b)) *)
let get_maxmin a b = if compare a b >= 0 then (a, b) else (b, a)


(** returns true when at least one cup is required to be empty or full at
	the end of the algorithm, false otherwise *)
let check_for_full_or_empty goal volume = 
	let res = ref false in
	for i = 0 to (Array.length !goal) - 1 do
		if !goal.(i) = 0 || !goal.(i) = !volume.(i) then res := true
	done;
	!res


(** determines whether the problem can be solved based on the input arrays *)
let is_solvable goal volume =
	if (Array.length !volume) = 0 then true else
		let gcd_of_all = List.fold_left (fun acc x -> gcd (get_maxmin acc x))
			!volume.(0) (Array.to_list !volume) in
		let res = ref (check_for_full_or_empty goal volume) in
		if gcd_of_all = 0 then true else begin
			for i = 0 to (Array.length !goal) - 1 do
				if !goal.(i) mod gcd_of_all <> 0 then res := false
			done;
			!res
		end


(** fill_each [move] [cost] [queue] [volume] [curr] - 
	adds states, where each of cups described in [curr] is filled, to the
	[queue] and adds state's required move to [cost] based on [moves]
	needed to achieve state described in [curr], ignores the states that
	were achieved previously *)
let fill_each move cost queue volume curr =
	let check = (Array.copy !curr) in
	let temp = ref 0 in
	for i = 0 to (Array.length !curr) - 1 do
		temp := !curr.(i);
		!curr.(i) <- !volume.(i);
		if compare check !curr <> 0 && not (Hashtbl.mem !cost !curr) then begin
			Hashtbl.add !cost (Array.copy !curr) (move + 1);
			Queue.push (Array.copy !curr) !queue;
		end;
		!curr.(i) <- !temp
	done


(** drain_each [move] [cost] [queue] [volume] [curr] - 
	adds states, where each of cups described in [curr] is drained, to the
	[queue] and adds state's required move to [cost] based on [moves]
	needed to achieve state described in [curr], ignores the states that
	were achieved previously *)
let drain_each move cost queue curr = 
	let check = (Array.copy !curr) in
	let temp = ref 0 in
	for i = 0 to (Array.length !curr) - 1 do
		temp := !curr.(i);
		!curr.(i) <- 0;
		if compare check !curr <> 0 && not (Hashtbl.mem !cost !curr) then begin
			Hashtbl.add !cost (Array.copy !curr) (move + 1);
			Queue.push (Array.copy !curr) !queue;
		end;
		!curr.(i) <- !temp
	done


(** transfer_each [move] [cost] [queue] [volume] [curr] - 
	adds states, where water is transfered between every two cups described in
	[curr], to the [queue] and adds state's required move to [cost] based on
	[moves] needed to achieve state described in [curr], ignores the states
	that were achieved previously *)
let transfer_each move cost queue volume curr = 
	let check = (Array.copy !curr) in
	let temp_i = ref 0 in
	let temp_j = ref 0 in
	let n = (Array.length !curr) - 1 in
	for i = 0 to n do
		for j = 0 to n do
			if i <> j then begin
				temp_i := !curr.(i);
				temp_j := !curr.(j);
				!curr.(j) <- min !volume.(j) (!temp_i + !temp_j);
				!curr.(i) <- max 0 (!temp_i - (!volume.(j) - !temp_j));
				if compare check !curr <> 0 && not (Hashtbl.mem !cost !curr)
					then begin
					Hashtbl.add !cost (Array.copy !curr) (move + 1);
					Queue.push (Array.copy !curr) !queue;
				end;
				!curr.(i) <- !temp_i;
				!curr.(j) <- !temp_j
			end
		done
	done


(** przelewanka [arr] - 
	returns the minimum number of moves required to achieve state described
	in [arr] with the volume limits described in [arr] *)
let przelewanka arr =
	let arr = remove_zeros arr in
	let volume = ref (Array.init (Array.length arr) (fun i -> fst arr.(i))) in
	let goal = ref (Array.init (Array.length arr) (fun i -> snd arr.(i))) in
	let start = ref (Array.make (Array.length arr) 0) in
	let cost = ref (Hashtbl.create (Array.length arr)) in
	let queue = ref (Queue.create ()) in
	let res = ref 0 in
	let stop = ref false in
		begin
			if not (is_solvable goal volume) then -1 else begin
				Hashtbl.add !cost (Array.copy !start) 0;
				Queue.push !start !queue;
				while not (Queue.is_empty !queue) do
					if !stop then Queue.clear !queue else begin
						let curr = ref (Queue.pop !queue) in
						let move = Hashtbl.find !cost !curr in begin
							if compare !curr !goal = 0 then (
								res := Hashtbl.find !cost !goal;
								stop := true
							);
							transfer_each move cost queue volume curr;
							fill_each move cost queue volume curr;
							drain_each move cost queue curr;
						end;
					end
				done;
				if !stop then !res else -1
			end
		end;;


(* ======================== TESTY ======================== *)
assert (przelewanka [| (10,2); (1,1) |] = 5);;
assert (przelewanka [| (0,0); (2,2); (2,2); (2,2); (0,0); (0,0); (1,0);
  (0,0); (1,0) |] = (3));;
assert (przelewanka [| (1,1); (2,1); (3,0); (4,2) |] = (3));;
assert (przelewanka [| (0,0); (2,2); (1,0); (1,1); (1,0); (2,2); (1,0);
  (0,0); (0,0) |] = (3));;
assert (przelewanka [| (11,11); (11,1) |] = (-1));;
assert (przelewanka [| (1,1); (0,0); (2,2); (0,0); (2,0); (0,0); (0,0);
  (1,0); (2,0); (1,0) |] = (2));;
assert (przelewanka [| (5,2); (0,0); (0,0); (2,0); (3,2) |] = (4));;
assert (przelewanka [| (1,1); (0,0); (4,4); (4,0); (4,4) |] = (3));;
assert (przelewanka [| (9,9); (13,12) |] = (10));;
assert (przelewanka [| (2,2); (1,0); (2,2); (0,0); (1,0); (0,0); (1,1);
  (1,0); (0,0) |] = (3));;
assert (przelewanka [| (5,2); (3,1); (0,0); (4,1); (0,0); (1,0) |] = (5));;
assert (przelewanka [| (310,76); (139,91) |] = (-1));;
assert (przelewanka [| (48,9); (12,0); (1,1); (65,64) |] = (10));;
assert (przelewanka [| (7,5); (3,3); (9,4); (10,4); (6,3); (5,3) |] =
  (8));;
assert (przelewanka [| (100000,50000); (1,1) |] = (100000));;
assert (przelewanka [| (0,0); (0,0); (0,0); (300000,151515);
  (1,0); (0,0) |] = (296971));;
assert (przelewanka [| (11,2); (11,10); (4,0); (10,8); (21,16) |] = (12));;
assert (przelewanka [| (50,1); (7,3); (78,64) |] = (-1));;
assert (przelewanka [| (85,23); (524,210) |] = (-1));;
assert (przelewanka [| (557,349); (73,49) |] = (-1));;