let print_array arr = 
	for i = 0 to (Array.length arr) - 1 do
		Printf.printf "%d " arr.(i)
	done;
	print_newline ()

let fill_each move cost queue volume curr =
	let check = (Array.copy !curr) in
	let temp = ref 0 in
	for i = 0 to (Array.length !curr) - 1 do
		temp := !curr.(i);
		!curr.(i) <- !volume.(i);
		Hashtbl.iter (fun arr v -> print_array arr; Printf.printf "dist - %d\n" v) !cost;
		print_array !curr;
		Printf.printf "%B %B\n" (compare check !curr <> 0) (not (Hashtbl.mem !cost !curr));
		if Hashtbl.mem !cost !curr then Printf.printf "exists - %d\n" (Hashtbl.find !cost (Array.copy !curr));
		if compare check !curr <> 0 && not (Hashtbl.mem !cost !curr) then begin
			Hashtbl.add !cost (Array.copy !curr) (move + 1);
			Queue.push (Array.copy !curr) !queue;
		end;
		!curr.(i) <- !temp;
	done

let drain_each move cost queue curr = 
	let check = (Array.copy !curr) in
	let temp = ref 0 in
	for i = 0 to (Array.length !curr) - 1 do
		temp := !curr.(i);
		!curr.(i) <- 0;
		if compare check !curr <> 0 && not (Hashtbl.mem !cost !curr) then begin
			Hashtbl.add !cost (Array.copy !curr) (move + 1);
			Queue.push (Array.copy !curr) !queue;
			(* print_array !curr; *)
		end;
		!curr.(i) <- !temp;
	done

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
					(* print_array !curr; *)
				end;
				!curr.(i) <- !temp_i;
				!curr.(j) <- !temp_j;
			end
		done
	done

(** usefull comment on what this function does ;-] *)
let przelewanka arr = 
	let volume = ref (Array.init (Array.length arr) (fun i -> fst arr.(i))) in
	let goal = Array.init (Array.length arr) (fun i -> snd arr.(i)) in
	let start = ref (Array.make (Array.length arr) 0) in
	let cost = ref (Hashtbl.create (Array.length arr)) in
	let queue = ref (Queue.create ()) in
	let res = ref 0 in
	let stop = ref false in
		(* begin
			Hashtbl.add !cost !start 0;
			Queue.push !start !queue;
			while not (Queue.is_empty !queue) do
				if !stop then Queue.clear !queue else begin
					print_array (Queue.top !queue);
					assert (Hashtbl.mem !cost (Queue.top !queue));
					let curr = ref (Queue.pop !queue) in
					let move = Hashtbl.find !cost !curr in begin
						(* Printf.printf "%d\n" (compare !curr goal); *)
						(* Printf.printf "%d\n" (Hashtbl.find !cost !curr); *)
						if compare !curr goal = 0 then (
							assert (Hashtbl.mem !cost goal);
							res := Hashtbl.find !cost goal;
							stop := true
						);
						transfer_each move cost queue volume curr;
						fill_each move cost queue volume curr;
						drain_each move cost queue curr;
					end;
					(* print_endline "------------\n" *)
				end
			done;
			if !stop then !res else -1
		end *)
		begin
			print_array !start;
			print_array goal;
			print_array !volume;
			Hashtbl.add !cost !start 0;
			(* print_endline "----------------------------------";
			transfer_each 0 cost queue volume start;
			print_endline "-----------------";
			Queue.iter (fun arr -> print_array arr) !queue;
			Queue.clear !queue; *)
			print_endline "----------------------------------";
			fill_each 0 cost queue volume start;
			print_endline "-----------------";
			Queue.iter (fun arr -> print_array arr) !queue;
			Queue.clear !queue;
			(* print_endline "----------------------------------";
			drain_each 0 cost queue start;
			print_endline "-----------------";
			Queue.iter (fun arr -> print_array arr) !queue;
			Queue.clear !queue; *)
		end



let a = [|(0, 0); (21, 21)|]