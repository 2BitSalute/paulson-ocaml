let () =
  (* Unlike SML, OCaml does not allow identifiers as names of user-defined infix operators.
      Conversely, it looks like SML does not allow symbols as infix operators!
  *)
  let (^^^) x y = x lxor y in
  let test_lxor = 7 ^^^ 4 in
  Printf.printf "%d == 3 (%b)\n" test_lxor (test_lxor == 3);

  (* Interestingly, it is possible to redefine the behavior of the operators, including
    lxor, but it is not possible to use, e.g., lxor as a regular function name. It is
    an infix operator and it will continue to be one. *)
  let (lxor) p q = (p || q) && (not (p && q)) in
  Printf.printf "Lxor of true and true is %b\n" (true lxor true);

  let even n = n mod 2 = 0 in

  (* SML has functions be recursive by default *)
  let rec is_power_of_two n =
    (* 2^0 *)
    n = 1 ||
    (even n && is_power_of_two (n / 2))
  in
  Printf.printf "Is 64 a power of 2? %b\n" (is_power_of_two 64);
  Printf.printf "Is 30 a power of 2? %b\n" (is_power_of_two 30);
  Printf.printf "Is 15 a power of 2? %b\n" (is_power_of_two 15);

  let rec power n p =
    if p = 1 then n
    else if even p then
      (* We can do this because (x^m)^n ~= x^mn *)
      power (n * n) (p / 2)
    else
      n * power (n * n) (p / 2)
  in

(*
  Exercise 2.11   Write the computation steps for power 2.0, 29
    First of all, I'll substitute 2 for 2.0, because arithmetic operators are not polymorphic in OCaml.

    power 2 29  =>  2 * (power (2 * 2) (29 / 2))
                =>  2 * (power (4 * 4) (14 / 2))
                =>  2 * (16 * (power (16 * 16) (7 / 2)))
                =>  2 * (16 * (256 * (power (256 * 256) (3 / 2))))
                =>  2 * (16 * (256 * 65536))
                =>  536870912

  Exercise 2.12   How many multiplications does power n p perform in the worst case?
    Something like 2 * ln(p) (1 for every level + 1 for odd powers) - in the case where all p's are odd

  Exercise 2.13   Why not take p = 0 for the base case instead of p = 1?
    That level is useless because it's an extra multiplication of n * n that will be discarded, and
    an extra function call the result of which is always 1 
*)
  Printf.printf "2^29 = %d\n" (power 2 29);
  Printf.printf "2^8 = %d\n" (power 2 8);
  Printf.printf "7^4 = %d\n" (power 7 4);
  Printf.printf "3^3 = %d\n" (power 3 3);

  let () =
    let open Tautology_checker in

    Printf.printf "Goal: %s\n" (show_explicit goal);
    Printf.printf "Goal: %s\n" (show goal);
    
    Printf.printf
      "Is `rich & saintly` true when 'saintly' is true? %b\n"
      (eval_prop
        (Conj ((Atom "rich"), (Atom "saintly")))
        ~true_atoms:["saintly"]
      );

    Printf.printf "\nInitial implementation of Negation Normal Form:\n";

    Printf.printf "Negation Normal Form of assumption 1: %s\n" (show (nnf assumption1));
    Printf.printf "Negation Normal Form of assumption 2: %s\n" (show (nnf assumption2));
    Printf.printf "Negation Normal Form of goal: %s\n" (show (nnf goal));

    Printf.printf "\nAlternative implementation of Negation Normal Form:\n";

    Printf.printf "Negation Normal Form of assumption 1: %s\n" (show (nnf_alt assumption1));
    Printf.printf "Negation Normal Form of assumption 2: %s\n" (show (nnf_alt assumption2));
    Printf.printf "Negation Normal Form of goal: %s\n" (show (nnf_alt goal));

    Printf.printf "\nDistribute disjunctions:\n";

    let example = distrib (Conj (rich, saintly), Conj (landed, Neg rich)) in
    Printf.printf "%s\n" (show example);

    let cnf_of_goal = cnf (nnf goal) in
    Printf.printf "\nCNF of goal: %s\n" (show_explicit cnf_of_goal);

    Printf.printf "\nIs the goal a tautology? %b\n" (taut cnf_of_goal);

    Printf.printf "\nList of CNF of goal prop:\n";
    let list_of_goal = list_of_prop goal in
    let list_of_disj =
      List.map
        (fun disj ->
          "[ " ^ (String.concat "; " disj) ^ " ]")
        list_of_goal
    in
    Printf.printf "[ %s ]\n" (String.concat "; " list_of_disj);
    ()
  in
  Printf.printf "\n\n";
  let () =
    let open Lazy in

    let tungsten_incandescent_lamp = tungsten_incandescent_lamp (Watts 60.0) in
    Printf.printf "%s\n" (show_light tungsten_incandescent_lamp);

    let s_of_seq = show_int_seq (tail (from 1)) in
    Printf.printf "Tail from 1: %s\n" s_of_seq;

    let seven_of_nine = lseq_takeq (7, lseq_from 9) in
    Printf.printf "(Using lseq*) [ %s ]\n" (String.concat "; " (List.map (fun i -> string_of_int i) seven_of_nine));

    let square_seq = squares (from 1) in
    let ten_squares = takeq (10, square_seq) in
    Printf.printf "10 squares from 1: (%s)\n" (String.concat ", " (List.map (fun i -> string_of_int i) ten_squares));
    let sums = takeq (5, addq (from 1000, squares (from 1))) in
    Printf.printf "5 sums: (%s)\n" (String.concat ", " (List.map (fun i -> string_of_int i) sums));

    let finiteq = consq (Some 25, consq (Some 10, Nil)) in
    let append_example = takeq (3, appendq (finiteq, (from 1415))) in
    Printf.printf "Append example: (%s)\n" (String.concat ", " (List.map (fun i -> string_of_int i) append_example));
    ()
  in
  Printf.printf "\n\n";
  let () =
    let open Queue in
    let dequeued = Queue1.deq ["We"; "happy"; "few"] in
    Printf.printf "Dequeued 'we' 'happy' 'few': %s\n" (String.concat ", " (List.map (fun el -> "'" ^ el ^ "'") dequeued));
    ()
  in
  let () =
    let count = Tree.count in
    ignore count;
    let sub = Array.sub in
    ignore sub;
    ()
  in
  let () =
    let open Order.StringTable in
    let table = empty 
      |> update "Crecy" 1346
    in
    ignore table;
  in
  let () =
    let open Pqueue.PQueueInt in
    let pqueue = empty
      |> enq 4
      |> enq 2
      |> enq 6
      |> enq 1
      |> enq 5
      |> enq 8
      |> enq 5
    in
    let rec deq_all pq l =
      if is_empty pq then List.rev l
      else deq_all (deq pq) ((head pq) :: l)
    in
    Printf.printf "Dequeued: %s\n" (String.concat ", " (List.map (fun el -> "'" ^ (string_of_int el) ^ "'") (deq_all pqueue [])));
  in
  ()