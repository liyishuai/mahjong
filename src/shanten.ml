open Base

module SuitSolver = struct
  (* Returns a table: (sets, pairs) -> cost *)
  (* sets: 0..4, pairs: 0..1 (actually usually just 0 or 1 pair total, but a suit could theoretically provide more candidates? No, in standard form we need exactly 1 pair total) *)
  
  let memo = Hashtbl.Poly.create ()

  let solve counts = 
    (* Input: int array of 9 elements *)
    (* Output: (int * int, int) List.t  -> list of ((sets, pairs), cost) *)
    
    (* Since we need to match `Require` which allows leftovers,
       we want to find max sets/pairs extractable, and the cost to reach specific targets.
       
       Actually, `Require(target_sets, target_pairs)` is asking:
       "What is the min tiles added to getting target_sets/target_pairs?"
       
       This can be solved by iterating all possible completions.
       Since we only care about limited targets, we can try to form them.
    *)
    
    let key = Array.to_list counts in
    Hashtbl.find_or_add memo key ~default:(fun () ->
      let results = Hashtbl.Poly.create () in
      
      let rec backtrack idx c_sets c_pairs cost =
        if idx >= 9 then (
          let current_best = Hashtbl.find results (c_sets, c_pairs) |> Option.value ~default:100 in
          if cost < current_best then
            Hashtbl.set results ~key:(c_sets, c_pairs) ~data:cost
        ) else (
          (* 1. Skip *)
          backtrack (idx + 1) c_sets c_pairs cost;
          
          (* 2. Pair (only if we haven't skipped too much? No, any tile can form pair) *)
          (* If we form a pair at idx *)
          if c_pairs < 2 then ( (* Optimization: don't track too many pairs, usually 1 is enough for the global join, but local we might want to know *)
             let need = if counts.(idx) >= 2 then 0 else 2 - counts.(idx) in
             let old = counts.(idx) in
             counts.(idx) <- max 0 (counts.(idx) - 2);
             backtrack idx c_sets (c_pairs + 1) (cost + need);
             counts.(idx) <- old
          );
          
          (* 3. Koutsu *)
          if c_sets < 4 then (
             let need = if counts.(idx) >= 3 then 0 else 3 - counts.(idx) in
             let old = counts.(idx) in
             counts.(idx) <- max 0 (counts.(idx) - 3);
             backtrack idx (c_sets + 1) c_pairs (cost + need);
             counts.(idx) <- old
          );
          
          (* 4. Shuntsu *)
          if c_sets < 4 && idx < 7 then (
             let c1 = counts.(idx) in
             let c2 = counts.(idx + 1) in
             let c3 = counts.(idx + 2) in
             let need = (if c1 > 0 then 0 else 1) + (if c2 > 0 then 0 else 1) + (if c3 > 0 then 0 else 1) in
             counts.(idx) <- max 0 (c1 - 1);
             counts.(idx + 1) <- max 0 (c2 - 1);
             counts.(idx + 2) <- max 0 (c3 - 1);
             backtrack idx (c_sets + 1) c_pairs (cost + need);
             counts.(idx) <- c1;
             counts.(idx + 1) <- c2;
             counts.(idx + 2) <- c3
          )
        )
      in
      backtrack 0 0 0 0;
      (* Fill in the gaps? `Require` might ask for (1, 0) even if we have (2, 0).
         Cost for (1,0) should be <= Cost for (2,0). 
         Actually `Require` asks for EXACT structures? 
         If I have 1,2,3 (1 set), cost for 1 set is 0. Cost for 0 sets is 0.
         
         The DP state in C++:
         `cost[i][j]` is min cost to get `i` sets and `j` pairs.
         It iterates `x` (sets from this suit) and `y` (pairs from this suit).
         
         So we need to return a function `f(sets, pairs) -> cost`.
      *)
      results
    )
end

let get_suit_cost counts x y =
  let results = SuitSolver.solve counts in
  (* We want min cost to get *at least* x sets and y pairs? 
     No, the DP sums up exactly x and y.
     However, a hand with 2 sets naturally satisfies a requirement for 1 set (by dropping one).
     So `Require(x, y)` should be the min cost to find a subset of tiles that forms x sets and y pairs.
     
     Our `backtrack` finds "maximal" disjoint sets.
     We need to be careful. Backtrack consuming 1,2,3 as a set removes them.
     
     Actually, calculating `Require(x, y)` directly is safer.
  *)
  match Hashtbl.find results (x, y) with
  | Some c -> c
  | None -> 
    (* If not found, it might be because we found a "better" one (e.g. we found (2,0) but queried (1,0)).
       But our backtrack explores all branches (skip vs take). 
       "Skip" allows forming fewer sets.
       So (1,0) should be present if (2,0) is possible (by skipping the second set).
       
       Issue: `backtrack` above doesn't explicitly decrement counts for "Skip", it just advances index.
       Wait, `backtrack` at `idx` tries to FORM shape starting at `idx`.
       It also recursively calls `backtrack (idx+1)` which is "Skip".
       So yes, it covers all subsets.
    *)
    100 (* Impossible / Large cost *)

(*
   Main Shanten Logic
*)

let shanten_normal counts num_opens =
  let dp = Array.make_matrix ~dimx:5 ~dimy:2 100 in
  dp.(0).(0) <- 0;
  
  let process_suit offset =
    let suit_counts = Array.init 9 ~f:(fun i -> counts.(offset + i)) in
    (* We need to update DP table.
       New DP table `next_dp`
    *)
    let next_dp = Array.make_matrix ~dimx:5 ~dimy:2 100 in
    
    for i = 0 to 4 - num_opens do
      for j = 0 to 1 do
        if dp.(i).(j) < 100 then (
          (* Try adding x sets and y pairs from this suit *)
          (* Max possible sets from one suit is 4. Max pairs 1 (for the global pair requirement) *)
          for x = 0 to 4 - num_opens - i do
            for y = 0 to 1 - j do
              let required = get_suit_cost suit_counts x y in
              if required < 100 then (
                let ni = i + x in
                let nj = j + y in
                next_dp.(ni).(nj) <- min next_dp.(ni).(nj) (dp.(i).(j) + required)
              )
            done
          done
        )
      done
    done;
    (* Copy next_dp to dp *)
    for i = 0 to 4 do
      for j = 0 to 1 do
        dp.(i).(j) <- next_dp.(i).(j)
      done
    done
  in
  
  process_suit 0; (* Manzu *)
  process_suit 9; (* Pinzu *)
  process_suit 18; (* Souzu *)
  
  (* Honors *)
  for k = 27 to 33 do
    let c = counts.(k) in
    for i = 4 - num_opens downto 0 do
      for j = 1 downto 0 do
        (* Add 1 set (Triplet) *)
        if i >= 1 && dp.(i - 1).(j) < 100 then (
          let cost = if c >= 3 then 0 else 3 - c in
          dp.(i).(j) <- min dp.(i).(j) (dp.(i - 1).(j) + cost)
        );
        (* Add 1 pair *)
        if j >= 1 && dp.(i).(j - 1) < 100 then (
          let cost = if c >= 2 then 0 else 2 - c in
          dp.(i).(j) <- min dp.(i).(j) (dp.(i).(j - 1) + cost)
        )
      done
    done
  done;
  
  dp.(4 - num_opens).(1) - 1

let shanten_seven_pairs counts =
  let num_pairs = 
    Array.fold counts ~init:0 ~f:(fun acc c -> if c >= 2 then acc + 1 else acc)
  in
  let num_types = 
    Array.fold counts ~init:0 ~f:(fun acc c -> if c >= 1 then acc + 1 else acc)
  in
  (* 7 pairs shanten: 6 - pairs + (missing types if any? no, usually just 6 - pairs) 
     Standard: 6 - pairs.
     But must have 7 distinct pairs.
     If we have 4 identical tiles, it counts as 2 pairs? 
     Standard 7-pairs rule: 7 *distinct* pairs. 
     mjx implementation: 
       n = count[i] >= 1
       m = count[i] >= 2
       14 - min(n, 7) - m - 1 ??? 
       Let's check C++: 14 - min(n, 7) - m - 1.
       If we have 7 pairs (m=7, n>=7), result = 14 - 7 - 7 - 1 = -1 (Agari). Correct.
  *)
  let n = num_types in
  let m = num_pairs in
  14 - min n 7 - m - 1

let shanten_thirteen_orphans counts =
  let orphans = [0; 8; 9; 17; 18; 26; 27; 28; 29; 30; 31; 32; 33] in
  let n = List.count orphans ~f:(fun i -> counts.(i) >= 1) in
  let m = List.count orphans ~f:(fun i -> counts.(i) >= 2) in
  14 - n - min m 1 - 1

let calculate hand_counts num_opens =
  if num_opens = 0 then
    min (shanten_normal hand_counts 0)
      (min (shanten_seven_pairs hand_counts) (shanten_thirteen_orphans hand_counts))
  else
    shanten_normal hand_counts num_opens

let proceeding_tiles hand num_opens =
  let current_shanten = calculate hand num_opens in
  Array.init 34 ~f:(fun tile_type ->
    if hand.(tile_type) >= 4 then
      false (* Can't add more than 4 of the same tile *)
    else
      let test_hand = Array.copy hand in
      test_hand.(tile_type) <- test_hand.(tile_type) + 1;
      let new_shanten = calculate test_hand num_opens in
      new_shanten < current_shanten
  )
