let factorial =
  let rec factorial' acc n =
    match n with
    | 0 -> acc
    | n -> factorial' (acc * n) (n - 1)
  in
  factorial' 1
;;

let () = factorial 20 |> Printf.printf "%i\n"

let fibonacci =
  let rec fibonacci' a b n =
    match n with
    | 0 -> a
    | 1 -> b
    | n when n > 0 -> fibonacci' b (a + b) (n - 1)
    | n -> fibonacci' (b - a) a (n + 1)
  in
  fibonacci' 0 1
;;

let () = fibonacci (-8) |> Printf.printf "%i\n"

let filter =
  let rec filter' acc f l =
    match l with
    | [] -> List.rev acc
    | x :: xs -> if f x then filter' (x :: acc) f xs else filter' acc f xs
  in
  filter' []
;;

let even x = x mod 2 = 0

let () =
  [ 1; 2; 5; 7; 34 ] |> filter even |> List.iter (Printf.printf "%i ");
  Printf.printf "\n"
;;

let rec fold f acc l =
  match l with
  | [] -> acc
  | x :: xs -> fold f (f acc x) xs
;;

let () = [ 1; 2; 5; 7; 34 ] |> fold ( + ) 0 |> Printf.printf "%i\n"
let filter' f l = fold (fun acc x -> if f x then x :: acc else acc) [] l |> List.rev

let () =
  [ 1; 2; 5; 7; 34 ] |> filter' even |> List.iter (Printf.printf "%i ");
  Printf.printf "\n"
;;
