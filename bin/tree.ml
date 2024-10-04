type ('k, 'v) tree =
  | Node of 'k * 'v * ('k, 'v) tree * ('k, 'v) tree
  | Leaf

let rec dump dumpKey dumpValue tree =
  match tree with
  | Leaf -> "Leaf"
  | Node (key, value, less, greater) ->
    Printf.sprintf
      "Node (%s, %s, %s, %s)"
      (dumpKey key)
      (dumpValue value)
      (dump dumpKey dumpValue less)
      (dump dumpKey dumpValue greater)
;;

(* let empty = Leaf *)

let rec insert compare key value tree =
  match tree with
  | Leaf -> Node (key, value, Leaf, Leaf)
  | Node (key', value', less, greater) ->
    (match compare key key' with
     | 0 -> Node (key', value', less, greater)
     | x when x < 0 -> Node (key', value', insert compare key value less, greater)
     | _ -> Node (key', value', less, insert compare key value greater))
;;

(* Функция ищет значение по ключу, хвостово-рекурсивная *)
let rec find compare key tree =
  match tree with
  | Leaf -> None
  | Node (key', value', less, greater) ->
    (match compare key key' with
     | 0 -> Some value'
     | x when x < 0 -> find compare key less
     | _ -> find compare key greater)
;;

let rec map f tree =
  match tree with
  | Leaf -> Leaf
  | Node (key, value, less, greater) -> Node (key, f value, map f less, map f greater)
;;

(* Проверка наличия значения в дереве по данному признаку. Не хвостово-рекурсивная *)
let rec exists criteria tree =
  match tree with
  | Leaf -> false
  | Node (_, value, less, greater) ->
    criteria value || exists criteria less || exists criteria greater
;;

let fold f acc tree =
  let rec fold' f acc tree k =
    match tree with
    | Leaf -> k acc
    | Node (_, value, less, greater) ->
      fold' f acc less (fun acc -> fold' f (f acc value) greater k)
  in
  fold' f acc tree (fun acc -> acc)
;;

let all p tree =
  let rec all' p tree k =
    match tree with
    | Leaf -> k true
    | Node (_, value, less, greater) ->
      p value && all' p less (fun result -> result && all' p greater k)
  in
  all' p tree (fun x -> x)
;;

let () =
  let insert = insert String.compare in
  let dump = dump (Printf.sprintf "%s") (Printf.sprintf "%d") in
  let nodes =
    Leaf
    |> insert "kns" 2
    |> insert "uek" 2
    |> insert "udm" 4
    |> insert "bkp" 2
    |> insert "bvm" 2
    |> insert "sd" 6
  in
  nodes |> dump |> Printf.printf "%s\n";
  let find_result name =
    match nodes |> find String.compare name with
    | Some index -> Printf.sprintf "%s has value %d" name index
    | None -> Printf.sprintf "no %s was found" name
  in
  find_result "bii" |> Printf.printf "%s\n";
  nodes |> map (( + ) 1) |> dump |> Printf.printf "%s\n";
  nodes |> exists (function x -> x mod 7 == 0) |> Printf.printf "%B\n";
  nodes |> fold ( + ) 0 |> Printf.printf "%i\n";
  nodes |> all (fun x -> x mod 2 == 0) |> Printf.printf "%B\n"
;;
