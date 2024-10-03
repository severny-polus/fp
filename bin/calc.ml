type op =
  | Plus
  | Minus
  | Times
  | Slash

let apply op =
  match op with
  | Plus -> ( + )
  | Minus -> ( - )
  | Times -> ( * )
  | Slash -> ( / )
;;

let show_op op =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Slash -> "/"
;;

type expr =
  | Num of int
  | Op of expr * op * expr

let error message s start i =
  Printf.eprintf "Error: %s\n" message;
  Printf.eprintf " %s\n" s;
  Printf.eprintf " %s%s\n" (String.make start ' ') (String.make (i - start) '^');
  raise Exit
;;

let rec parse_number s start i =
  match s.[i] with
  | '0' .. '9' -> parse_number s start (i + 1)
  | _ | (exception Invalid_argument _) ->
    let token = String.sub s start (i - start) in
    (match int_of_string_opt token with
     | Some n -> Num n, i
     | None -> error "invalid numeral" s start i)
;;

let rec skip_whitespace s i =
  match s.[i] with
  | ' ' | '\t' | '\n' | '\r' -> skip_whitespace s (i + 1)
  | _ | (exception Invalid_argument _) -> i
;;

let rec parse_parenthesis s i =
  let start = i in
  let i = skip_whitespace s (i + 1) in
  let expr, i = parse_additive s i in
  let i = skip_whitespace s i in
  match s.[i] with
  | ')' -> expr, i + 1
  | _ | (exception Invalid_argument _) -> error "unclosed parenthesis" s start (start + 1)

and parse_multiplicative s i =
  let parse_operand s i =
    match s.[i] with
    | '0' .. '9' | '-' -> parse_number s i (i + 1)
    | '(' -> parse_parenthesis s i
    | _ | (exception Invalid_argument _) -> error "expected an expression" s i (i + 1)
  in
  let initial, i = parse_operand s i in
  let rec parse_more s left i =
    let i = skip_whitespace s i in
    match s.[i] with
    | ('*' | '/') as operator ->
      let i = skip_whitespace s (i + 1) in
      let right, i = parse_operand s i in
      let operator = if operator = '*' then Times else Slash in
      parse_more s (Op (left, operator, right)) i
    | _ | (exception Invalid_argument _) -> left, i
  in
  parse_more s initial i

and parse_additive s i =
  let parse_operand s i =
    match s.[i] with
    | '0' .. '9' | '.' | '-' | '(' -> parse_multiplicative s i
    | _ | (exception Invalid_argument _) -> error "expected an expression" s i (i + 1)
  in
  let initial, i = parse_operand s i in
  let rec parse_more s left i =
    let i = skip_whitespace s i in
    match s.[i] with
    | ('+' | '-') as operator ->
      let i = skip_whitespace s (i + 1) in
      let right, i = parse_operand s i in
      let operator = if operator = '+' then Plus else Minus in
      parse_more s (Op (left, operator, right)) i
    | _ | (exception Invalid_argument _) -> left, i
  in
  parse_more s initial i
;;

let parse s =
  let i = skip_whitespace s 0 in
  let expr, i = parse_additive s i in
  if i < String.length s then error "unexpected trailing symbols" s i (String.length s);
  expr
;;

let rec eval e =
  match e with
  | Num n -> n
  | Op (l, op, r) -> (apply op) (eval l) (eval r)
;;

let show =
  let rec show' need_parens is_right e =
    match e with
    | Num n -> Printf.sprintf "%d" n
    | Op (l, op, r) ->
      (match op with
       | Plus | Minus ->
         Printf.sprintf
           (if need_parens || is_right then "(%s %s %s)" else "%s %s %s")
           (show' false false l)
           (show_op op)
           (show' false true r)
       | Times | Slash ->
         Printf.sprintf
           (if is_right then "(%s %s %s)" else "%s %s %s")
           (show' true false l)
           (show_op op)
           (show' true true r))
  in
  show' false false
;;

let () =
  let e = In_channel.stdin |> input_line |> parse in
  e |> eval |> Printf.printf "%s = %i\n" (show e);
  assert (parse (show e) = e)
;;
