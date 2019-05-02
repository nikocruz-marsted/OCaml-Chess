open OUnit2
open ChessGame
open ChessBoard
open ChessAgent
open ChessPiece

module Chess = ChessGame.Make(ChessBoard)
open Chess

(* ~a wild monad appeared~ *)
module MaybeMove = struct
  type game = Chess.t
  type maybeGame = Chess.t option
  let return (x : Chess.t) : maybeGame =
    Some x
  let bind (x : maybeGame) (move : Chess.t -> maybeGame) : maybeGame =
  match x with
  | None -> None
  | Some a -> move a
  let (>>=) = bind
end

let cat_if_not_empty str acc =
  if str = "" then acc else str::acc

(** Helper function to convert the letter in [coord] to the index of
    a column in the board. *)
let conv_letter coord =
  ((String.get coord 0) |> Char.uppercase_ascii |> Char.code) - 65

(** Helper function to process the number in [coord]. *)
let conv_number coord = String.sub coord 1 1 |> int_of_string_opt

let parse str =
  let split_str = String.split_on_char ' ' str in
  let nosp_str = List.map String.trim split_str in
  let non_empty_list = List.fold_right cat_if_not_empty nosp_str [] in
  match non_empty_list with
  | coord1::coord2::[] ->
    let letter1 = conv_letter coord1 in
    let number1 = conv_number coord1 in
    let letter2 = conv_letter coord2 in
    let number2 = conv_number coord2 in
    begin
    match number1, number2 with
    | Some n1, Some n2 -> ((n1 - 1, letter1), (n2 - 1, letter2))
    | _ -> failwith "no valid move provided"
    end
  | _ -> failwith "no valid move provided"

let move_op (moveString, color) game =
   let coords = parse moveString in
   let src1,dst1 = coords in
   let tryMove = Chess.move game color ({src = src1; dest = dst1}) in
      match tryMove with
      |Ok g -> Some g
      |Error err_msg -> None

let make_valid_move_test
    (name : string)
    (move : string)
    (color: ChessPiece.color)
    (expected_out : bool) =
  name >:: (fun _ -> 
   let coords = parse move in
   let src1,dst1 = coords in
   let tryMove = Chess.move (Chess.init ChessPiece.init) color ({src = src1; dest = dst1}) in
      match tryMove with
      |Ok _ -> assert_equal true expected_out
      |Error err_msg -> assert_equal false expected_out
    )

let make_multi_move_test
    (name : string)
    (moves : string list)
    (colors : ChessPiece.color list)
    (expected_out : bool) = 
  name >:: (fun _ ->
      let move_list = List.map move_op (List.combine moves colors) in
      match (List.fold_left MaybeMove.bind
          (MaybeMove.return (Chess.init ChessPiece.init)) move_list) with
      | None -> assert_equal false expected_out
      | Some _ -> assert_equal true expected_out      
    )

let make_status_test
    (name : string)
    (moves : string list)
    (colors : ChessPiece.color list)
    (check_color : ChessPiece.color)
    (status : Chess.check_status) =
  name >:: (fun _ ->
      let move_list = List.map move_op (List.combine moves colors) in
      match (List.fold_left MaybeMove.bind
          (MaybeMove.return (Chess.init ChessPiece.initTest1)) move_list) with
      | None -> failwith "invalid move set"
      | Some game -> assert_equal status (Chess.status game check_color)
    )

let make_castle_test
    (name : string)
    (move : string)
    (color: ChessPiece.color)
    (expected_out : bool) =
  name >:: (fun _ -> 
   let coords = parse move in
   let src1,dst1 = coords in
   let tryMove = Chess.move (Chess.init ChessPiece.initCastleTest) color ({src = src1; dest = dst1}) in
      match tryMove with
      |Ok _ -> assert_equal true expected_out
      |Error err_msg -> assert_equal false expected_out
    )


let make_passant_test
    (name : string)
    (moves : string list)
    (colors : ChessPiece.color list)
    (expected_out : bool) =
  name >:: (fun _ ->
      let move_list = List.map move_op (List.combine moves colors) in
      match (List.fold_left MaybeMove.bind
          (MaybeMove.return (Chess.init ChessPiece.initPassantTest)) move_list) with
      | None -> assert_equal false expected_out
      | Some _ -> assert_equal true expected_out
    )

(* testing if piece movement works correctly*)
let move_tests =
  [
    make_valid_move_test "pawn forward 1" "A2 A3" ChessPiece.White true;
    make_valid_move_test "pawn forward 2" "A2 A4" ChessPiece.White true;
    make_valid_move_test "pawn forward 1" "B7 B6" ChessPiece.Black true;
    make_valid_move_test "pawn forward 2" "B7 B5" ChessPiece.Black true;
    make_valid_move_test "knight move LW" "G1 H3" ChessPiece.White true;
    make_valid_move_test "knight move LW2" "B1 C3" ChessPiece.White true;
    make_valid_move_test "knight move LB" "B8 A6" ChessPiece.Black true;
    make_valid_move_test "knight move LB2" "G8 H6" ChessPiece.Black true;
    make_valid_move_test "knight invalid B" "G8 G6" ChessPiece.Black false;
    make_valid_move_test "knight invalid W" "B1 B3" ChessPiece.Black false;
    make_valid_move_test "rook invalid" "A1 A4" ChessPiece.White false;
    make_valid_move_test "bishop invalid" "C1 F4" ChessPiece.White false;
    make_valid_move_test "queen invalid" "D1 D5" ChessPiece.White false;
    make_valid_move_test "king invalid" "E1 E2" ChessPiece.White false;
    make_valid_move_test "king invalid B" "E8 F8" ChessPiece.Black false;
    make_valid_move_test "rook invalid B" "H8 H7" ChessPiece.Black false;
    make_multi_move_test "pawn forward twice" ["A2 A3"; "A3 A4"] 
      [White;White] true;
    make_multi_move_test "pawn forward not twice" ["A2 A3"; "A3 A5"] 
      [White;White] false;
    make_multi_move_test "not your piece" ["A2 A3"; "A3 A4"] 
      [White;Black] false;
    
    make_multi_move_test "rook out" ["A2 A4"; "A1 A3"; "A3 G3"]
      [White;White;White] true;
    make_multi_move_test "bishop out" ["D2 D4"; "C1 F4"]
      [White;White] true;
    make_multi_move_test "queen out" ["E2 E4"; "D1 H5"]
      [White;White] true;
    make_multi_move_test "king out" ["E2 E4"; "E1 E2"]
      [White;White] true;
    make_multi_move_test "rook multi invalid" ["A2 A4"; "A1 A4"]
      [White;White] false;
    make_multi_move_test "bishop multi invalid" ["D2 D4"; "C1 F5"]
      [White;White] false;
    make_multi_move_test "queen multi invalid" ["E2 E4"; "D1 H4"]
      [White;White] false;    
  ]

(* testing if piece capturing works correctly*)
let capture_tests = 
  [
    make_multi_move_test "pawn capture" ["A2 A4"; "B7 B5"; "A4 B5"]
      [White;Black;White] true;
    make_multi_move_test "rook capture" ["A2 A4"; "A1 A3"; "A3 G3"; "G3 G7"]
      [White;White;White;White] true;
    make_multi_move_test "bishop capture" ["D2 D4"; "C1 F4"; "F4 C7"]
      [White;White;White] true;
    make_multi_move_test "queen capture" ["E2 E4"; "D1 H5"; "H5 H7"]
      [White;White;White] true;
  ]

(* testing if checking if a king is either free, 
 * in check, or in checkmate work correctly*)
let status_tests = 
  [
    make_status_test "check test" ["D1 E2"] [White] Black Check; 
    make_status_test "safe test" ["D1 F3"] [White] Black Safe;
    make_status_test "checkmate test" ["A1 A8"; "D1 A4"; "A4 A7"] 
      [White; White; White] Black CheckMate;
    make_status_test "check with bishop" ["F1 B5"] [White] Black Check;
    make_status_test "check with rook" ["H1 H8"] [White] Black Check;
    make_status_test "check with knight" ["G1 F3"; "F3 G5"; "E8 F8"; "G5 H7"] 
      [White;White;Black;White] Black Check;
  ]

(* testing if castling works correctly *)
let castle_tests = 
  [
    make_castle_test "king castle 1" "E1 G1" ChessPiece.White true; 
    make_castle_test "king castle 2" "E1 C1" ChessPiece.White true;
    make_castle_test "king castle invalid 1" "E1 B1" ChessPiece.White false;
    make_castle_test "king castle invalid 2" "E1 H1" ChessPiece.White false;
  ]

(* testing if en passant works correctly *)
let en_passant_tests = 
  [ 
    make_passant_test "white passant black" ["A2 A4"; "A4 A5"; "B7 B5"; "A5 B6"]
      [White;White;Black;White] true;
    make_passant_test "white passant black too late" 
      ["A2 A4"; "A4 A5"; "B7 B5";"B2 B3";"A5 B6"]
      [White;White;Black;White;White] false;
  ]


let suite =
  "chess test suite" >::: List.flatten [
    move_tests;
    capture_tests;
    status_tests;
    castle_tests;
    en_passant_tests;
  ]

let _ = run_test_tt_main suite

