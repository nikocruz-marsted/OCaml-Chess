open ChessPiece
open ChessGame
open ChessBoard
open Agent

module ChessAI :
  Agent with module P = ChessPiece and
module G = ChessGame.Make(ChessBoard) = struct

  module P = ChessPiece
  module G = ChessGame.Make(ChessBoard)

  type t = P.move option

  let pawn_table =
    [|
      [| 0.0;  0.0;  0.0;  0.0;  0.0;  0.0;  0.0;  0.0|]; 
      [| 0.5;  1.0;  1.0; -2.5; -2.5;  1.0;  1.0;  0.5|];
      [| 0.5; -0.5; -1.0;  0.0;  0.0; -1.0; -0.5;  0.5|];
      [| 0.0;  0.0;  0.0;  2.0;  2.0;  0.0;  0.0;  0.0|];
      [| 0.5;  0.5;  1.5;  2.5;  2.5;  1.0;  0.5;  0.5|];
      [| 1.0;  1.0;  2.0;  3.0;  3.0;  2.0;  1.0;  1.0|];
      [| 5.0;  5.0;  5.0;  5.0;  5.0;  5.0;  5.0;  5.0|];
      [| 5.0;  5.0;  5.0;  5.0;  5.0;  5.0;  5.0;  5.0|]
    |]

  let knight_table =
    [|
      [|-5.0; -4.0; -2.0; -3.0; -3.0; -2.0; -4.0; -5.0|]; 
      [|-4.0; -2.0;  0.0;  0.5;  0.5;  0.0; -2.0; -4.0|];
      [|-3.0;  0.5;  1.0;  1.5;  1.5;  1.0;  0.5; -3.0|];
      [|-3.0;  0.0;  1.5;  2.0;  2.0;  1.5;  0.0; -3.0|];
      [|-3.0;  0.5;  1.5;  2.0;  2.0;  1.5;  0.5; -3.0|];
      [|-3.0;  0.0;  1.0;  1.5;  1.5;  1.0;  0.0; -3.0|];
      [|-4.0; -2.0;  0.0;  0.0;  0.0;  0.0; -2.0; -4.0|];
      [|-5.0; -4.0; -3.0; -3.0; -3.0; -3.0; -4.0; -5.0|]
    |]

  let bishop_table =
    [|
      [|-2.0; -1.0; -4.0; -1.0; -1.0; -4.0; -1.0; -2.0|]; 
      [|-1.0;  0.5;  0.0;  0.0;  0.0;  0.0;  5.0; -1.0|];
      [|-1.0;  1.0;  1.0;  1.0;  1.0;  1.0;  1.0; -1.0|];
      [|-1.0;  0.0;  1.0;  1.0;  1.0;  1.0;  0.0; -1.0|];
      [|-1.0;  0.5;  0.5;  1.0;  1.0;  0.5;  0.5; -1.0|];
      [|-1.0;  0.0;  0.5;  1.0;  1.0;  0.5;  0.0; -1.0|];
      [|-1.0;  0.0;  0.0;  0.0;  0.0;  0.0;  0.0; -1.0|];
      [|-2.0; -1.0; -1.0; -1.0; -1.0; -1.0; -1.0; -2.0|]
    |]

  let king_table =
    [|
      [|  2.0;  3.0;  1.0;  0.0;  0.0;  1.0;  3.0;  2.0|];
      [|  2.0;  2.0;  0.0;  0.0;  0.0;  0.0;  2.0;  2.0|];
      [| -1.0; -2.0; -2.0; -2.0; -2.0; -2.0; -2.0; -1.0|];
      [| -2.0; -3.0; -3.0; -4.0; -4.0; -3.0; -3.0; -2.0|];
      [| -3.0; -4.0; -4.0; -5.0; -5.0; -4.0; -4.0; -3.0|];
      [| -3.0; -4.0; -4.0; -5.0; -5.0; -4.0; -4.0; -3.0|];
      [| -3.0; -4.0; -4.0; -5.0; -5.0; -4.0; -4.0; -3.0|];
      [| -3.0; -4.0; -4.0; -5.0; -5.0; -4.0; -4.0; -3.0|]
    |]

  let rook_table =
    [|
      [|  0.0;  0.0;  0.0;  0.5;  0.5;  0.0;  0.0;  0.0|];
      [| -0.5;  0.0;  0.0;  0.0;  0.0;  0.0;  0.0; -0.5|];
      [| -0.5;  0.0;  0.0;  0.0;  0.0;  0.0;  0.0; -0.5|];
      [| -0.5;  0.0;  0.0;  0.0;  0.0;  0.0;  0.0; -0.5|];
      [| -0.5;  0.0;  0.0;  0.0;  0.0;  0.0;  0.0; -0.5|];
      [| -0.5;  0.0;  0.0;  0.0;  0.0;  0.0;  0.0; -0.5|];
      [|  0.5;  1.0;  1.0;  1.0;  1.0;  1.0;  1.0;  0.5|];
      [|  0.0;  0.0;  0.0;  0.0;  0.0;  0.0;  0.0;  0.0|]
    |]

  let queen_table =
    [|
      [| -2.0; -1.0; -1.0; -0.5; -0.5; -1.0; -1.0; -2.0|];
      [| -1.0;  0.0;  0.0;  0.0;  0.0;  0.0;  0.0; -1.0|];
      [| -1.0;  0.0;  0.5;  0.5;  0.5;  0.5;  0.0; -1.0|];
      [|  0.0;  0.0;  0.5;  0.5;  0.5;  0.5;  0.0;  0.0|];
      [|  0.0;  0.0;  0.5;  0.5;  0.5;  0.5;  0.0;  0.0|];
      [| -1.0;  0.0;  0.5;  0.5;  0.5;  0.5;  0.0; -1.0|];
      [| -1.0;  0.0;  0.0;  0.0;  0.0;  0.0;  0.0; -1.0|];
      [| -2.0; -1.0; -1.0; -0.5; -0.5; -1.0; -1.0; -2.0|];
    |]



  let get_table piece = 
    match piece with
    | Pawn   -> pawn_table
    | Knight -> knight_table
    | Bishop -> bishop_table
    | Rook   -> rook_table
    | Queen  -> queen_table
    | King   -> king_table

  let piece_table piece = 
    match piece with
    | Pawn   -> 10
    | Knight -> 30
    | Bishop -> 30
    | Rook   -> 50
    | Queen  -> 90
    | King   -> 900

  let negate = function
    | White -> Black 
    | Black -> White

  let flip = List.map (fun (a,b) -> (b,a))

  let value_of_position piece color = 
    let pos = snd piece in
    let table = get_table (fst piece).pc in
    match color with 
    | ChessPiece.White -> table.(fst pos).(snd pos)
    | ChessPiece.Black -> table.(7-(fst pos)).(7-(snd pos))

  (**Takes a board state and the color of the pieces being evaluated and return
     a number totalling the value of 
     all of the positions of the given colors pieces*)
  let eval_positions_of_pieces board color =  
    let list_of_pieces = List.filter (fun (k,v) -> k.c = color) board in 
    let pieces_mapped_to_piece_positions = 
      List.map (fun p -> 
          value_of_position p color) list_of_pieces in
    List.fold_right (fun f l -> f +. l) pieces_mapped_to_piece_positions 0.0

  (**Takes a board state and the color of the pieces being evaluated and return
     a number totalling the value of the given colors pieces*)
  let eval_piece_on_board board color = 
    let list_of_pieces = List.filter (fun (k,v) -> k.c = color) board in 
    List.fold_right (fun (k,v) l -> piece_table k.pc + l) list_of_pieces 0

  (**Takes a board state, the moves that created that board,
     and the color of the player who wants the board evaluated and returns 
     a tuple: the first value is the total value of the evaluation of the board, 
     and the second is the move that led to that evaluation*)
  let eval_board board m c = 
    let own_piece_val = eval_piece_on_board board c in
    let oppo_piece_val = 0 - eval_piece_on_board board (negate c)in
    let own_pos_val = eval_positions_of_pieces board c in
    let oppo_pos_val = 0.0 -. eval_positions_of_pieces board (negate c) in
    let total_board_score = 
      (float_of_int own_piece_val) +. (float_of_int oppo_piece_val) +. own_pos_val +. oppo_pos_val in
    total_board_score, m


  let get_board_from_move (r: (G.t, string)result) = 
    match r with 
    | Ok g  -> g
    | _ -> failwith "uh oh"


  (**Takes a board state, a list of possible moves,
     and the color of the player who wants the board evaluated and returns 
     a list of tuples: the first value is the total value of the evaluation 
     of the board for the given move, 
     and the second is the move that led to that evaluation*)
  let rec eval_list_of_moves board l c game =
    match l with 
    | [] -> []
    | h::t -> 
      let board_and_score = 
        eval_board (
          (get_board_from_move (G.move game c h)) 
          |> G.board
          |> ChessBoard.get_all 
          |> flip) h c in
      board_and_score :: (eval_list_of_moves board t c game) 

  let rec best_possible_score (l:(float * G.P.move) list) = 
    match l with 
    | [] -> (-9999999.0, {src = (1,1); dest = (1,1)})
    | (f,s)::t -> 
      if f > fst (best_possible_score t) then 
        (f,s) 
      else 
        best_possible_score t

  let rec possible_boards l b game c = 
    failwith "unimplimented"


  let eval_moves board c game = 
    let list_of_moves = G.possible_moves game c in 
    let evaluated_boards = eval_list_of_moves board list_of_moves c game in
    best_possible_score evaluated_boards


  let rec mini_max_helper func l maxing c depth game = 
    match l with
    | [] -> 0.0,{src = (0,0); dest = (0,0)}
    | h::t -> let h_res = 
                (minimax 
                   ((get_board_from_move (G.move game c h)) 
                    |> G.board
                    |> ChessBoard.get_all 
                    |> flip))
                  (depth-1) (not maxing) c game in
      let recursive_result = (mini_max_helper func t maxing c depth game) in
      if func (fst h_res) (fst recursive_result) then h_res else recursive_result

  and

    minimax board depth trying_to_max c game : float * P.move = 
    if depth = 2 then
      (eval_moves board c game)
    else 
      let list_of_boards = G.possible_moves game c in
      if trying_to_max then 
        mini_max_helper (>) list_of_boards true c depth game
      else 
        mini_max_helper (<) list_of_boards false c depth game



  let compute_best_move current_board c game : P.move = 
    snd (minimax current_board 2 true c game)

  let update _ g = 
    Some (compute_best_move (G.board g |> ChessBoard.get_all |> flip) P.Black g)

  let get_command = function
    | None -> failwith "Please update your agent first"
    | Some mv -> mv

  let init = None

end
