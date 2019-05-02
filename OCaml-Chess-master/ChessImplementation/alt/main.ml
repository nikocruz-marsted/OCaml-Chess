open ChessGame
open ChessBoard
open ChessAgent
open ChessPiece

module CG = ChessGame.Make(ChessBoard)

module White = ChessAgent
module Black = ChessAgent

let rec white_move g w = 
  print_string "\n\nWhite's Turn\n\n";
  CG.display_board g;
  let w' = White.update w g in
  match White.get_command w' |> CG.move g ChessPiece.White with
    | Ok g' -> g', w'
    | Error str -> print_endline str; white_move g w'

let rec black_move g b = 
  print_string "\n\nBlack's Turn\n\n";
  CG.display_board g;
  let b' = Black.update b g in
  match Black.get_command b' |> CG.move g ChessPiece.Black with
    | Ok g' -> g', b'
    | Error str -> print_endline str; black_move g b'

let rec loop g w b =
  let g', w' = white_move g w in
  begin
    match CG.status g' ChessPiece.Black with
    | CheckMate ->
      print_string "\n\nCheckMate. White Wins!!\n\n";
      exit 0
    | Check ->
      print_string "\n\nBlack is in Check\n\n"
    | _ -> ()
  end;
  let g'', b' = black_move g' b in
  begin
    match CG.status g'' ChessPiece.White with
    | CheckMate ->
      print_string "\n\nCheckMate. Black Wins!!\n\n";
      exit 0
    | Check ->
      print_string "\n\nWhite is in Check\n\n"
    | _ -> ()
  end;
  loop g'' w' b'

(** [main ()] starts a new game of chess. *)
let main () =
  ANSITerminal.(print_string [blue]
                  "\nWelcome to the 3110 version of chess!\n");
  print_endline "Type help to see available commands.\n";
  let _ = loop (CG.init ChessPiece.init) White.init Black.init
  in ()

(* Start the game. *)
let () = main ()
