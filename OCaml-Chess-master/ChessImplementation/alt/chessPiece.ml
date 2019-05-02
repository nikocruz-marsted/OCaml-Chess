type color =
  | Black
  | White

type piece = 
  | King
  | Queen
  | Bishop
  | Knight
  | Rook
  | Pawn

type move = { 
  src : (int * int);  
  dest : (int * int);
}

type t = {
  c : color;
  pc : piece;
  not_moved : bool
}

let init =
  let make_t c pc = { c = c; pc = pc; not_moved = true } in
  let w = White in
  let b = Black in
  [
    make_t w Rook, (0, 0); make_t w Knight, (0, 1); 
    make_t w Bishop, (0, 2); make_t w Queen, (0, 3);
    make_t w King, (0, 4); make_t w Bishop, (0, 5); 
    make_t w Knight, (0, 6); make_t w Rook, (0, 7);
    make_t w Pawn, (1, 0); make_t w Pawn, (1, 1); 
    make_t w Pawn, (1, 2); make_t w Pawn, (1, 3); 
    make_t w Pawn, (1, 4); make_t w Pawn, (1, 5); 
    make_t w Pawn, (1, 6); make_t w Pawn, (1, 7); 
    make_t b Rook, (7, 0); make_t b Knight, (7, 1); 
    make_t b Bishop, (7, 2); make_t b Queen, (7, 3);
    make_t b King, (7, 4); make_t b Bishop, (7, 5); 
    make_t b Knight, (7, 6); make_t b Rook, (7, 7);
    make_t b Pawn, (6, 0); make_t b Pawn, (6, 1); 
    make_t b Pawn, (6, 2); make_t b Pawn, (6, 3); 
    make_t b Pawn, (6, 4); make_t b Pawn, (6, 5); 
    make_t b Pawn, (6, 6); make_t b Pawn, (6, 7); 
  ]

let initTest1 =
  let make_t c pc = { c = c; pc = pc; not_moved = true } in
  let w = White in
  let b = Black in
  [
    make_t w Rook, (0, 0); make_t w Knight, (0, 1); 
    make_t w Bishop, (0, 2); make_t w Queen, (0, 3);
    make_t w King, (0, 4); make_t w Bishop, (0, 5); 
    make_t w Knight, (0, 6); make_t w Rook, (0, 7);
    make_t b King, (7, 4);  
  ]

let initCastleTest = 
  let make_t c pc = { c = c; pc = pc; not_moved = true } in
  let w = White in
  [
    make_t w Rook, (0, 0);
    make_t w King, (0, 4); make_t w Rook, (0, 7);
  ]

let initPassantTest = 
  let make_t c pc = { c = c; pc = pc; not_moved = true } in
  let w = White in
  let b = Black in
  [
    make_t w King, (0, 4);
    make_t w Pawn, (1, 0); make_t w Pawn, (1, 1);
    make_t w Pawn, (1, 2); make_t w Pawn, (1, 3);
    make_t w Pawn, (1, 4); make_t w Pawn, (1, 5);
    make_t w Pawn, (1, 6); make_t w Pawn, (1, 7);
    make_t b King, (7, 4); 
    make_t b Pawn, (6, 0); make_t b Pawn, (6, 1);
    make_t b Pawn, (6, 2); make_t b Pawn, (6, 3);
    make_t b Pawn, (6, 4); make_t b Pawn, (6, 5);
    make_t b Pawn, (6, 6); make_t b Pawn, (6, 7);
  ]

let print_piece p =
  match p.c, p.pc with
  | Black, King -> print_string "♔ ";
  | Black,Queen -> print_string "♕ "
  | Black,Bishop -> print_string "♗ "
  | Black,Rook -> print_string "♖ "
  | Black,Knight -> print_string "♘ "
  | Black,Pawn -> print_string "♙ "
  | White,King -> print_string "♚ "
  | White,Queen -> print_string "♛ "
  | White,Bishop -> print_string "♝ "
  | White,Rook -> print_string "♜ "
  | White,Knight -> print_string "♞ "
  | White,Pawn -> print_string "♟ "
