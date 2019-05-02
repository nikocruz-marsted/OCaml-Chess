(** Implements a piece in the game of chess. *)

(** [color] is a variant of all the players participating
 * in the game. *)
type color =
  | Black
  | White

(** [piece] is a variant with all the different types
 * of chess pieces. *)
type piece = 
  | King
  | Queen
  | Bishop
  | Knight
  | Rook
  | Pawn

(** [move] is the type of a move for a piece. *)
type move = { src : (int * int);  dest : (int * int) }

(** [t] is the type of a chess piece. *)
type t = {
  c : color;
  pc : piece;
  not_moved : bool
}

(** [init] is a list of pairs containing chess pieces
 *  and their initial starting positions on a chess board. *)
val init : (t * (int * int)) list

(** [initTest1] returns a list of pairs containing chess pieces
 *  and their initial starting positions in test1. *)
val initTest1 : (t * (int * int)) list


val initCastleTest : (t * (int * int)) list

val initPassantTest : (t * (int * int)) list

(** [print_piece p] prints the chess piece [p] according to
 *  its type. *)
val print_piece : t -> unit
