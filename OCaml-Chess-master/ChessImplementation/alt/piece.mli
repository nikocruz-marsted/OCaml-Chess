(** Represents a piece in the game of chess. *)

module type Piece = sig

  (** [move] is the type of a move *)
  type move

  (** [piece] is the type of a piece*)
  type piece
  (** [color] is the type of a piece's color *)
  type color

  (** [t] is the type of a piece*)
  type t

  (** [init] is a list of pairs with intial pieces and their
   * positions on the game board*)
  val init : (t * (int * int)) list

  val initTest1 : (t * (int * int)) list

  val initCastleTest : (t * (int * int)) list

  val initPassantTest : (t * (int * int)) list

  (** [print_piece piece] will print the given chess piece*)
  val print_piece : t -> unit

end
