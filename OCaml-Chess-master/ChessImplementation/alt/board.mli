(** Represents the board in a game of chess. *)

module type Board = sig

  type 'piece t

  (** [pos] is the type that holds position*)
  type pos = int * int

  (** [size] is the size of the chess board*)
  val size : int

  (** [get_piece board position] is an option of 
   * Some 'p that is in that position of the board
   * or None *)
  val get_piece : 'p t -> pos -> 'p option

  (** [set_piece board position 'p] is the game board
   * with the position set to 'p *)
  val set_piece : 'p t -> pos -> 'p -> 'p t

  (** [get_all board] is a list of pairs containing
   * position and 'p *)
  val get_all : 'p t -> (pos * 'p) list

  (** [remove_piece board position] is the game board
   * with the 'p at position removed *)
  val remove_piece : 'p t -> pos -> 'p t

  (** [empty] is an empty game board*)
  val empty : 'p t

  (** [is_occupied board position] is whether or not a 
   * position on board is occupied by a 'p *)
  val is_occupied : 'p t -> pos -> bool

end
