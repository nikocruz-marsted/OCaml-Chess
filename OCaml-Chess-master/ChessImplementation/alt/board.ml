module type Board = sig

  type 'piece t
  type pos = int * int

  val size : int

  val get_piece : 'p t -> pos -> 'p option
  val set_piece : 'p t -> pos -> 'p -> 'p t
  val get_all : 'p t -> (pos * 'p) list
  val remove_piece : 'p t -> pos -> 'p t
  val empty : 'p t
  val is_occupied : 'p t -> pos -> bool

end
