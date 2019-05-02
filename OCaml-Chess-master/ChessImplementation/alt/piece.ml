module type Piece = sig

  type move
  type piece
  type color

  type t

  val init : (t * (int * int)) list
  val initTest1 : (t * (int * int)) list
  val initCastleTest : (t * (int * int)) list
  val initPassantTest : (t * (int * int)) list
  val print_piece : t -> unit

end
