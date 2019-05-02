open Board
open Piece

module type Game = sig
  type t

  module Board : Board
  
  module P : Piece

  type check_status =
    | Safe
    | Check
    | CheckMate

  val init : (P.t * (int * int)) list -> t

  val history : t -> P.move list
  
  val status : t -> P.color -> check_status

  val display_board : t -> unit

  val move : t -> P.color -> P.move -> (t, string) result

  val possible_moves : t -> P.color -> P.move list

  val board : t -> P.t Board.t

end

module type GameMaker =
    functor (Brd : Board) ->
      Game with module Board = Brd and module P = ChessPiece
