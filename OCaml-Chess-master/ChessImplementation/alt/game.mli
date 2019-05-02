(** Contains functions for manipulating a game of chess. *)

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

  (** [init] is an initial game. *)
  val init : (P.t * (int * int)) list -> t

  (** [history g] is a list of previous moves in the game. *)
  val history : t -> P.move list
  
  (** [status g c] is the status of the king of color [c]. *)
  val status : t -> P.color -> check_status

  (** [display_board g] displays the current state of the game. *)
  val display_board : t -> unit

  (** [move g c mv] is the game resulting from the player of color [c]
      making move [mv] in game [g].
      Raises: Error if the move is invalid. *)
  val move : t -> P.color -> P.move -> (t, string) result 

  (** [possible_moves g c] returns the valid moves that can be
      taken by color [c]. *)
  val possible_moves : t -> P.color -> P.move list

  val board : t -> P.t Board.t
end

(** A [GameMaker] is a functor that makes a [Game]
    out of modules representing pieces and the board. *)
module type GameMaker =
    functor (Brd : Board) ->
      Game with module Board = Brd and module P = ChessPiece
