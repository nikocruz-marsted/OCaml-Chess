(** The main part of the game implementation. *)

open Game
open Board
open ChessPiece

(** [Make] makes a [Game] with the given board and chess pieces. *)
module Make : GameMaker
