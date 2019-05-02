(** A player in the game of chess. *)

open Agent
open ChessPiece
open ChessGame
open ChessBoard

module ChessAgent : 
  Agent with module P = ChessPiece and module G = ChessGame.Make(ChessBoard)
