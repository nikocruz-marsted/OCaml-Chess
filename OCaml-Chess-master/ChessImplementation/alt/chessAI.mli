(** An AI to play the game of chess. *)

open Agent
open ChessPiece
open ChessGame
open ChessBoard

module ChessAI :
  Agent with module P = ChessPiece and module G = ChessGame.Make(ChessBoard)
