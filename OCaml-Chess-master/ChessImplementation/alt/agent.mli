(** Represents an agent, or player, of the game. *)

open Piece
open Game

module type Agent = sig

  (** [t] is the type of the game agent. *)
  type t

  module P : Piece
  module G : Game

  (** [init] is the initial game agent at the start of a game. *)
  val init : t

  (** [update agent game] is a game with the new, passed move list. *)
  val update : t -> G.t -> t

  (** [get_command agent] gets the next command from the agent
      and returns it as a move. *)
  val get_command : t -> P.move

end
