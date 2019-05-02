open Piece
open Game

module type Agent = sig
  type t
  module P : Piece
  module G : Game

  val init : t
  val update : t -> G.t -> t
  val get_command : t -> P.move

end
