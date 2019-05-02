open Board

module ChessBoard : Board = struct
  type order = LT | EQ | GT
  type pos = int * int

  module Pair = struct 
    type t = pos 
    let compare (r1, c1) (r2, c2) = 
      if r1 < r2 || (r1 = r2 && c1 < c2) then
        -1
      else if r1 > r2 || (r1 = r2 && c1 > c2) then
        1
      else
        0
  end

  module M = Map.Make(Pair)

  type 'p t = 'p M.t

  let size = 8

  let valid_pos (r, c) = 
    0 <= r && r < size && 0 <= c && c < size

  let check_coord coord = if valid_pos coord then coord else failwith "OOB"

  let get_piece b coord = M.find_opt coord b

  let set_piece b coord p = 
    let coord = check_coord coord in
    match get_piece b coord with
    | Some _ -> failwith "Space Occupied"
    | None -> M.add coord p b

  let remove_piece b coord =   
    let coord = check_coord coord in
    M.remove coord b

  let get_all b = M.bindings b

  let empty = M.empty

  let is_occupied b coord = M.mem (check_coord coord) b

end
