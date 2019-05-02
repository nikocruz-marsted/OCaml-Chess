open Game
open Board
open ChessPiece

module Make : GameMaker =
  functor (Brd : Board) -> struct
    module Board = Brd
    module P = ChessPiece

    type t = {
      board : P.t Board.t;
      history : P.move list;
    }

    type check_status =
      | Safe
      | Check
      | CheckMate

    let history g = g.history

    let init initPieces = {
      board = List.fold_left 
        (fun a (p, crd) -> Brd.set_piece a crd p) Board.empty initPieces;
      history = []
    }

    (** [diff x y] is the absolute value of x - y. *)
    let diff x y = x - y |> abs

    (** [sign x] returns -1 if x is negative, 1 if x is positive,
        otherwise 0. *)
    let sign x = if x < 0 then -1 else if x > 0 then 1 else 0

    (** [move_dir c] converts color c into an int. *)
    let move_dir c = if c = White then 1 else -1

    (** [is_linear (x1,y1) (x2,y2)] checks if two points are on the
        same line. *)
    let is_linear (r1, c1) (r2, c2) =
      r1 = r2 || c1 = c2

    (** [is_diagonal (x1,y1) (x2,y2)] checks if two points are diagonal. *)
    let is_diagonal (r1, c1) (r2, c2) =
      diff r1 r2 = diff c1 c2

    (** [is_lmove (x1,y1) (x2,y2)] checks if two points are in a 
     *  "knights L" away from each other. *)
    let is_lmove (r1, c1) (r2, c2) =
      let rdiff = diff r1 r2 in
      let cdiff = diff c1 c2 in
      (rdiff = 1 && cdiff = 2) || (rdiff = 2 && cdiff = 1)

    (** [is_one_space_move (x1,y1) (x2,y2)] checks if two points are
     *  one space away from one another. *)
    let is_one_space_move (r1, c1) (r2, c2) = 
      diff r1 r2 <= 1 && diff c1 c2 <= 1

    (** [step_towards (x1,y1) (x2,y2)] steps from one pos to another. *)
    let step_towards (r1, c1) (r2, c2) =
      (r1 + sign (r2 - r1), c1 + sign (c2 - c1))

    (** [lin_range_inclusive pos1 pos2] is a list of positions 
     *  in a range, inclusive. *)
    let lin_range_inclusive pos1 pos2 =
      let rec lri' pos acc =
        if pos = pos2 then pos::acc else
          let nxt = step_towards pos pos2 in  lri' nxt (pos::acc) in
      lri' pos1 [] |> List.rev

    (** [lin_range_exclusive pos1 pos2] is a list of positions
     *  in a range, exclusive. *)
    let lin_range_exclusive pos1 pos2 =
      if pos1 = pos2 || step_towards pos1 pos2 = pos2 then
        []
      else
        lin_range_inclusive 
          (step_towards pos1 pos2) (step_towards pos2 pos1)


    (** [clear_path b pos1 pos2] determines whether there is a clear
        path between pos1 and pos2; that is, whether there are no pieces
        lying on that path.
    *)
    let clear_path b pos1 pos2 =
      let range = lin_range_exclusive pos1 pos2 in
      List.fold_left 
        (fun acc pos -> acc && (not (Board.is_occupied b pos))) true range

    (** [is_pawn_movement board piece (x1,y1) (x2,y2)] checks if a move is
     *  a valid pawn movement. *)
    let is_pawn_movement b p (r1, c1) (r2, c2) = 
      let dir = move_dir p.c in
      match Board.get_piece b (r2, c2) with
      | Some p' when p'.c <> p.c -> 
          diff c1 c2 = 1 && r2 - r1 = dir
      | _ ->
          c1 = c2 && (r2 - r1 = dir || (p.not_moved && r2 - r1 = 2 * dir)) 
          && clear_path b (r1,c1) (r2,c2)

    (** [nums x y] is a helper to get a list of numbers from x to y. *)
    let nums x y = 
      let dir = sign (y - x) in
      let rec nums' z acc = 
        if x = z then z::acc else nums' (z - dir) (z::acc) in
      nums' y []

    (** [square_range (x1,y1) (x2,y2)] is a helper to get a list of coords
     *  in a coord range. *)
    let square_range (r1, c1) (r2, c2) = 
      let rowlst = nums r1 r2 in
      let collst = nums c1 c2 in
      let rec sr' rl cl accrow acc = 
        begin
          match rl, cl with
          | [], _ -> acc
          | r::tr, [] -> sr' tr collst [] ((List.rev accrow)::acc)
          | r::tr, c::tc -> sr' rl tc ((r, c)::accrow) acc
        end in sr' rowlst collst [] [] |> List.rev

    (** [move_shape board piece pos1 pos2] checks if a certain "move shape"
     *  is valid for a given piece. *)
    let move_shape b p pos1 pos2 =
      let clear = clear_path b pos1 pos2 in 
      match p.pc with
      | King -> is_one_space_move pos1 pos2
      | Queen -> (is_linear pos1 pos2 || is_diagonal pos1 pos2) && clear
      | Bishop -> is_diagonal pos1 pos2 && clear
      | Knight -> is_lmove pos1 pos2
      | Rook -> is_linear pos1 pos2 && clear
      | Pawn -> is_pawn_movement b p pos1 pos2

    (** [valid_move board piece pos1 pos2] checks if a move from
     *  [pos1] to [pos2] is valid. *)
    let valid_move b p pos1 pos2 =
      pos1 <> pos2 && 
      match Board.get_piece b pos2 with
      | None -> move_shape b p pos1 pos2
      | Some pdest when pdest.c <> p.c -> move_shape b p pos1 pos2
      | _ -> false

    (** [king_pos g c] is the position of a king of color [c]
     *  in game [g]. *)
    let king_pos game color =
          List.fold_left
            begin
              fun a (pos, piece) ->
                match a with
                | Some king_pos -> Some king_pos
                | None -> if piece.pc = King && piece.c = color
                  then Some pos else None
            end
            None (Board.get_all game.board)

    (** [attack_locations dest game] determines the list of all pieces
     *  that could attack a certain dest location. *)
    let attack_locations dest g = 
      let pieces = Board.get_all g.board in
      List.fold_left begin
        fun acc (pos, pc) -> 
          if valid_move g.board pc pos dest then
            { src = pos; dest = dest }::acc
          else
            acc
      end [] pieces

    (** [in_danger attack_pos game] checks if the given position
     *  is in danger. *)
    let in_danger attack_pos g =
      attack_locations attack_pos g <> []

    (** [in_check game color] checks if a king of color [color] 
     *  is in check in [game]. *)
    let in_check game color =
          let pos = match king_pos game color with
            | None -> failwith "no king"
            | Some p -> p in
          in_danger pos game

    let display_board g =
      let sz = Board.size in
      let sq = 
        square_range (0, 0) (sz - 1, sz - 1) |> List.rev |> List.concat in
      let show (a, b) = begin
        let _ = print_string 
          (if b = 0 then (string_of_int (a + 1)) ^ " " else "") in
        match Board.get_piece g.board (a, b) with 
        | Some pc -> 
            P.print_piece pc;if b = 7 then print_string "\n" else ();
        | None -> 
            print_string (if a mod 2 = b mod 2 then "□ " else "■ ");
            if b = Board.size - 1 then print_string "\n" else ();
      end in
      List.iter show sq;
      print_string "  A B C D E F G H\n"

    (** [move_piece board piece pos1 pos2] is a new board with the piece at
     *  at [pos1] moved to [pos2]. *)
    let move_piece board piece pos1 pos2 =
      let b' = Board.remove_piece board pos2 in
      let b'' = Board.remove_piece b' pos1 in
      Board.set_piece b'' pos2 {piece with not_moved = false}

    let can_castle g pc (r1, c1) (r2, c2) =
      let rkpos = if c2 > c1 then (r1, Board.size - 1) else (r1, 0) in
      match Board.get_piece g.board rkpos with
      | Some pc' when pc.pc = P.King && pc.not_moved && pc'.not_moved -> 
          not (in_check g pc.c) && (r1 = r2) && (diff c1 c2 = 2) &&
            (clear_path g.board (r1, c1) (r2, c2))
      | _ -> false
        
      
    let do_castle b pc (r1, c1) (r2, c2) =
      let rkpos = if c2 > c1 then (r1, Board.size - 1) else (r1, 0) in
      match Board.get_piece b rkpos with
      | None -> failwith "No rook"
      | Some pc' -> 
          let b' = Board.remove_piece b rkpos in
          let b'' = Board.remove_piece b' (r1, c1) in
          let b3 = 
            Board.set_piece b'' (r2, c2) {pc with not_moved = false} in
          Board.set_piece b3
            (step_towards (r1, c1) (r2, c2)) {pc' with not_moved = false}

    let can_passant g pc (r1, c1) (r2, c2) =
      match Board.get_piece g.board (r1, c2) with
      | Some pc' when pc.c <> pc'.c ->
          begin
            let origin = (r2 + (move_dir pc.c), c2) in
            match g.history with
            | mv::_ -> mv = ({ src = origin; dest = (r1, c2) })
            | _ -> false
          end
      | _ -> false

    let do_passant b piece (r1, c1) (r2, c2) =
      let b' = Board.remove_piece b (r1, c2) in
      let b'' = Board.remove_piece b' (r1, c1) in
      Board.set_piece b'' (r2, c2) {piece with not_moved = false}

    (** [propose_move g c mv] checks if move [mv] is ok to make. *)
    let propose_move g c mv = 
      let (pos1, pos2) = mv.src, mv.dest in
      let board = g.board in
      let piece_opt = Board.get_piece g.board pos1 in
      match piece_opt with
      | None -> Error "No piece at location"
      | Some piece when piece.c <> c -> Error "Not your piece"
      | Some piece when valid_move board piece pos1 pos2 -> 
          Ok ({ board = move_piece board piece pos1 pos2; 
            history = mv::g.history })
      | Some piece when can_passant g piece pos1 pos2 -> 
          Ok ({ board = do_passant g.board piece pos1 pos2;
            history = mv::g.history })
      | Some piece when can_castle g piece pos1 pos2 ->
          Ok ({ board = do_castle g.board piece pos1 pos2;
            history = mv::g.history })
      | _ -> Error "Invalid Move"

    let move g c mv = 
      match propose_move g c mv with
      | Error str as e -> e
      | Ok g as ok-> if in_check g c then
        Error "Cannot leave the king in check"
      else
        ok

    (** [get_c_pieces g c] gets all pieces of a color [c] in game [g]. *)
    let get_c_pieces g c = 
      Board.get_all g.board |> List.filter begin
        fun (pos, p) -> p.c = c
      end 

    let possible_moves g c = 
      let sz = Board.size in
      let range = square_range (0, 0) (sz - 1, sz - 1) |> List.flatten in
      List.fold_left begin
        fun acc pos -> 
          let locs = attack_locations pos g in
          let valid_comms = List.fold_left begin 
            fun acc mv ->
              match move g c mv with
              | Ok _ -> mv::acc
              | Error _ -> acc
          end [] locs in
          List.concat [valid_comms; acc]
      end [] range

    let status g c =
      if possible_moves g c = [] then
        CheckMate
      else if in_check g c then
        Check
      else
        Safe

    let board g = g.board

  end
