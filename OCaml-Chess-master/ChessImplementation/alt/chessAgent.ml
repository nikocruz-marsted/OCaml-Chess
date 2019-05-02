open ChessPiece
open Agent
open ChessGame
open ChessBoard

module ChessAgent : 
  Agent with module P = ChessPiece and 
    module G = ChessGame.Make(ChessBoard) = struct

  module P = ChessPiece
  module G = ChessGame.Make(ChessBoard)

  type t = P.move option

  (** The type [command] represents a player command that consists of
      a verb and possibly a tuple of arguments. *)
  type command =
    | Move of P.move
    | Help
    | Quit

  (** Raised when an empty command is parsed. *)
  exception Empty

  (** Raised when attempting to parse a malformed command. *)
  exception Malformed

  (** [cat_if_not_empty str acc] checks if a string is empty, and if not,
      appends it to the given list of strings.
      Examples: 
      - [cat_if_not_empty "" ["a"; "b"]] is [["a"; "b"]]
      - [cat_if_not_empty "c" ["a"; "b"]] is [["c"; "a"; "b"]]
  *)
  let cat_if_not_empty str acc =
    if str = "" then acc else str::acc

  (** Helper function to convert the letter in [coord] to the index of
    a column in the board. *)
  let conv_letter coord =
  ((String.get coord 0) |> Char.uppercase_ascii |> Char.code) - 65

  (** Helper function to process the number in [coord]. *)
  let conv_number coord = String.sub coord 1 1 |> int_of_string_opt

  let parse_singleton s = 
    match String.lowercase_ascii s with
    | "quit" -> Quit
    | "help" -> Help
    | _ -> raise Malformed

  let parse_complex lst =
    match List.map String.lowercase_ascii lst with
    | h::coord1::goto::coord2::[] when h = "move" && goto = "to" -> 
        let letter1 = conv_letter coord1 in
        let number1 = conv_number coord1 in
        let letter2 = conv_letter coord2 in
        let number2 = conv_number coord2 in
        begin
          match number1, number2 with
          | Some n1, Some n2 -> 
              Move ({ src = (n1 - 1, letter1); dest = (n2 - 1, letter2) })
          | _ -> raise Malformed
        end
    | _ -> raise Malformed

  (** [parse str] parses an input of the form "move <Letter><Number>
      to <Letter><Number>", "quit", or "help" into a [command].
      Examples:
      - [parse "    move   A2  to C4    "] is [Move (6, 0, 4, 2)].
      - [parse "help"] is [Help].
      - [parse "quit"] is [Quit].

      Raises: [Empty] if [str] is the empty string or contains only
      whitespace.
      Raises: [Malformed] if the command is malformed; that is, not one of
      the forms listed above. *)
  let parse s =
    let split_s = String.split_on_char ' ' s in
    let nosp_s = List.map String.trim split_s in
    let non_empty_list = List.fold_right cat_if_not_empty nosp_s [] in
    match non_empty_list with
    | [] -> raise Empty
    | h::[] -> parse_singleton h
    | lst -> parse_complex lst

  let help_msg =
    [
      "List of Commands\n";
      "help : print information about available commands\n";
      "move <Letter><Number> to <Letter><Number> : perform the specified move\n";
      "quit : exit the game\n\n";
    ]

  let user_input () =
    let _ = print_string "\n> " in
    read_line ()

  let rec input_loop () = 
    try begin 
      match user_input () |> parse with
      | Move mv -> mv
      | Quit -> print_string "Goodbye.\n"; exit 0
      | Help ->
        let _ = List.map print_string help_msg in
          input_loop ()
    end with
    | Empty -> input_loop ()
    | Malformed -> 
        print_string "Please enter a valid command.\n"; input_loop ()

  let init = None

  let update _ _ = Some (input_loop ())

  let get_command a =
    match a with
    | Some mv -> mv
    | None -> failwith "Please update the agent first"

end
