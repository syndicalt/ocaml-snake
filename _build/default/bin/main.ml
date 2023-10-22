open Graphics

type direction = Up | Down | Left | Right
type point = int * int

type game_state = {
  snake : point list;
  food : point;
  board_size : int;
  cell_size : int;
  current_dir : direction;
}

let move_point point dir =
  let (x, y) = point in
  match dir with
  | Up -> (x, y + 1)
  | Down -> (x, y - 1)
  | Left -> (x - 1, y)
  | Right -> (x + 1, y)

let generate_food snake board_size =
  let rec aux () =
    let x = Random.int board_size in
    let y = Random.int board_size in
    if List.mem (x, y) snake then aux ()
    else (x, y)
  in
  aux ()

  let move_snake state =
    let new_head = move_point (List.hd state.snake) state.current_dir in
    if new_head = state.food then
      let new_food = generate_food (new_head :: state.snake) state.board_size in
      { state with snake = new_head :: state.snake; food = new_food }
    else
      let snake_without_tail = List.rev (List.tl (List.rev state.snake)) in
      { state with snake = new_head :: snake_without_tail }
  

let game_over state =
  let (hx, hy) = List.hd state.snake in
  hx < 0 || hy < 0 || hx >= state.board_size || hy >= state.board_size || List.mem (hx, hy) (List.tl state.snake)

let draw_square x y cell_size color =
  set_color color;
  fill_rect (x * cell_size) (y * cell_size) cell_size cell_size

let draw_board state =
  clear_graph ();
  List.iter (fun (x, y) -> draw_square x y state.cell_size green) state.snake;
  let (fx, fy) = state.food in
  draw_square fx fy state.cell_size red;
  synchronize ()

let handle_input state =
  let event = wait_next_event [Key_pressed] in
  if event.keypressed then
    match event.key with
    | 'w' -> if state.current_dir != Down then { state with current_dir = Up } else state
    | 's' -> if state.current_dir != Up then { state with current_dir = Down } else state
    | 'a' -> if state.current_dir != Right then { state with current_dir = Left } else state
    | 'd' -> if state.current_dir != Left then { state with current_dir = Right } else state
    | 'q' -> close_graph (); exit 0
    | _ -> state
  else
    state

let rec game_loop state =
  if game_over state then begin
    draw_board state;
    moveto (size_x () / 4) (size_y () / 2);
    draw_string "Game over!";
    ignore (read_key ())
  end else begin
    draw_board state;
    let new_state = handle_input state in
    game_loop (move_snake new_state)
  end

let () =
  Random.self_init ();
  let board_size = 20 in
  let cell_size = 30 in
  open_graph (Printf.sprintf " %dx%d" (board_size * cell_size) (board_size * cell_size));
  auto_synchronize false;
  let initial_state = {
    snake = [(board_size / 2, board_size / 2)];
    food = generate_food [(board_size / 2, board_size / 2)] board_size;
    board_size;
    cell_size;
    current_dir = Up;
  } in
  game_loop initial_state
