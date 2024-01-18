let box_width = 100
let box_height = 10
let box_color = Graphics.rgb 0 0 0
let box_pos_y = 10
let box_edge_coord mouse_x = 
    mouse_x - box_width / 2, 
    box_pos_y - box_height / 2, 
    mouse_x + box_width / 2, 
    box_pos_y + box_height / 2

let draw_box mouse_x = 
    Graphics.set_color box_color;
    let x1, y1, x2, y2 = box_edge_coord mouse_x in
    Graphics.fill_rect x1 y1 (x2 - x1) (y2 - y1)

let game_hello () = print_endline "Hello, Newtonoiders!"

