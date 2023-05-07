let board_size = 10
let carrier = 5
let destroyer = 4
let submarine = 3
let patrol = 2
let carrier_num = 1
let destroyer_num = 1
let submarine_num = 2
let patrol_num = 3
let total_ships = 7

let ship_num_arr =
  [|
    (carrier, carrier_num);
    (destroyer, destroyer_num);
    (submarine, submarine_num);
    (patrol, patrol_num);
  |]

let box_size = 58
let box_off = 5
let background_llx = 20
let background_lly = 120
let background_length = 634
let background_tly = background_lly + background_length
let go_green = 0x1B512D
let quit_red = 0x94241a
let logo_wht = 0xF7F7F2
let ocean_blue = 0x7BB5FF
let piss_yellow = 0xFFBF00
let depression_grey = 0xABB0B8
let white = 0xFFFFFF
let intersect_weight = 2
