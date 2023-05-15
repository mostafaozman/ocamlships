let board_size = 10
let carrier = 5
let destroyer = 4
let submarine = 3
let patrol = 2
let carrier_num = 1
let destroyer_num = 1
let submarine_num = 2
let patrol_num = 1

let ship_num_lst =
  [
    (carrier, carrier_num);
    (destroyer, destroyer_num);
    (submarine, submarine_num);
    (patrol, patrol_num);
  ]

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
let purple = 0x3432a8
let intersect_weight = 10
let adjacent_weight = 10000
let ( +^+ ) a b = Buffer.add_string a b
let horiz = "Horizontal"
let vert = "Vertical"
