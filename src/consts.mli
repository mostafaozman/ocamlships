(**Consts Module consists of all the constants used in the game *)

val board_size : int
(** [board_size] an int the represents what size the square shaped board is
    going to be.contents board_size of 14 would mean it constructs a board of
    the size 14 x 14 *)

val carrier : int
(**[carrier] is the length of the carrier ships*)

val destroyer : int
(**[destroyer] is the length of the destroyer ships*)

val submarine : int
(**[submarine] is the length of the submarine ships*)

val patrol : int
(**[patrol] is the length of the patrol ships*)

val carrier_num : int
(**[carrier_num] is the number of the carrier ships present on the board*)

val destroyer_num : int
(**[destroyer_num] is the number of the destroyer ships present on the board*)

val submarine_num : int
(**[submarine_num] is the number of the submarine ships present on the board*)

val patrol_num : int
(**[patrol_num] is the number of the patrol ships present on the board*)

val total_ships : int
(**[total_ships] is the number of the total ships present on the board*)

val ship_num_lst : (int * int) list
(**[ship_num_lst] represents a tuple of a ship length and ship number for each
   ship.*)

val box_size : int
(**[box_size] representsthe pixel size of the drawn board*)

val box_off : int
(**[box_off] represents the pixel size of the drawn board*)

val background_llx : int
(**[background_llx] the lower left x coordinate of the black board*)

val background_lly : int
(**[background_lly] the lower left y coordinate of the black board*)

val background_length : int
(**[background_length] the length of the black of the black board by pixels*)

val background_tly : int
(**[background_tly] the top left y coordinate of the black board*)

val go_green : int
(**[go_green] the hex code for green*)

val quit_red : int
(**[quit_red] the hex code for red*)

val logo_wht : int
(**[logo_wht] the hex code for white*)

val ocean_blue : int
(**[ocean_blue] the hex code for blue*)

val piss_yellow : int
(**[piss_yellow] the hex code for yellow*)

val depression_grey : int
(**[depression_grey] the hex code for grey*)

val white : int
(**[white] the hex code for white*)

val purple : int
(**[purple] the hex code for purple*)

val intersect_weight : int
(**[intersect_weight] is how much the Hard AI prioritizes intersecting Hit cells
   in its calculations. *)

val ( +^+ ) : Buffer.t -> string -> unit
(**[a +^+ b] adds [b] to buffer [a]*)
