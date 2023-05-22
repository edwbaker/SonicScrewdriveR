setClass(
  "organism",
  representation(
    position="numeric",
    direction="numeric",
    speed="numeric",
    amplitude="numeric",
    strategy="function",
    dutycycle="numeric",
    dutycycle_offset="numeric",
    directionbits="numeric",

    #Internals
    start_pos_random="logical",
    start_pos_grid="logical",
    is_transmitting="logical",
    do_terminate_run="logical",
    made_move="logical",

    #Historical data
    x="numeric",
    y="numeric",
    d="numeric",

    #Final data
    path_length="numeric"
  )
)
