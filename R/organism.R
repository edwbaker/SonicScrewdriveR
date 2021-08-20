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
    is_transmitting="logical",
    do_terminate_run="logical",

    #Historical data
    x="numeric",
    y="numeric",
    d="numeric",

    #Final data
    path_length="numeric"
  )
)
