setClass(
  "organism",
  representation(
    position="numeric",
    direction="numeric",
    speed="numeric",
    amplitude="numeric"
  )
)

setClass(
  "arena",
  representation(
    max.coord="numeric"
  )
)
