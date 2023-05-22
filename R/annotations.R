setClass(
  "Annotation",
  slots=list(
    start="numeric",
    end="numeric",
    source="character",
    value="character"
  ),
  prototype = list(
    start = 0,
    end = 0,
    source = NA_character_,
    value = NA_character_
  )
)
