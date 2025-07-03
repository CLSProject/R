create_color_palette <- function(colorpattern) {
  if (colorpattern == "Rainbow") {
    colorRampPalette(c(
      "darkred", "red", "orange", "yellow", "lightgreen",
      "green", "lightblue", "blue", "lavender",
      "purple"
    ))(100)
  } else if (colorpattern == "Heat") {
    colorRampPalette(c(
      "darkred", "red", "orangered", "darkorange", "orange",
      "gold", "yellow", "lightyellow"
    ))(100)
  } else if (colorpattern == "Topo") {
    colorRampPalette(c(
      "darkgreen", "green", "lightgreen", "lightblue",
      "blue", "darkblue"
    ))(100)
  } else {
    colorRampPalette(c("red", "white", "green"))(100)
  }
}
