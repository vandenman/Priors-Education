saveFigure <- function(filename, graph, width = 20, height = 10) {

  p1 <- file.path("figures", filename)
  p2 <- file.path("paper", p1)

  pdf(p1, width = width, height = height)
  print(graph)
  dev.off()
  file.copy(p1, p2, overwrite = TRUE)

}
