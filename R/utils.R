saveFigure <- function(filename, graph, width = 20, height = 10) {

  p1 <- file.path("figures", filename)
  if (!dir.exists("figures"))
    dir.create("figures")

  if (getOption("writeFiguresToFile", FALSE)) {
    pdf(p1, width = width, height = height)
    print(graph)
    dev.off()
  }
}

col2hex <- function(cname) {
  # definition from gtools::col2hex
  colMat <- col2rgb(cname)
  rgb(red = colMat[1, ]/255, green = colMat[2, ]/255, blue = colMat[3, ]/255)
}

writeTable <- function(x, file) {
  # small wrapper around write.csv to save tables in a way that the latex package pgfplotstable
  # can easily read them

  p1 <- file.path("tables", file)
  if (!dir.exists("tables"))
    dir.create("tables")

  if (getOption("writeTablesToFile", FALSE))
    write.csv(x, p1, row.names = FALSE, quote = FALSE)
}

# colors
cols <- colorspace::qualitative_hcl(6)
colsBaseline <- cols[1:3]
colsExperim  <- cols[4:6]
