# probably yes!
fitmultidist <- function(x) {

  fg <- fitdist(x, "gamma")
  fsht <- fitdist(x, "halft.scaled", start = list(df = 1, mean = mean(x), sd = sd(x), ncp = 0))
  fln <- fitdist(x, "lnorm")

  return(list(fg = fg, fsht = fsht, fln = fln))

}

panelplot <- function(x) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = c(2, 2))
  denscomp(fitted)
  ppcomp(fitted)
  cdfcomp(fitted)
  qqcomp(fitted)
  return(invisible())
}
