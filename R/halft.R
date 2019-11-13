# WARNING: exp(df) is used to enforce positivity!
dhalft <- function(x, df, ncp, a = 0, b = Inf, log = FALSE, expdf = TRUE) {
  if (expdf)
    df <- exp(df)

  ans <- dt(x, df, ncp, TRUE) - pt(a, df, ncp, lower.tail = FALSE, log.p = TRUE)
  if (log) return(ans)
  return(exp(ans))
}
phalft <- function(x, df, ncp, a = 0, b = Inf, log = FALSE, expdf = TRUE) {
  if (expdf)
    df <- exp(df)

  nx <- length(x)
  ans <- numeric(nx)
  if (!nx)
    return(ans)

  idx <- x > 0
  idxNaN <- is.na(idx)
  idx[idxNaN] <- TRUE
  ans[!idx] <- -Inf
  ans[idxNaN] <- NaN
  ans[idx] <- log(pt(x[idx], df, ncp) - pt(a, df, ncp)) - pt(a, df, ncp, lower.tail = FALSE, log.p = TRUE)

  if (log) return(ans)
  return(exp(ans))
}
qhalft <- function(x, df, ncp, a = 0, b = Inf, log = FALSE, expdf = TRUE) {
  if (expdf)
    df <- exp(df)

  K <- 1 - pt(a, df, ncp)
  u <- x * K + pt(a, df, ncp)
  return(qt(u, df = df, ncp = ncp))
}
rhalft <- function(n, df, ncp, a = 0, b = Inf, expdf = TRUE) {
  if (expdf)
    df <- exp(df)
  u <- runif(n)
  return(qhalft(x = u, df = df, ncp = ncp, a = a,  b = b, expdf = FALSE))
}

# TODO: remove dependency on this package by manual implementation of the functions!
dt.scaled <- metRology::dt.scaled
pt.scaled <- metRology::pt.scaled
qt.scaled <- metRology::qt.scaled
rt.scaled <- metRology::rt.scaled


dhalft.scaled <- function(x, df, mean = 0, sd = 1, ncp, a = 0, b = Inf, log = FALSE, expdf = TRUE) {
  if (expdf)
    df <- exp(df)

  ans <- dt.scaled(x, df, mean, sd, ncp, TRUE) - log(1 - pt.scaled(a, df, mean, sd, ncp))
  if (log) return(ans)
  return(exp(ans))
}
phalft.scaled <- function(x, df, mean = 0, sd = 1, ncp, a = 0, b = Inf, log = FALSE, expdf = TRUE) {
  if (expdf)
    df <- exp(df)

  nx <- length(x)
  ans <- numeric(nx)
  if (!nx)
    return(ans)

  idx <- x > 0
  idxNaN <- is.na(idx)
  idx[idxNaN] <- TRUE
  ans[!idx] <- -Inf
  ans[idxNaN] <- NaN
  # lower.tail argument of pt.scaled is not implemented!
  ans[idx] <- log(pt.scaled(x[idx], df, mean, sd, ncp) - pt.scaled(a, df, mean, sd, ncp)) - log(1 - pt.scaled(a, df, mean, sd, ncp))

  if (log) return(ans)
  return(exp(ans))
}
qhalft.scaled <- function(x, df, mean = 0, sd = 1, ncp, a = 0, b = Inf, log = FALSE, expdf = TRUE) {
  if (expdf)
    df <- exp(df)

  K <- 1 - pt.scaled(a, df, mean, sd, ncp)
  u <- x * K + pt.scaled(a, df, mean, sd, ncp)
  return(qt.scaled(u, df = df, mean = mean, sd = sd, ncp = ncp))
}
rhalft.scaled <- function(n, df, mean = 0, sd = 1, ncp, a = 0, b = Inf, expdf = TRUE) {
  if (expdf)
    df <- exp(df)
  u <- runif(n)
  return(qhalft.scaled(x = u, df = df, ncp = ncp, a = a,  b = b, expdf = FALSE))
}


qhalft.scaled(phalft.scaled(.5, 1, 1, 1, 1, expdf = FALSE), 1, 1, 1, 1, expdf = FALSE)

qhalft(phalft(.5, 1, 1, expdf = FALSE), 1, 1, expdf = FALSE)
