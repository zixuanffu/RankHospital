y <- Z$pdt$y
id <- Z$pdt$id
u <- 300
v <- 300
n <- length(y)
eps <- 1e-04
w <- rep(1, n)
wsum <- tapply(w, id, "sum")
dt2 <- Z$dt2
t <- tapply(w * y, id, "sum") / wsum
m <- tapply(y, id, "length")
r <- (m - 1) / 2
s <- (tapply(w * y^2, id, "sum") - t^2 * wsum) / (m - 1)
n <- length(s)
if (length(u) == 1) {
    u <- seq(min(t) - eps, max(t) + eps, length = u)
}
if (length(v) == 1) {
    v <- seq(min(s) - eps, max(s) + eps, length = v)
}
v <- v[v > 0]
pu <- length(u)
du <- rep(1, pu)
wu <- rep(1, n) / n
pv <- length(v)
dv <- rep(1, pv)
Av <- matrix(NA, n, pv)
for (i in 1:n) {
    for (j in 1:pv) {
        Av[i, j] <- dgamma(s[i], r[i], scale = v[j] / r[i])
    }
}
Av <- outer(Av, rep(1, pu))
Av <- aperm(Av, c(1, 3, 2))
Au <- dnorm(outer(outer(t, u, "-") * outer(sqrt(wsum), rep(
    1,
    pu
)), sqrt(v), "/"))
Auv <- Av * Au
A <- NULL
for (j in 1:pv) A <- cbind(A, Auv[, , j])
duv <- as.vector(kronecker(du, dv))
uv <- expand.grid(alpha = u, theta = v)
f <- KWDual(A, duv, wu, ...)
fuv <- f$f
g <- f$g
status <- f$status
r <- (m - 1) / 2
logK <- log(gamma(r)) - r * log(r) - 0.5 * log(wsum) - r *
    log(2 * pi) - log(s^(r - 1)) + 0.5 * tapply(
    log(w), id,
    "sum"
)
logLik <- sum(log(g)) + sum(logK)
du <- A %*% (uv[, 1] * duv * fuv) / g
dv <- A %*% (uv[, 2] * duv * fuv) / g
z <- list(
    u = u, v = v, fuv = fuv, logLik = logLik, du = du,
    dv = dv, A = A, status = status
)
class(z) <- c("WGLVmix", "GLVmix")
z
