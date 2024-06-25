t <- Z$dt2$hat_mu
s <- Z$dt2$Var_res1
m <- Z$dt2$Nobs
u <- 300
v <- 300
n <- length(t)
w <- rep(1, n) / n
eps <- 1e-04
if (length(m) == 1) {
    m <- rep(m, length(t))
}
r <- (m - 1) / 2
if (length(u) == 1) {
    u <- seq(min(t) - eps, max(t) + eps, length = u)
}
if (length(v) == 1) {
    v <- seq(min(s) - eps, max(s) + eps, length = v)
}
v <- v[v > 0]
pu <- length(u)
du <- rep(1, pu)
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
Au <- dnorm(outer(outer(t, u, "-") * outer(sqrt(m), rep(
    1,
    pu
)), sqrt(v), "/"))
Au <- Au / outer(outer(1 / sqrt(m), rep(1, pu)), sqrt(v))
Auv <- Av * Au
A <- NULL
for (j in 1:pv) A <- cbind(A, Auv[, , j])
duv <- as.vector(kronecker(du, dv))
f <- KWDual(A, duv, w, ...)
fuv <- f$f
uv <- expand.grid(alpha = u, theta = v)
g <- as.vector(A %*% (duv * fuv))
logLik <- n * sum(w * log(f$g))
du <- A %*% (uv[, 1] * duv * fuv) / g
dv <- A %*% (uv[, 2] * duv * fuv) / g
z <- list(
    u = u, v = v, fuv = fuv, logLik = logLik, du = du,
    dv = dv, A = A, status = f$status
)
class(z) <- "GLVmix"
z
