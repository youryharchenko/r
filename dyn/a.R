n <- 2

a <- diag(rep(1, n))
a

x <- array(c(2, 3))
x
crossprod(x, a)
crossprod(t(x), x)
crossprod(x, x)
