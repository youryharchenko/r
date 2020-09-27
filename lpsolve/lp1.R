#library(lpSolve)
library(lpSolveAPI)

lprec <- make.lp(0, 4)

obj.fn <- c(1, 3, 6.24, 0.1)
set.objfn(lprec, obj.fn)

c1 <- c(0, 78.26, 0, 2.9)
add.constraint(lprec, c1, ">=", 92.3)
c2 <- c(0.24, 0, 11.31, 0)
add.constraint(lprec, c2, "<=", 14.8)
c3 <- c(12.68, 0, 0.08, 0.9)
add.constraint(lprec, c3, ">=", 4)

set.bounds(lprec, lower = c(28.6, 18), columns = c(1, 4))
set.bounds(lprec, upper = 48.98, columns = 4)

RowNames <- c("THISROW", "THATROW", "LASTROW")
ColNames <- c("COLONE", "COLTWO", "COLTHREE", "COLFOUR")
dimnames(lprec) <- list(RowNames, ColNames)

print(lprec)

res.s <- solve(lprec)

res.obj <- get.objective(lprec)
res.vars <- get.variables(lprec)

print(res.vars)

print(res.obj)
print(sum(res.vars * obj.fn))

res.constr <- get.constraints(lprec)
print(res.constr)
print(c(sum(res.vars * c1), sum(res.vars * c2), sum(res.vars * c3)))