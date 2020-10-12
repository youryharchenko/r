#library(lpSolve)
library(lpSolveAPI)

lprec <- make.lp(0, 2)

obj.fn <- c(-50, -40)
set.objfn(lprec, obj.fn)

c1 <- c(2, 5)
add.constraint(lprec, c1, "<=", 20)
c2 <- c(8, 5)
add.constraint(lprec, c2, "<=", 40)
c3 <- c(5, 6)
add.constraint(lprec, c3, "<=", 30)

#set.bounds(lprec, lower = c(28.6, 18), columns = c(1, 4))
#set.bounds(lprec, upper = 48.98, columns = 4)

RowNames <- c("R1", "R2", "R3")
ColNames <- c("P1", "P2")
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

get.sensitivity.obj(lprec)
get.sensitivity.rhs(lprec)
