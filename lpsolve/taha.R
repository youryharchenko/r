library(lpSolveAPI)

rows <- 0
cols <- 5

lprec <- make.lp(rows, cols)

r1 <- c(5, 4, 3, 7, 8)
r2 <- c(1, 7, 9, 4, 6)
r3 <- c(8, 10, 2, 1, 10)

obj.fn <- c(-20, -40, -20, -15, -30)

add.constraint(lprec, r1, "<=", 25)
add.constraint(lprec, r2, "<=", 25)
add.constraint(lprec, r3, "<=", 25)

#set.bounds(lprec, lower = c(0, 0, 0, 0, 0))
#set.bounds(lprec, upper = c(1, 1, 1, 1, 1))

set.type(lprec, 1:5, "binary")

set.objfn(lprec, obj.fn)

RowNames <- c("R1", "R2", "R3")
ColNames <- c("X1", "X2", "X3", "X4", "X5")
dimnames(lprec) <- list(RowNames, ColNames)

print(lprec)

res.s <- solve(lprec)

res.obj <- get.objective(lprec)
res.vars <- get.variables(lprec)

print(res.vars)
print(res.obj)
