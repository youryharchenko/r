library(lpSolve)

obj.fun <- c(20, 60)
print(obj.fun)

constr <- matrix(c(30, 20, 5, 10, 1, 1), ncol = 2 ,byrow = TRUE)
print(constr)

rhs <- c(2700, 850, 95)
print(rhs)

constr.dir <- c("<=", "<=", ">=")
print(constr.dir)

prod.sol <- lp("max", obj.fun, constr, constr.dir, rhs, compute.sens = TRUE)

print(prod.sol$obj.val)
print(prod.sol$solution)
print(prod.sol$duals)