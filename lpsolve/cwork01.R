library(lpSolveAPI)



n_fields <- 3 # 3 поля, i
n_agros <- 4  # 4 агрокультури, j
n_period <- 5 # 5 періодів, t

calcIndex <- function(i, j, t) {
  r <- (i - 1) * n_agros * n_period  + (j - 1) * n_period + t
  r
}

rows <- 0
cols <- n_fields * n_agros * n_period

area_fields <- c(10, 15, 20) # площа полів в га
prod_agros <- c(30, 55, 70, 95)  # вартість агрокультури з одного га

obj.fn <- vector("numeric", length = cols)

RowNames <- vector("character", length = 0)
ColNames <- vector("character", length = cols)

for(i in 1:n_fields) {
  for(j in 1:n_agros) {
    for(t in 1:n_period) {
      ind <- calcIndex(i, j, t)
      ColNames[ind] <- paste("X", i, j, t, sep="_")
      # print(paste(i - 1, j - 1, t, "->", ind, sep=" "))
      obj.fn[ind] <- -1 * area_fields[i] * prod_agros[j]
    }
  }
}

lprec <- make.lp(rows, cols)

set.objfn(lprec, obj.fn)

# 
set.type(lprec, 1:cols, "binary")

# на одному полі в один період одна культура
for(i in 1:n_fields) {
  for(t in 1:n_period) {
    rows <- rows + 1
    c <- vector("numeric", length = cols)
    for(j in 1:n_agros) {
      c[calcIndex(i, j, t)] <- 1 
    }
    RowNames <- c(RowNames, paste("A", i, t, sep="_"))
    add.constraint(lprec, c, "=", 1)
  }  
}

# на одному полі одна культура не підряд два періоди
for(i in 1:n_fields) {
  for(j in 1:n_agros) {
    for(t in 2:n_period) {
      rows <- rows + 1
      c <- vector("numeric", length = cols)
      c[calcIndex(i, j, t)] <- 1
      c[calcIndex(i, j, t - 1)] <- 1 
      RowNames <- c(RowNames, paste("B", i, j, t, sep="_"))
      add.constraint(lprec, c, "<=", 1)
    }
  }  
}



dimnames(lprec) <- list(RowNames, ColNames)

#write.table(, file="cwork01-lprec.csv")

res.s <- solve(lprec)

res.obj <- get.objective(lprec)
res.vars <- get.variables(lprec)

names(res.vars) <- ColNames
names(obj.fn) <- ColNames
#write.table(res.vars, file="cwork01-res-vars.csv")

print(obj.fn)
print(res.vars)
print(res.obj)