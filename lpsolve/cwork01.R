library(lpSolveAPI)

data_fields <- data.frame(
  name = c("k", "l", "m", "n"), # 3 поля, i
  area = c(10, 15, 20, 10)     # площа полів в га
)

data_agros <- data.frame(
  name=c("p", "q", "r", "s"), # 4 агрокультури, j
  prod=c(30, 55, 70, 95)      # вартість агрокультури з одного га
)

n_period <- 5 # 5 періодів, t
n_fields <- nrow(data_fields)
n_agros <- nrow(data_agros)

calcIndex <- function(i, j, t) {
  r <- (i - 1) * n_agros * n_period  + (j - 1) * n_period + t
}

rows <- 0
cols <- n_fields * n_agros * n_period

obj.fn <- vector("numeric", length = cols)

RowNames <- vector("character", length = 0)
ColNames <- vector("character", length = cols)

data_matrix <- data.frame()
l_cols <- vector("list", length = cols + 1)
l_cols[[1]] <- "row_name"
v_cols <- vector("list", length = cols + 1)
v_cols[[1]] <- "obj"

for(i in 1:n_fields) {
  for(j in 1:n_agros) {
    for(t in 1:n_period) {
      ind <- calcIndex(i, j, t)
      ColNames[ind] <- paste("X", data_fields$name[i], data_agros$name[j], t, sep="_")
      l_cols[[ind + 1]] <- ColNames[ind]
      # print(paste(i - 1, j - 1, t, "->", ind, sep=" "))
      obj.fn[ind] <- -1 * data_fields$area[i] * data_agros$prod[j]
      v_cols[[ind + 1]] <- obj.fn[ind]
    }
  }
}

data_matrix <- rbind(data_matrix, v_cols)
colnames(data_matrix) <- l_cols


names(obj.fn) <- ColNames

lprec <- make.lp(rows, cols)

set.objfn(lprec, obj.fn)

# 
set.type(lprec, 1:cols, "binary")

# на одному полі в один період одна культура
for(i in 1:n_fields) {
  for(t in 1:n_period) {
    rows <- rows + 1
    c <- vector("numeric", length = cols)
    v_cols <- vector("list", length = cols + 1)
    r_name <- paste("A", data_fields$name[i], t, sep="_")
    v_cols[1:cols + 1] <- 0
    v_cols[[1]] <- r_name
    
    for(j in 1:n_agros) {
      ind <- calcIndex(i, j, t)
      c[ind] <- 1 
      v_cols[[ind + 1]] <- 1
    }
    data_matrix <- rbind(data_matrix, v_cols)
    RowNames <- c(RowNames, r_name)
    add.constraint(lprec, c, "=", 1)
  }  
}

# на одному полі одна культура не підряд два періоди
for(i in 1:n_fields) {
  for(j in 1:n_agros) {
    for(t in 2:n_period) {
      rows <- rows + 1
      c <- vector("numeric", length = cols)
      v_cols <- vector("list", length = cols + 1)
      r_name <- paste("B", data_fields$name[i], data_agros$name[j], t, sep="_")
      v_cols[1:cols + 1] <- 0
      v_cols[[1]] <- r_name
      ind0 <- calcIndex(i, j, t - 1)
      ind1 <- calcIndex(i, j, t)
      c[ind1] <- 1
      c[ind0] <- 1
      v_cols[[ind1 + 1]] <- 1
      v_cols[[ind0 + 1]] <- 1
      data_matrix <- rbind(data_matrix, v_cols)
      RowNames <- c(RowNames, r_name)
      add.constraint(lprec, c, "<=", 1)
    }
  }  
}

# в один період одна культура хоча б на одному полі
for(j in 1:n_agros) {
  for(t in 1:n_period) {
    rows <- rows + 1
    c <- vector("numeric", length = cols)
    v_cols <- vector("list", length = cols + 1)
    r_name <- paste("C", data_agros$name[j], t, sep="_")
    v_cols[1:cols + 1] <- 0
    v_cols[[1]] <- r_name
    
    for(i in 1:n_fields) {
      ind <- calcIndex(i, j, t)
      c[ind] <- 1 
      v_cols[[ind + 1]] <- 1
    }
    data_matrix <- rbind(data_matrix, v_cols)
    RowNames <- c(RowNames, r_name)
    add.constraint(lprec, c, ">=", 1)
  }  
}


#print(data_matrix)
write.csv(data_matrix, file="cwork01-matrix.csv")

dimnames(lprec) <- list(RowNames, ColNames)

#print(lprec)
write.lp(lprec, file="cwork01-lprec.lp")
res.s <- solve(lprec)

if (res.s == 0) {
  print(paste("Solve OK:", res.s, sep=" "))
  
  res.obj <- get.objective(lprec)
  res.vars <- get.variables(lprec)
  
  names(res.vars) <- ColNames
  
  write.csv(data.frame(name=ColNames, value=res.vars), file="cwork01-res-vars.csv")
  
  
  print(res.vars)
  print(res.obj)
} else {
  print(paste("Solve ret_code:", res.s, sep=" "))
}

# Status Codes:
# 0:  "optimal solution found"                                                         
# 1:  "the model is sub-optimal"                                                       
# 2:  "the model is infeasible"                                                        
# 3:  "the model is unbounded"                                                         
# 4:  "the model is degenerate"                                                        
# 5:  "numerical failure encountered"                                                  
# 6:  "process aborted"                                                                
# 7:  "timeout"                                                                        
# 9:  "the model was solved by presolve"                                               
# 10:  "the branch and bound routine failed"                                            
# 11:  "the branch and bound was stopped because of a break-at-first or break-at-value" 
# 12:  "a feasible branch and bound solution was found"                                 
# 13:  "no feasible branch and bound solution was found"
