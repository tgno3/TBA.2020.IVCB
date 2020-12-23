#########################################################################################################################
### Project  : Dynamic Inverse DEA with Capital Budgetting
### Script   : CBinvDEA on GIT.R
### Contents : Demonstrate the limitations of the existing model
#########################################################################################################################

#########################################################################################################################
### Setting up Environment
#########################################################################################################################

# Load library
pkgs <- c("DJL", "abind")
sapply(pkgs, require, character.only = T)

# Load data and parameters
load("CBinvDEA.RData")


#########################################################################################################################
### Replication
#########################################################################################################################

# Table 1 in JSG paper
table.1.JSG <- cbind(df.JSG$Z0, df.JSG$io[,,1], df.JSG$io[,,2], df.JSG$ZT)
print(table.1.JSG[, c(2:7, 1, 8)])

# Standard JSG model
dm.dynamic.JSG(df.JSG$io[,1,], df.JSG$io[,3,], df.JSG$io[,2,], df.JSG$ZT)

# Inverse JSG model on DMU A
target.dynamic.JSG(df.JSG$io[,1,], df.JSG$io[,3,], df.JSG$io[,2,], df.JSG$Z0, dmu = 1, beta = c(5, 7))


#########################################################################################################################
### Falsification
#########################################################################################################################

# Scenario 1: Make DMU A to have produced output of 1 in t1 & t2
df.JSG.p1          <- df.JSG
df.JSG.p1$io[1,3,] <- 1; df.JSG.p1$io[1,1,] <- 100
dm.dynamic.JSG(df.JSG.p1$io[,1,], df.JSG.p1$io[,3,], df.JSG.p1$io[,2,], df.JSG.p1$ZT)

# Scenario 2: Make DMU B to have produced output of 1 in t1 & t2
df.JSG.p2          <- df.JSG
df.JSG.p2$io[2,3,] <- 1
dm.dynamic.JSG(df.JSG.p2$io[,1,], df.JSG.p2$io[,3,], df.JSG.p2$io[,2,], df.JSG.p2$ZT)

# Scenario 3: Introduce DMU D
df.JSG.p3 <- df.JSG
df.JSG.p3 <- list(io = abind(df.JSG.p3$io, 
                             array(c(42, 30, 4, 18, 20, 1), c(1, 3, 2), dimnames = list("D", c("x", "z", "y"))), 
                             along = 1),
                  Z0 = rbind(df.JSG.p3$Z0, D = 105), ZT = rbind(df.JSG.p3$ZT, D = 55))
res.JSG.p3 <- dm.dynamic.JSG(df.JSG.p3$io[,1,], df.JSG.p3$io[,3,], df.JSG.p3$io[,2,], df.JSG.p3$ZT)

# Table 1
table.1 <- cbind(df.JSG.p3$Z0, df.JSG.p3$io[,,1], df.JSG.p3$io[,,2], df.JSG.p3$ZT, eff = res.JSG.p3$eff.t)
print(table.1[, c(1:4, 9, 6:7, 10, 8)])

# Inverse JSG model on DMU D
target.dynamic.JSG(df.JSG.p3$io[,1,], df.JSG.p3$io[,3,], df.JSG.p3$io[,2,], df.JSG.p3$Z0, dmu = 4, beta = c(5, 2))

# Inverse JSG model on DMU D under VRS
target.dynamic.JSG(df.JSG.p3$io[,1,], df.JSG.p3$io[,3,], df.JSG.p3$io[,2,], df.JSG.p3$Z0, dmu = 4, beta = c(5, 2), 
                   rts = "vrs")

