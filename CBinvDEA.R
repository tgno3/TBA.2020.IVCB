#########################################################################################################################
### Project  : The correct formulation of inverse DEA with capital budget
### Script   : CBinvDEA.R
### Contents : Prove JSG's model is wrong once again
#########################################################################################################################

#########################################################################################################################
### Setting up Environment
#########################################################################################################################

# Load library
library(DJL)

# Load data and parameters
load("CBinvDEA.RData")


#########################################################################################################################
### Numerical Example: Table 1 in JSG's paper
#########################################################################################################################

# Table 1. Experimental data
table.1 <- cbind(df.JSG$Z0, df.JSG$io[,,1], 
                 Z1 = c(df.JSG$io[,2,2] + df.JSG$ZT), df.JSG$io[,,2], df.JSG$ZT)
print(table.1[, c(2:4, 6:8, 1, 9)])

# Run JSG model
dm.dynamic.et(df.JSG$io[,1,], df.JSG$io[,3,], df.JSG$io[,2,], df.JSG$ZT)

# Run DJL model
dm.dynamic.bc(df.JSG$io[,1,], df.JSG$io[,3,], df.JSG$io[,2,], df.JSG$Z0)


#########################################################################################################################
### Fire Department
#########################################################################################################################

# Table 2. Descriptive statistics
df.f.agg <- cbind(Total.Budget = rep(df.f.bg$Z.0 * 10^-6, 5),
                  df.f.2d[, c(id.x) + 1],
                  df.f.2d[, c(id.z, id.y[1]) + 1] * 10^-6,
                  df.f.2d[, c(id.y[2]) + 1, drop = F])
table.2 <- sapply(df.f.agg, function(x) c(Min  = min(x), 
                                          Med  = median(x), 
                                          Mean = mean(x), 
                                          Max  = max(x), 
                                          Std  = sd(x)))
noquote(format(round(t(table.2), 2), big.mark = ","))


# Table 3. Comparative results of efficiency
res.f.et <- dm.dynamic.et(df.f.3d[, id.x, ], df.f.3d[, id.y, ], df.f.3d[, id.z, ], df.f.bg$Z.T, rts)
res.f.bc <- dm.dynamic.bc(df.f.3d[, id.x, ], df.f.3d[, id.y, ], df.f.3d[, id.z, ], df.f.bg$Z.0, rts)
table.3  <- matrix(c(res.f.et$eff.s, res.f.bc$eff.s, res.f.et$eff.t, res.f.bc$eff.t), nrow(df.f.bg), 
                   dimnames = list(df.f.bg$DMU, 
                                   c("ET.Eff.sys", "LL.Eff.sys", paste0("ET.", 2012:2016), paste0("LL.", 2012:2016))))
#id.show  <- c(which(round(table.3[, 1], 6) == 1), which(rownames(table.3) == "Yeoju"))
print(round(table.3[, c(1, 2, 3, 8, 4, 9, 5, 10, 6, 11, 7, 12)], 4))


# How many system efficient DMUs?
apply(table.3[, 1:2], 2, function(x) sum(round(x, 8) == 1))


# Footnote 5
summary(lm((table.3[,1] - table.3[,2]) ~ df.f.bg$Z.T))


# Table 4. Namyangju
id.nyj  <- which(df.f.2d$DMU == "Namyangju")
table.4 <- rbind(df.f.agg[id.nyj, -1],
                 aggregate(df.f.agg[-id.nyj, -1], list(df.f.2d$Year[-id.nyj]), "mean")[, -1])
rownames(table.4) <- c(2012:2016, paste0("Avg.", 2012:2016))
round(t(table.4[c(1, 6, 2, 7, 3, 8, 4, 9, 5, 10),]), 2)