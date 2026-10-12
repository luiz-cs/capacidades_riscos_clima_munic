######################################################################
## CODE TO REPLICATE THE RESULTS IN THE PAPER 
##
## LMest: an R package for latent Markov models for longitudinal categorical data
## 
## Submitted to Journal of Statistical Software
##
## Authors: Bartolucci, F.,  Pandolfi, S. Pennoni, F. 
## 
######################################################################


######################################################################
## CODE OF THE SECTION 
##
## 3. Basic latent Markov model
## 3.1. Application to job satisfaction
##
######################################################################

library("LMest")
data("RLMSdat", package = "LMest")
head(RLMSdat)

out <- aggr_data(RLMSdat)
yv <- out$freq
S <- 5 - out$data_dis

time <- proc.time()
mod1 <- est_lm_basic(S, yv, k = 3, mod = 1, start = 0, out_se = TRUE)
proc.time() - time

mod1
summary(mod1)

######################################################################
## CODE OF THE SECTION 
## Selection of the number of states

set.seed(14326)
time <- proc.time()
res1 <- search.model.LM(version = "basic", kv = 1:5, S, yv, mod = 1, out_se = TRUE)
proc.time()-time

summary(res1)
summary(res1$out.single[[4]])

######################################################################
## CODE OF THE SECTION 
##
##
## 4. Covariates in the measurement model
## 4.2. Application to the health related longitudinal data
##
######################################################################

data("data_SRHS_long", package = "LMest")
data_SRHS <- data_SRHS_long
data_SRHS[1:10, ]

out <- with(data_SRHS,
            long2matrices(id = id,
                          X = cbind(gender - 1,
                                    race == 2 | race == 3,
                                    education == 4,
                                    education == 5,
                                    age - 50,
                                    (age - 50)^2/100),
                          Y = srhs))
S <- 5 - out$YY
X <- out$XX
X[3994, , ]

mod2 <- est_lm_cov_manifest(S, X, k = 3, mod = "LM", tol = 1e-8, start = 1, output = TRUE, out_se = TRUE)

ind <- (X[, 3, 5] >= 20) 
Sn <- S[ind, , ]
Xn  <- X[ind, , ]
set.seed(71432)
time <- proc.time()
res2 <- search.model.LM(version = "manifest", kv = 1:5, Sn, Xn, mod = "LM", out_se = TRUE, tol2 = 1e-8)
proc.time() - time
summary(res2)

summary(res2$out.single[[4]])

round(res2$out.single[[4]]$sebe, 3)

######################################################################
## CODE OF THE SECTION 
##
## 5. Covariates in the latent model
## 5.2. Application to the health related longitudinal data
######################################################################

data("data_SRHS_long", package = "LMest")
data_SRHS <- data_SRHS_long
out <- with(data_SRHS,
            long2matrices(id = id,
                          X = cbind(gender - 1,
                                    race == 2 | race == 3,
                                    education == 4,
                                    education == 5,
                                    age - 50, (age - 50)^2/100),
                          Y = srhs))
S <- 5 - out$YY
X <- out$XX  	
X1 <- X[, 1, ]
TT <- 8
X2 <- X[, 2:TT, ]
colnames(X1) <- c("gender", "race", "some college", "college and above", 
   "age", "age^2")
dimnames(X2)[[3]] <- c("gender", "race", "some college", "college and above", 
   "age", "age^2")

time <- proc.time()
mod3 <- est_lm_cov_latent(S = S, X1 = X1, X2 = X2, k = 5,
                          start = 0, param = "multilogit", fort = TRUE,
                          output = TRUE)
proc.time() - time
mod3
summary(mod3)

ind1 <- (X1[, 1] == 1 & X1[, 2] == 0 & X1[, 4] == 1)
piv1 <- round(colMeans(mod3$Piv[ind1, ]), 4)
piv1

PI1 <- round(apply(mod3$PI[ , , ind1, 2:TT], c(1, 2), mean), 4)
PI1

ind2 <- (X1[,1] == 1 & X1[,2] == 1 &  X1[,4] == 1)
piv2 <- round(colMeans(mod3$Piv[ind2, ]), 4)
piv2

PI2 <- round(apply(mod3$PI[, , ind2, 2:TT], c(1, 2), mean), 4)
PI2

######################################################################
## Decoding
##
######################################################################

out <- decoding(mod3, S, X1, X2)
head(out$Ug)

######################################################################
## CODE OF THE SECTION 
##
## 6. Mixed latent Markov model
## 6.2. Application example on longitudinal data from criminology
##
######################################################################  

data("data_criminal_sim", package = "LMest")
head(data_criminal_sim)

out <- long2wide(data = data_criminal_sim, nameid = "id", namet = "time",
                 colx = "sex", coly = paste0("y", 1:10))
YY <- out$YY
XX <- out$XX
freq <- out$freq

YY[148, , ]
XX[148, ]
freq[148]

YY[149, , ]
XX[149, ]
freq[149]


YY <- YY[XX[ , 1] == 2, , ]
freq <- freq[XX[ , 1] == 2]
time <- proc.time()
mod4 <-  est_lm_mixed(S = YY, yv = freq, k1 = 2, k2 = 2)
proc.time() - time

mod4
summary(mod4)

round(mod4$Psi[2, , ], 3)


