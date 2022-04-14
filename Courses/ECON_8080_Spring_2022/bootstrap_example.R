load("fertilizer_2000.RData")

# regress log(avyield) on log(avfert)
Y <- log(fertilizer_2000$avyield)
X <- cbind(1, log(fertilizer_2000$avfert))
bet <- solve(t(X)%*%X)%*%t(X)%*%Y
bet

# asymptotic standard error
n <- nrow(fertilizer_2000)
XX <- t(X)%*%X/n
ehat <- as.numeric(Y - X%*%bet)
Xe <- X*ehat
Omeg <- t(Xe)%*%Xe/n
V <- solve(XX)%*%Omeg%*%solve(XX)
se_asy <- (sqrt(diag(V))/sqrt(n))[2]
se_asy

# nonparametric/empirical bootstrap
B <- 1000
run_this_reg <- function(data) {
  this_Y <- log(data$avyield)
  this_X <- cbind(1, log(data$avfert))
  this_bet <- solve(t(this_X)%*%this_X)%*%t(this_X)%*%this_Y
  this_bet[2]
}

# vector to hold bootstrap estimates
boot_bet <- c()
for (b in 1:B) {
  # draw bootstrap sample
  boot_idx <- sample(1:n, size=n, replace=TRUE)
  boot_data <- fertilizer_2000[boot_idx,]
  boot_bet[b] <- run_this_reg(boot_data)
}
boot_V <- var(boot_bet)
se_np_boot <- sqrt(boot_V)
se_np_boot

# weighted bootstrap
boot_bet <- c()
for (b in 1:B) {
  w <- rexp(n) # exponential weights
  # w <- as.numeric(rmultinom(1:n,n,rep(1/n,n))) # alternative empirical bootstrap
  Xw <- X*w
  this_bet <- solve(t(Xw)%*%X)%*%t(Xw)%*%Y
  boot_bet[b] <- this_bet[2]
}
boot_V <- var(boot_bet)
se_wt_boot <- sqrt(boot_V)
se_wt_boot

# multiplier bootstrap
boot_bet <- c()
for (b in 1:B) {
  w <- rnorm(n)
  Xw <- X*w
  this_bet <- bet + solve(t(X)%*%X)%*%t(Xw)%*%ehat
  boot_bet[b] <- this_bet[2]
}
boot_V <- var(boot_bet)
se_mult_boot <- sqrt(boot_V)
se_mult_boot

round(cbind.data.frame(se_asy, se_np_boot, se_wt_boot, se_mult_boot),3)
