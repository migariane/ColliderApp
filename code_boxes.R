# Box 1 --------------------
library(visreg) # to visualize regression output
library(ggplot2) # to visualize regression output
N <- 1000
set.seed(777)
W <- rnorm(N)
A <- 0.5 * W + rnorm(N)
Y <- 0.3 * A + 0.4 * W + rnorm(N)
fit1 <- lm(Y ~ A)
fit2 <- lm(Y ~ A + W)
visreg(fit1, "A", gg = TRUE, line = list(col = "blue"),
       points = list(size = 2, pch = 1, col = "black")) + theme_classic()

# Box 2 --------------------
library(visreg)
library(ggplot2)
N <- 1000
set.seed(777)
A <- rnorm(N)
Y <- 0.3 * A + rnorm(N)
W <- 1.2 * A + 0.9 * Y + rnorm(N)
fit3 <- lm(Y ~ A)
fit4 <- lm(Y ~ A + W)
visreg(fit3, "A", gg = TRUE, line = list(col = "red"),
       points = list(size = 2, pch = 1, col = "black")) + theme_classic()

# Box 3 --------------------
generateData <- function(n, seed){
  set.seed(seed)
  Sodium_gr <- rnorm(n, 3.50, 0.50)
  Age_years <- Sodium_gr * 18 + rnorm(n)
  sbp_in_mmHg <- 2.25 * Sodium_gr + 2.00 * Age_years + rnorm(n)
  Proteinuria_in_mg <- 0.90 * Age_years + 1.80 * sbp_in_mmHg + 3.50 *Sodium_gr + rnorm(n)
  data.frame(sbp_in_mmHg, Sodium_gr, Age_years, Proteinuria_in_mg)
}
ObsData <- generateData(n = 1000, seed = 777)

# Box 4 --------------------
library(broom) # to visualize regression models output
library(visreg)
## Models Fit
fit0 <- lm(sbp_in_mmHg ~ Sodium_gr, data = ObsData);tidy(fit0)
fit1 <- lm(sbp_in_mmHg ~ Sodium_gr + Age_years , data = ObsData);tidy(fit1)
fit2 <- lm(sbp_in_mmHg ~ Sodium_gr + Age_years + Proteinuria_in_mg, data = ObsData);tidy(fit2)
## Models fit visualization
par(mfrow = c(1,3))
visreg(fit0, ylab = "SBP in mmHg", line = list(col = "blue"),
       points = list(cex = 1.5, pch = 1), jitter = 10, bty = "n")
visreg(fit1, ylab = "SBP in mmHg", line = list(col = "blue"),
       points = list(cex = 1.5, pch = 1), jitter = 10, bty = "n")
visreg(fit2, ylab = "SBP in mmHg", line = list(col = "red"),
       points = list(cex = 1.5, pch = 1), jitter = 10, bty = "n")