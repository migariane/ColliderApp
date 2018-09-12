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
g1 <- visreg(fit1, "A", gg = TRUE, line = list(col = "blue"),
       points = list(size = 2, pch = 1, col = "black")) + 
       theme_classic() +
       coord_cartesian(ylim = c(-4, 4)) +
       ggtitle("Figure 2A") 
       
# Box 2 --------------------
library(visreg)
library(ggplot2)
N <- 1000
set.seed(777)
A <- rnorm(N)
Y <- -1.2 * A + rnorm(N)
C <- +1 * A + +1 * Y + rnorm(N)
fit3 <- lm(Y ~ A)
fit4 <- lm(Y ~ A + C)
g2 <- visreg(fit4, "A", gg = TRUE, line = list(col = "red"),
     points = list(size = 2, pch = 1, col = "black")) + 
     theme_classic() +
     coord_cartesian(ylim = c(-4, 4)) +
     ggtitle("Figure 2B") 

library(gridExtra)
pdf(file = "Figure2.pdf", 
    width = 12, 
    height = 6,
    pointsize = 1)
grid.arrange(g1, g2, ncol = 2)
dev.off()

# Box 3 --------------------
generateData <- function(n, seed){
  set.seed(seed)
  Age_years <- rnorm(n, 65, 5)
  Sodium_gr <- Age_years / 18 + rnorm(n)
  sbp_in_mmHg <- 1.05 * Sodium_gr + 2.00 * Age_years + rnorm(n)
  hypertension <- ifelse(sbp_in_mmHg>=140,1,0)
  Proteinuria_in_mg <- 0.90 * Age_years + 2.00 * sbp_in_mmHg + 2.80 *Sodium_gr + rnorm(n)
  data.frame(sbp_in_mmHg, hypertension, Sodium_gr, Age_years, Proteinuria_in_mg)
}
ObsData <- generateData(n = 1000, seed = 777)
head(ObsData)
library(xtable)

print(xtable(summary(ObsData)))

# Box 4 --------------------
library(broom) # to visualize regression models output
library(visreg)

## Models fit on an additive scale
fit0 <- lm(sbp_in_mmHg ~ Sodium_gr, data = ObsData);tidy(fit0)
fit1 <- lm(sbp_in_mmHg ~ Sodium_gr + Age_years , data = ObsData);tidy(fit1)
fit2 <- lm(sbp_in_mmHg ~ Sodium_gr + Age_years + Proteinuria_in_mg, data = ObsData);tidy(fit2)

## Models fit visualization on an additive scale 
m1 <- visreg(fit0, "Sodium_gr", gg = TRUE, xlab = "Sodium (gr)", ylab = "SBP (mmHg)", line = list(col = "blue"),
       points = list(size = 2, pch = 1, col = "black"), bty = "n") + 
       theme_classic() +
       coord_cartesian(ylim = c(110, 165)) +
       ggtitle("Figure 4A") 
m2 <- visreg(fit1, "Sodium_gr", gg = TRUE, xlab = "Sodium (gr)", ylab = "SBP (mmHg)", line = list(col = "blue"),
       points = list(size = 2, pch = 1, col = "black"), bty = "n") + 
       theme_classic() +
       coord_cartesian(ylim = c(129, 140)) +
       ggtitle("Figure 4B") 
m3 <- visreg(fit2, "Sodium_gr", gg = TRUE, xlab = "Sodium (gr)", ylab = "SBP (mmHg)", line = list(col = "red"),
       points = list(size = 2, pch = 1, col = "black"), bty = "n") + 
       theme_classic() +
       coord_cartesian(ylim = c(129, 140)) +
       ggtitle("Figure 4C") 
library(gridExtra)
pdf(file = "Figure4.pdf", 
    width = 12, 
    height = 6,
    pointsize = 1)
grid.arrange(m1, m2, m3, ncol = 3)
dev.off()

# Box 5 --------------------
## Models fit in a multiplicative scale
library(dplyr) 
library(forestplot)

fit3 <- glm(hypertension ~ Sodium_gr, family=binomial(link='logit'), data=ObsData)
or <- round(exp(fit3$coef)[2],3)
ci95 <- exp(confint(fit3))[-1,]
lci <- round(ci95[1],3)
uci <- round(ci95[2],3)
model <- c("Crude")
result1 <- data.frame(model, or, lci, uci, stringsAsFactors = FALSE)

fit4 <- glm(hypertension ~ Sodium_gr + Age_years, family = binomial(link = "logit"), data = ObsData) 
or <- round(exp(fit4$coef)[2],3)
ci95 <- exp(confint(fit4))[2,]
lci <- round(ci95[1],3)
uci <- round(ci95[2],3)
model <- c("Adjusted")
result2 <- data.frame(model, or, lci, uci, stringsAsFactors = FALSE)

fit5 <- glm(hypertension ~ Sodium_gr + Age_years + Proteinuria_in_mg, family = binomial(link = "logit"), data = ObsData) 
or <- round(exp(fit5$coef)[2],3)
ci95 <- exp(confint(fit5))[2,]
lci <- round(ci95[1],3)
uci <- round(ci95[2],3)
model <- c("Collider")
result3 <- data.frame(model, or, lci, uci, stringsAsFactors = FALSE)

# Models fit visualization (Forest plot function and plot) to depict the collider effect
or_graph <- function(fp){
    
    tabla <- cbind(c("Model", paste(fp$model)),
                   c("Odds ratio", fp$or), 
                   c("95%CI", paste0("(", fp$lci, " - ", fp$uci, ")")))

    forestplot(labeltext = tabla,
               graph.pos = 3,
               mean = c(NA, fp$or),
               is.summary = c(TRUE, rep(FALSE, nrow(fp))),
               lower = c(NA, fp$lci),
               upper = c(NA, fp$uci),
               xlab = "Odds ratio",
               txt_gp = fpTxtGp(label = gpar(cex = 1.25),
                                ticks = gpar(cex = 1.1),
                                xlab  = gpar(cex = 1.2),
                                title = gpar(cex = 1.2)),
               col = fpColors(box = "blue", lines = "blue", zero = "black"),
               cex = 0.9,
               clip = c(0, 10),
               zero = 1,
               boxsize = 0.05,
               lwd.ci = 2,
               ci.vertices = TRUE,
               lineheight = "auto",
               xticks = seq(0, 10, 1),
               ci.vertices.height = 0.1,
               grid = TRUE
    )
}

fp <- rbind(result1,result2,result3);fp %>% or_graph()

# Box 6 -----------------------
# Monte Carlo Simulations
R<-1000
true <- rep(NA, R)
collider <- rep(NA,R)
se <- rep(NA,R)
set.seed(050472)

for(r in 1:R) {
    if (r%%10 == 0) cat(paste("This is simulation run number", r, "\n"))
    #Function to generate data 
    generateData <- function(n){
        Age_years <- rnorm(n, 65, 5)
        Sodium_gr <- Age_years / 18 + rnorm(n)
        sbp_in_mmHg <- 1.05 * Sodium_gr + 2.00 * Age_years + rnorm(n)
        Proteinuria_in_mg <- 0.90 * Age_years + 2.00 * sbp_in_mmHg + 2.80 *Sodium_gr + rnorm(n)
        data.frame(sbp_in_mmHg, Sodium_gr, Age_years, Proteinuria_in_mg)
    }
    ObsData <- generateData(n=10000) 
    # True effect
    true[r] <- summary(lm(sbp_in_mmHg ~ Sodium_gr + Age_years, data = ObsData))$coef[2,1]
    # Collider effect
    collider[r] <- summary(lm(sbp_in_mmHg ~ Sodium_gr + Age_years + Proteinuria_in_mg, data = ObsData))$coef[2,1]
    se[r] <- summary(lm(sbp_in_mmHg ~ Sodium_gr + Age_years + Proteinuria_in_mg, data = ObsData))$coef[2,2]
}

# Estimate of true effect
mean(true)

# Estimate of outcome regression including the collider 
mean(collider)

# Standard error/confidence 
lci <- (mean(collider) - 1.96*mean(se)); mean(lci)
uci <- (mean(collider) + 1.96*mean(se)); mean(uci)

# Bias 
Bias <- (true - abs(collider));mean(Bias)

# % Bias
relBias <- ((true - abs(collider)) / true); mean(relBias) * 100

# Plot bias
plot(relBias)
