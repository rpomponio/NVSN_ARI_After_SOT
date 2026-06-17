# ── Custom tidy function: glm() output with Wald CIs ──────────────────────────
#    broom::tidy.glm() defaults to profile likelihood CIs via confint.glm(),
#    which are asymmetric and computationally expensive. For a binary-outcome
#    GLM (family = binomial) we prefer symmetric Wald CIs:
#      CI = estimate ± z_{alpha/2} * std.error   (on the link scale)
#    then exponentiate to the OR scale if requested. p-values are already
#    Wald-based in broom's default output, so we do not recompute them.
#
#    Note: conf.int=TRUE must be passed to broom::tidy() to get any CI
#    columns at all; we then overwrite them with the Wald values.
tidy.glm.wald <- function(x, exponentiate=FALSE, conf.level=0.95, ...) {
  
  # ── pull raw coefficient table: Estimate / Std. Error / z / p ─────────────
  coef.mat <- as.data.frame(coef(summary(x)))
  coef.mat$term <- rownames(coef.mat)
  
  z.crit <- qnorm(1 - (1 - conf.level) / 2)
  
  coef.mat$conf.low  <- coef.mat$Estimate - z.crit * coef.mat$`Std. Error`
  coef.mat$conf.high <- coef.mat$Estimate + z.crit * coef.mat$`Std. Error`
  
  if (exponentiate) {
    coef.mat$estimate  <- exp(coef.mat$Estimate)
    coef.mat$conf.low  <- exp(coef.mat$conf.low)
    coef.mat$conf.high <- exp(coef.mat$conf.high)
  } else {
    coef.mat$estimate  <- coef.mat$Estimate
  }
  
  coef.mat$std.error <- coef.mat$`Std. Error`
  coef.mat$statistic <- coef.mat$Estimate / coef.mat$`Std. Error`
  coef.mat$p.value   <- coef.mat[, which(grepl("Pr(>|\\w|)", colnames(coef.mat)))]
  
  # ── return columns in standard broom order ─────────────────────────────────
  coef.mat[, c("term", "estimate", "std.error",
               "statistic", "p.value", "conf.low", "conf.high")]
}