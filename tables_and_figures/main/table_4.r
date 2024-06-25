#required variables from analysis: https://github.com/bethan-mallabar-rimmer/CRC_IRM/blob/main/analysis.R
#p_v_train and p_v_test from Section 7

#TABLE 4
#========
#Required packages
#==================
#install.packages('pROC')
library('pROC')
library('dplyr')

#Epi tests function
#==================
epi.tests=function (dat, method = "exact", digits = 2, conf.level = 0.95) 
{
  if (digits != 2 & digits != 3 & digits != 4) 
    stop("Argument 'digits' for this function must take the value of 2, 3 or 4.")
  dim <- ifelse(is.null(dim(dat)[2]), 0, dim(dat)[2])
  id <- class(dat) == "grouped_df" | class(dat) == "tbl_df" | 
    class(dat) == "tbl" | class(dat) == "data.frame"
  if (dim == 3 & sum(id) == 4) {
    names(dat) <- c("tes", "out", "n")
    if (!is.integer(dat$n)) 
      stop("Column 3 (cell frequencies) must be integer.")
    if (!is.factor(dat$tes)) 
      stop("Column 1 (test) must be a factor.")
    if (!is.factor(dat$out)) 
      stop("Column 2 (outcome) must be a factor.")
    dat <- xtabs(n ~ tes + out, data = dat)
  }
  if (length(dat) == 4 & is.vector(dat) == TRUE) {
    dat <- as.table(matrix(dat, nrow = 2, byrow = TRUE))
  }
  N. <- 1 - ((1 - conf.level)/2)
  z <- qnorm(N., mean = 0, sd = 1)
  a <- dat[1]
  b <- dat[3]
  c <- dat[2]
  d <- dat[4]
  M1 <- a + c
  M0 <- b + d
  N1 <- a + b
  N0 <- c + d
  total <- a + b + c + d
  tdat <- as.matrix(cbind(M1, total))
  if (method == "exact") {
    trval <- zexact(tdat, conf.level)
  }
  if (method == "wilson") {
    trval <- zwilson(tdat, conf.level)
  }
  if (method == "agresti") {
    trval <- zagresti(tdat, conf.level)
  }
  if (method == "clopper-pearson") {
    trval <- zclopperpearson(tdat, conf.level)
  }
  if (method == "jeffreys") {
    trval <- zjeffreys(tdat, conf.level)
  }
  tp <- data.frame(statistic = "tp", est = trval$est, lower = trval$lower, 
                   upper = trval$upper)
  tdat <- as.matrix(cbind(N1, total))
  if (method == "exact") {
    trval <- zexact(tdat, conf.level)
  }
  if (method == "wilson") {
    trval <- zwilson(tdat, conf.level)
  }
  if (method == "agresti") {
    trval <- zagresti(tdat, conf.level)
  }
  if (method == "clopper-pearson") {
    trval <- zclopperpearson(tdat, conf.level)
  }
  if (method == "jeffreys") {
    trval <- zjeffreys(tdat, conf.level)
  }
  ap <- data.frame(statistic = "ap", est = trval$est, lower = trval$lower, 
                   upper = trval$upper)
  tdat <- as.matrix(cbind(a, M1))
  if (method == "exact") {
    trval <- zexact(tdat, conf.level)
  }
  if (method == "wilson") {
    trval <- zwilson(tdat, conf.level)
  }
  if (method == "agresti") {
    trval <- zagresti(tdat, conf.level)
  }
  if (method == "clopper-pearson") {
    trval <- zclopperpearson(tdat, conf.level)
  }
  if (method == "jeffreys") {
    trval <- zjeffreys(tdat, conf.level)
  }
  se <- data.frame(statistic = "se", est = trval$est, lower = trval$lower, 
                   upper = trval$upper)
  tdat <- as.matrix(cbind(d, M0))
  if (method == "exact") {
    trval <- zexact(tdat, conf.level)
  }
  if (method == "wilson") {
    trval <- zwilson(tdat, conf.level)
  }
  if (method == "agresti") {
    trval <- zagresti(tdat, conf.level)
  }
  if (method == "clopper-pearson") {
    trval <- zclopperpearson(tdat, conf.level)
  }
  if (method == "jeffreys") {
    trval <- zjeffreys(tdat, conf.level)
  }
  sp <- data.frame(statistic = "sp", est = trval$est, lower = trval$lower, 
                   upper = trval$upper)
  tdat <- as.matrix(cbind(a, N1))
  if (method == "exact") {
    trval <- zexact(tdat, conf.level)
  }
  if (method == "wilson") {
    trval <- zwilson(tdat, conf.level)
  }
  if (method == "agresti") {
    trval <- zagresti(tdat, conf.level)
  }
  if (method == "clopper-pearson") {
    trval <- zclopperpearson(tdat, conf.level)
  }
  if (method == "jeffreys") {
    trval <- zjeffreys(tdat, conf.level)
  }
  pv.pos <- data.frame(statistic = "pv.pos", est = trval$est, 
                       lower = trval$lower, upper = trval$upper)
  tdat <- as.matrix(cbind(d, N0))
  if (method == "exact") {
    trval <- zexact(tdat, conf.level)
  }
  if (method == "wilson") {
    trval <- zwilson(tdat, conf.level)
  }
  if (method == "agresti") {
    trval <- zagresti(tdat, conf.level)
  }
  if (method == "clopper-pearson") {
    trval <- zclopperpearson(tdat, conf.level)
  }
  if (method == "jeffreys") {
    trval <- zjeffreys(tdat, conf.level)
  }
  pv.neg <- data.frame(statistic = "pv.neg", est = trval$est, 
                       lower = trval$lower, upper = trval$upper)
  lrpos.est <- (a/M1)/(1 - (d/M0))
  lrpos.low <- exp(log(lrpos.est) - z * sqrt((1 - se$est)/(M1 * 
                                                             se$est) + (sp$est)/(M0 * (1 - sp$est))))
  lrpos.up <- exp(log(lrpos.est) + z * sqrt((1 - se$est)/(M1 * 
                                                            se$est) + (sp$est)/(M0 * (1 - sp$est))))
  lr.pos <- data.frame(statistic = "lr.pos", est = lrpos.est, 
                       lower = lrpos.low, upper = lrpos.up)
  lrneg.est <- (1 - (a/M1))/(d/M0)
  lrneg.low <- exp(log(lrneg.est) - z * sqrt((se$est)/(M1 * 
                                                         (1 - se$est)) + (1 - sp$est)/(M0 * (sp$est))))
  lrneg.up <- exp(log(lrneg.est) + z * sqrt((se$est)/(M1 * 
                                                        (1 - se$est)) + (1 - sp$est)/(M0 * (sp$est))))
  lr.neg <- data.frame(statistic = "lr.neg", est = lrneg.est, 
                       lower = lrneg.low, upper = lrneg.up)
  tdat <- as.matrix(cbind((a + d), total))
  if (method == "exact") {
    trval <- zexact(tdat, conf.level)
  }
  if (method == "wilson") {
    trval <- zwilson(tdat, conf.level)
  }
  if (method == "agresti") {
    trval <- zagresti(tdat, conf.level)
  }
  if (method == "clopper-pearson") {
    trval <- zclopperpearson(tdat, conf.level)
  }
  if (method == "jeffreys") {
    trval <- zjeffreys(tdat, conf.level)
  }
  diag.ac <- data.frame(statistic = "diag.ac", est = trval$est, 
                        lower = trval$lower, upper = trval$upper)
  dOR.p <- (a * d)/(b * c)
  lndOR <- log(dOR.p)
  lndOR.var <- 1/a + 1/b + 1/c + 1/d
  lndOR.se <- sqrt(1/a + 1/b + 1/c + 1/d)
  lndOR.l <- lndOR - (z * lndOR.se)
  lndOR.u <- lndOR + (z * lndOR.se)
  dOR.se <- exp(lndOR.se)
  dOR.low <- exp(lndOR.l)
  dOR.up <- exp(lndOR.u)
  diag.or <- data.frame(statistic = "diag.or", est = dOR.p, 
                        lower = dOR.low, upper = dOR.up)
  nndx.est <- 1/(se$est - (1 - sp$est))
  nndx.1 <- 1/(se$lower - (1 - sp$lower))
  nndx.2 <- 1/(se$upper - (1 - sp$upper))
  nndx.low <- min(nndx.1, nndx.2)
  nndx.up <- max(nndx.1, nndx.2)
  nndx <- data.frame(statistic = "nndx", est = nndx.est, lower = nndx.low, 
                     upper = nndx.up)
  c.p <- se$est - (1 - sp$est)
  c.1 <- se$lower - (1 - sp$lower)
  c.2 <- se$upper - (1 - sp$upper)
  c.low <- min(c.1, c.2)
  c.up <- max(c.1, c.2)
  youden <- data.frame(statistic = "youden", est = c.p, lower = c.low, 
                       upper = c.up)
  tdat <- as.matrix(cbind((c + d), total))
  if (method == "exact") {
    trval <- zexact(tdat, conf.level)
  }
  if (method == "wilson") {
    trval <- zwilson(tdat, conf.level)
  }
  if (method == "agresti") {
    trval <- zagresti(tdat, conf.level)
  }
  if (method == "clopper-pearson") {
    trval <- zclopperpearson(tdat, conf.level)
  }
  if (method == "jeffreys") {
    trval <- zjeffreys(tdat, conf.level)
  }
  p.rout <- data.frame(statistic = "p.rout", est = trval$est, 
                       lower = trval$lower, upper = trval$upper)
  tdat <- as.matrix(cbind((a + b), total))
  if (method == "exact") {
    trval <- zexact(tdat, conf.level)
  }
  if (method == "wilson") {
    trval <- zwilson(tdat, conf.level)
  }
  if (method == "agresti") {
    trval <- zagresti(tdat, conf.level)
  }
  if (method == "clopper-pearson") {
    trval <- zclopperpearson(tdat, conf.level)
  }
  if (method == "jeffreys") {
    trval <- zjeffreys(tdat, conf.level)
  }
  p.rin <- data.frame(statistic = "p.rin", est = trval$est, 
                      lower = trval$lower, upper = trval$upper)
  tdat <- as.matrix(cbind(b, M0))
  if (method == "exact") {
    trval <- zexact(tdat, conf.level)
  }
  if (method == "wilson") {
    trval <- zwilson(tdat, conf.level)
  }
  if (method == "agresti") {
    trval <- zagresti(tdat, conf.level)
  }
  if (method == "clopper-pearson") {
    trval <- zclopperpearson(tdat, conf.level)
  }
  if (method == "jeffreys") {
    trval <- zjeffreys(tdat, conf.level)
  }
  p.fpos <- data.frame(statistic = "p.fpos", est = trval$est, 
                       lower = trval$lower, upper = trval$upper)
  p.tpdn <- data.frame(statistic = "p.tpdn", est = trval$est, 
                       lower = trval$lower, upper = trval$upper)
  tdat <- as.matrix(cbind(c, M1))
  if (method == "exact") {
    trval <- zexact(tdat, conf.level)
  }
  if (method == "wilson") {
    trval <- zwilson(tdat, conf.level)
  }
  if (method == "agresti") {
    trval <- zagresti(tdat, conf.level)
  }
  if (method == "clopper-pearson") {
    trval <- zclopperpearson(tdat, conf.level)
  }
  if (method == "jeffreys") {
    trval <- zjeffreys(tdat, conf.level)
  }
  p.tndp <- data.frame(statistic = "p.tndp", est = trval$est, 
                       lower = trval$lower, upper = trval$upper)
  tdat <- as.matrix(cbind(b, N1))
  if (method == "exact") {
    trval <- zexact(tdat, conf.level)
  }
  if (method == "wilson") {
    trval <- zwilson(tdat, conf.level)
  }
  if (method == "agresti") {
    trval <- zagresti(tdat, conf.level)
  }
  if (method == "clopper-pearson") {
    trval <- zclopperpearson(tdat, conf.level)
  }
  if (method == "jeffreys") {
    trval <- zjeffreys(tdat, conf.level)
  }
  p.dntp <- data.frame(statistic = "p.dntp", est = trval$est, 
                       lower = trval$lower, upper = trval$upper)
  tdat <- as.matrix(cbind(c, N0))
  if (method == "exact") {
    trval <- zexact(tdat, conf.level)
  }
  if (method == "wilson") {
    trval <- zwilson(tdat, conf.level)
  }
  if (method == "agresti") {
    trval <- zagresti(tdat, conf.level)
  }
  if (method == "clopper-pearson") {
    trval <- zclopperpearson(tdat, conf.level)
  }
  if (method == "jeffreys") {
    trval <- zjeffreys(tdat, conf.level)
  }
  p.dptn <- data.frame(statistic = "p.dptn", est = trval$est, 
                       lower = trval$lower, upper = trval$upper)
  rval.df <- rbind(ap, tp, se, sp, diag.ac, diag.or, nndx, 
                   youden, pv.pos, pv.neg, lr.pos, lr.neg, p.rout, p.rin, 
                   p.tpdn, p.tndp, p.dntp, p.dptn)
  r1 <- c(a, b, N1)
  r2 <- c(c, d, N0)
  r3 <- c(M1, M0, M0 + M1)
  tab <- as.data.frame(rbind(r1, r2, r3))
  colnames(tab) <- c("   Outcome +", "   Outcome -", "     Total")
  rownames(tab) <- c("Test +", "Test -", "Total")
  tab <- format.data.frame(tab, digits = 3, justify = "right")
  out <- list(detail = rval.df, tab = tab, method = method, 
              digits = digits, conf.level = conf.level)
  class(out) <- "epi.tests"
  return(out)
}


zexact <- function(dat, conf.level){
  # Exact binomial confidence limits from function binom::binom.confint. Changed 190716.
  
  alpha <- 1 - conf.level
  alpha2 <- 0.5 * alpha
  a <- dat[,1]
  n <- dat[,2]
  
  p <- a / n
  a1 <- a == 0
  a2 <- a == n
  lb <- ub <- a
  lb[a1] <- 1
  ub[a2] <- n[a2] - 1
  low <- 1 - qbeta(1 - alpha2, n + 1 - a, lb)
  upp <- 1 - qbeta(alpha2, n - ub, a + 1)
  
  if (any(a1)) 
    low[a1] <- rep(0, sum(a1))
  
  if (any(a2)) 
    upp[a2] <- rep(1, sum(a2))
  
  rval <- data.frame(est = p, lower = low, upper = upp)
  rval
}

#Get diagnostic statistics
#==========================
basemod <- glm('case~abdo_pain+sym_age+grs+rectal_bloodloss+change_bowel_habit+sex', p_v_train$p_40_ve, family='binomial')
#each participant's probability of being a case:
predicted_probs <- predict(basemod, newdata = p_v_test$p_40_ve, type = "response")

# this function takes a level and outputs the spec, sens, ppv, npv, youdens
formatepi=function(level,frame,prob){
  predicted.classes <- prob > level
  atab=sum(predicted.classes==TRUE & frame$case==TRUE)
  btab=sum(predicted.classes==TRUE & frame$case==FALSE)
  ctab=sum(predicted.classes==FALSE & frame$case==TRUE)
  dtab=sum(predicted.classes==FALSE & frame$case==FALSE)
  data <- as.table(matrix(c(atab,btab,ctab,dtab), nrow = 2, byrow = TRUE))
  rval <- epi.tests(data, conf.level = 0.95)
  df=data.frame(rval$detail)
  out=data.frame(threshold=level*100,
                 specificity=df[df$statistic=='sp',2],
                 sensitivity=df[df$statistic=='se',2],
                 PPV=df[df$statistic=='pv.pos',2],
                 NPV=df[df$statistic=='pv.neg',2],
                 Youdens=df[df$statistic=='youden',2])
  return(out%>%round(3))
}

#Make table
#==========
epistats=rbind(
  formatepi(0.005896726,p_v_test$p_40_ve,predicted_probs),
  formatepi(0.01,p_v_test$p_40_ve,predicted_probs),
  formatepi(0.02,p_v_test$p_40_ve,predicted_probs),
  formatepi(0.03,p_v_test$p_40_ve,predicted_probs),
  formatepi(0.04,p_v_test$p_40_ve,predicted_probs),
  formatepi(0.05,p_v_test$p_40_ve,predicted_probs))

formatepi(0.01,p_v_test$p_40_ve,predicted_probs)
