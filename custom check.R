else if (p == 17) {
  if (length(dates) != 19) {
    stop("For a 1-fleet 17P model 'dates' must be a vector of length 19")
  }
  if (distr == "apnormal" | distr == "aplnormal" | 
      distr == "poisson") {
    if (length(par) != 22) {
      stop("For a 1-fleet 17 perturbation fit with poisson or adjusted profile likelihood par must be a vector of length 22")
    }
  }
  if (distr == "normal" | distr == "lognormal" | distr == 
      "negbin" | distr == "gamma" | distr == "roblognormal" | 
      distr == "gumbel") {
    if (length(par) != 23) {
      stop("For a 1-fleet 17 perturbation fit with negative binomial, normal, lognormal, gamma, robust lognormal or gumbel distribution, par mustbe a vector of length 23")
    }
  }
  results1 <- do.call(optimx, list(par = par, fn = eval(as.name(paste(".CDMN", 
                                                                      as.character(p), "P", sep = ""))), gr = NULL, 
                                   dates = dates, obseff1 = x$Data[[fleet.name]][, 
                                                                                 2], obscat1 = x$Data[[fleet.name]][, 5], distr = distr, 
                                   method = method, lower = -Inf, upper = Inf, 
                                   control = list(), hessian = hessian, itnmax = itnmax, 
                                   output = "estimate"))
  results2 <- vector("list", length(method))
  names(results2) <- method
  temp <- attr(results1, "details")
  for (i in 1:length(method)) {
    results2[[i]]$Type <- p
    results2[[i]]$Dates <- dates
    results2[[i]]$Distr <- distr
    names(results2[[i]]$Dates) <- c("ts.start", 
                                    "ts.P1", "ts.P2", "ts.P3", "ts.P4", "ts.P5", 
                                    "ts.P6", "ts.P7", "ts.P8", "ts.P9", "ts.P10", 
                                    "ts.P11", "ts.P12", "ts.P13", "ts.P14", "ts.P15", 
                                    "ts.P16", "ts.P17", "ts.end")
    par.names <- c("M", "N0", paste(c("P1.", "P2.", 
                                      "P3.", "P4.", "P5.", "P6.", "P7.", "P8.", 
                                      "P9.", "P10.", "P11.", "P12.", "P13.", "P14.", 
                                      "P15.", "P16.", "P17.", "k.", "alpha.", "beta."), 
                                    sort(rep(fleet.name, 3 + p)), sep = ""))
    if (distr == "gamma" | distr == "normal" | distr == 
        "lognormal" | distr == "negbin") {
      par.names <- c(par.names, paste("psi.", fleet.name, 
                                      sep = ""))
    }
    results2[[i]]$converg <- "FAIL"
    results2[[i]]$kkt <- NA
    results2[[i]]$AIC <- NA
    results2[[i]]$bt.par <- NA
    results2[[i]]$num.grads <- NA
    results2[[i]]$bt.stdev <- rep(NA, length(par.names))
    results2[[i]]$Cor <- matrix(NA, length(par.names), 
                                length(par.names))
    if (length(temp[i, ]$ngatend) == length(par.names) & 
        !any(is.na(temp[i, ]$nhatend)) & 1/kappa(temp[i, 
        ]$nhatend) > 1e-15) {
      results2[[i]]$converg <- results1[i, length(par) + 
                                          5]
      results2[[i]]$kkt <- results1[i, (length(par) + 
                                          6):(length(par) + 7)]
      results2[[i]]$AIC <- 2 * length(par) + 2 * 
        results1[i, length(par) + 1]
      results2[[i]]$bt.par <- exp(results1[i, 1:length(par)])
      results2[[i]]$num.grads <- temp[i, ]$ngatend
      v <- matrix(0, length(par.names), length(par.names))
      if (distr == "poisson" | distr == "apnormal" | 
          distr == "aplnormal") {
        v <- deltamethod(g = list(~exp(x1), ~exp(x2), 
                                  ~exp(x3), ~exp(x4), ~exp(x5), ~exp(x6), 
                                  ~exp(x7), ~exp(x8), ~exp(x9), ~exp(x10), 
                                  ~exp(x11), ~exp(x12), ~exp(x13), ~exp(x14), 
                                  ~exp(x15), ~exp(x16), ~exp(x17), ~exp(x18), 
                                  ~exp(x19), ~exp(x20), ~exp(x21), ~exp(x22)), 
                         mean = as.numeric(results1[i, 1:length(par)]), 
                         cov = try(solve(temp[i, ]$nhatend)), ses = FALSE)
      }
      else {
        v <- deltamethod(g = list(~exp(x1), ~exp(x2), 
                                  ~exp(x3), ~exp(x4), ~exp(x5), ~exp(x6), 
                                  ~exp(x7), ~exp(x8), ~exp(x9), ~exp(x10), 
                                  ~exp(x11), ~exp(x12), ~exp(x13), ~exp(x14), 
                                  ~exp(x15), ~exp(x16), ~exp(x17), ~exp(x18), 
                                  ~exp(x19), ~exp(x20), ~exp(x21), ~exp(x22), 
                                  ~exp(x23)), mean = as.numeric(results1[i, 
                                                                         1:length(par)]), cov = try(solve(temp[i, 
                                                                         ]$nhatend)), ses = FALSE)
      }
      results2[[i]]$bt.stdev <- sqrt(diag(v))
      results2[[i]]$Cor <- cor(v)
      names(results2[[i]]$num.grads) <- par.names
      names(results2[[i]]$bt.par) <- par.names
      names(results2[[i]]$bt.stdev) <- par.names
      colnames(results2[[i]]$Cor) <- par.names
      rownames(results2[[i]]$Cor) <- par.names
    }
  }
  rm(v)
  rm(temp)
}