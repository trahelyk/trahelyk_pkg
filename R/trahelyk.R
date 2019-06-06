options(stringsAsFactors = FALSE)
default.stringsAsFactors()

# Generate n NA values in a vector
batman <- function(n) {
  return(rep(NA, n))
}

#' Calculate a standard error
#' 
#' @param x A numeric vector
#' @param s A standard deviation
#' @param n N corresponding to s
#' @
#' @return Standard error of the mean (numeric)

sem <- function(x=NA, s=NA, n=NA) {
  if(length(x) > 1) {
    sem <- sd(x)/sqrt(length(x))
  } else {
    if(is.na(s) | is.na(n)) stop("Must provide a numeric vector or s and n.")
    sem <- s/sqrt(n)
  }
  return(sem)
}

#' Calculate a 95% CI and return it as a formatted string
#' 
#' @param x A numeric or integer vector
#' @param d An integer specifying the number of significant digits to be presented in the point estimate and interval.
#' @param dist A character string specifying the distribution from which the interval should be calculated. Currently supports "normal" or "t".
#' @return A character string with the point estimate of the mean, followed by the interval in parentheses.

ci_fmt <- function(x, d=2, dist="normal") {
  if(dist=="normal") {
    dist_cut <- c(qnorm(0.975))
  } else if(dist=="t") {
    dist_cut <- c(qt(0.975, df=n-1))
  }
  
  n <- length(x)
  se <- sd(x)/sqrt(n)
  xbar <- mean(x, na.rm=TRUE) 
  return(paste0(rnd(xbar, d), " (", 
                rnd(xbar - dist_cut*se, d), ", ",
                rnd(xbar + dist_cut*se, d), ")"))
}


# Present a specified odds ratio from a glm object as a character string (for use in narrative)
present_or <- function(mdl, varname, d=2, fmt="noparens") {
  est <- rnd(exp(coef(mdl)[varname]), d)
  ci <- as.tibble(t(exp(confint.default(mdl))))[[varname]]
  lb <- rnd(ci[1], d)
  ub <- rnd(ci[2], d)
  if(fmt=="parens") {
    x <- paste0(est, " (95\\% CI: ", lb, ", ", ub, ")")
  } else {
    x <- paste0(est, ", 95\\% CI: ", lb, ", ", ub)
  }
  return(x)
}

# -------------------------------------------------------------------------------- 
# Reports a 95% CI for model coefficients. Ideal for in-line R code in a knitr document.
# Allows the user to specify confidence level and whether results should be exponentiated.
# --------------------------------------------------------------------------------
# Define a generic function
inline_ci <- function(mdl, parm, sep="paren", digits=2, exp=FALSE, level=0.95) UseMethod("inline_ci")

inline_ci.default <- function(mdl, parm, sep="paren", digits=2, exp=FALSE, level=0.95) {
  if(sep=="paren") {
    seps <- c(" (", ")")
  } else {
    seps <- c(", ", "")
  }
  if(exp)
  {
    return(paste0(as.character(rnd(exp(mdl$coefficients[[parm]]),digits)), 
                  seps[1], "95\\% CI: ",
                  as.character(rnd(exp(confint(mdl, parm=parm, level=level))[1],digits)),
                  ", ",
                  as.character(rnd(exp(confint(mdl, parm=parm, level=level))[2],digits)),
                  seps[2]))
  }
  else
  {
    return(paste0(as.character(rnd(mdl$coefficients[[parm]],digits)), 
                  seps[1], "95\\% CI: ",
                  as.character(rnd(confint(mdl, parm=parm, level=level)[1],digits)),
                  ", ",
                  as.character(rnd(confint(mdl, parm=parm, level=level)[2],digits)),
                  seps[2]))
  }
}

inline_ci.lme <- function(mdl, parm, sep="paren", digits=2, exp=FALSE, level=0.95) {
  if(sep=="paren") {
    seps <- c(", (", ")")
  } else {
    seps <- c(", ", "")
  }
  coefs <- intervals(mdl, which="fixed")$fixed
  coefs <- bind_cols(tibble(var = rownames(coefs)), as.tibble(coefs))
  coefs %<>%
    gather(key = est, value = value, 2:ncol(coefs)) %>% 
    spread_(key = names(coefs)[1],value = 'value')
  
  if(exp)
  {
    return(paste0(as.character(rnd(exp(coefs[[parm]][1]),digits)), 
                  seps[1], "95\\% CI: ",
                  as.character(rnd(exp(coefs[[parm]][2]),digits)),
                  ", ",
                  as.character(rnd(exp(coefs[[parm]][3]),digits)),
                  seps[2]))
  }
  else
  {
    return(paste0(as.character(rnd(coefs[[parm]][1],digits)), 
                  seps[1], "95\\% CI: ",
                  as.character(rnd(coefs[[parm]][2],digits)),
                  ", ",
                  as.character(rnd(coefs[[parm]][3],digits)),
                  seps[2]))
  }
}

# Return all the column names of a data frame except for those specified
cols.except <- function(df, except) {
  colnames(df)[!(colnames(df) %in% except)]
}

# Return a 2-level factor with labels "No" and "Yes"
factorny <- function(x) {
  return(factor(x, labels = c("No", "Yes")))
}

# Define laglead function
laglead<-function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  #stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}


#' Apply Hmisc-brand labels to all variables in a dataframe or tibble
#' 
#' @param df A data frame or tibble.
#' @param labels A vector of quoted label of length ncol(df).
#' @return A data frame or tibble.
#' @examples
#' foo <- tibble(a = c(1,2,3),
#'               b = c(4, 5,6)) %>%
#'   apply_labels(c("A", "B"))
#' label(foo)
apply.labels <- function(df, labels) {
  for(i in 1:length(df)) {
    Hmisc::label(df[[i]]) <- labels[i]
  }
  return(df)
}

#' Apply an Hmisc-brand label to a single variable in a dataframe or tibble
#' 
#' @param df A data frame or tibble.
#' @param x Unquoted name of the column to be labeled. 
#' @param lbl Quoted label.
#' @return A data frame or tibble.
#' @examples
#' foo <- tibble(a = c(1,2,3),
#'               b = c(4, 5,6)) %>%
#'   apply_label(a, "A") %>%
#'   apply_label(b, "B")
#'   
#' label(foo)
apply_label <- function(df, x, lbl) {
  label(df[[deparse(substitute(x))]]) <- lbl
  return(df)
}

# Replace a particular character in an entire data frame
replace.df <- function(df, find, replace="") {
  for(col in colnames(df)) {
    df[[col]] <- gsub(find, replace, df[[col]])
  }
  return(df)
}

# Clean up columns with "" values
clean.cols <- function(df, na.text="") {
  for(col in colnames(df)) {
    df[[col]][df[[col]]==na.text] <- NA
  }
  return(df)
}

# Count distinct observations in a vector
cnt.distinct <- function(x) {
  return(length(unique(x)))
}

# Search column names of a data frame for specified string
grepcol <- function(phrase, df) {
  return(colnames(df)[grep(phrase, colnames(df), ignore.case=TRUE)])
}

# A simple rounding function that returns exactly the number of digits requested. 
# Defaults to a character string.
# This function is really just a wrapper for format and round. 
rnd <- function(x,d=3,as.numeric=FALSE) {
  if(as.numeric)
    return(as.numeric(format(round(x,d), nsmall=d, scientific=FALSE)))
  else
    return(format(round(x,d), nsmall=d, scientific=FALSE))
}

# Returns a character-formatted version of a p-value, including markup
# to indicate when p is less than the minimum value in the specified number
# of decimal-place digits.
fmt.pval <- function(pval, digits=2, include.p=TRUE, latex=FALSE, md=FALSE) {
  p.df <- as.data.frame(cbind(1:length(pval), pval))
  colnames(p.df) <- c("order", "p")
  if(latex) {
    lt <- "\\textless"
  } else {
    lt <- "<"
  }
  if(md) {
    spc <- "\ "
  } else {
    spc <- " "
  }
  if(include.p) {
    prefix.1 <- paste0("p", spc, lt, spc)
    prefix.2 <- paste0("p", spc, "=", spc)
  }
  else{
    prefix.1 <- lt
    prefix.2 <- ""
  }
  p.df[p.df$p*(10^(digits)) < 1 & !is.na(p.df$p),c("p.fmt")] <- paste0(prefix.1, format(1/(10^digits), scientific=FALSE))
  p.df[p.df$p*(10^(digits)) >= 1 & !is.na(p.df$p),c("p.fmt")] <- paste0(prefix.2, 
                                                                       as.character(rnd(p.df$p[p.df$p*(10^(digits)) >= 1 & !is.na(p.df$p)],digits)))
  p.df[is.na(p.df$p),c("p.fmt")] <- ""
  
  return(p.df$p.fmt)
}

# Convert proportion (0 < p < 1) to a percent (0 < pct < 100)
fmt.pct <- function(x, d=0, as.numeric=FALSE, latex=FALSE) {
  if(latex) pct.char <- "\\%"
  else pct.char <- "%"
  if(as.numeric)
    return(as.numeric(format(round(x*100,d), nsmall=d)))
  else
    return(paste0(format(round(x*100,d), nsmall=d), pct.char))
}

# Function for comparing models to check for confounding.
### SHOULD MODIFY THIS FUNCTION TO INCLUDE P-VALUES
compare.model <- function(mdl.0, mdl.1, cnfd.level=0.2) {
  comp <- merge(cbind(row=rownames(as.data.frame(mdl.0$coef)), coef=as.data.frame(mdl.0$coef), stringsAsFactors = FALSE), 
                cbind(row=rownames(as.data.frame(mdl.1$coef)), coef=as.data.frame(mdl.1$coef), stringsAsFactors = FALSE), 
                by="row", all=TRUE)
  comp$reldiff <- comp[,2] / comp[,3]
  comp$confound <- ""
  comp$confound[(1-comp$reldiff > cnfd.level) | (comp$reldiff-1 > cnfd.level)] <- "*"
  comp <- rbind(comp, c("AIC", AIC(mdl.0), AIC(mdl.1), AIC(mdl.0) - AIC(mdl.1), ""))
  return(comp)
}

# Produces a series of univariable linear regression models and wraps them in LaTeX.
univ.lm <- function(y, x, df, rownames=NULL, caption="", digits=2, label="", asymp.nml=TRUE) {  
  univ.models <- data.frame(matrix(vector(), 0, 6))
  for(pred in x) {
    mdl <- lm(formula(paste0(y, " ~ ", pred)), data=df)
    if(asymp.nml) {
      ci <- confint.default(mdl)
    } else {
      ci <- confint(mdl)
    }
    univ.models <- rbind(univ.models, cbind(summary(mdl)$coef, ci))
  }
  univ.models <- univ.models[!grepl("(Intercept)", row.names(univ.models)),]
  colnames(univ.models) <- c("coef", "SE", "t", "p", "LB", "UB")
  univ.models <- univ.models[,c("coef", "SE", "LB", "UB", "p")]
  
  for (i in 1:length(univ.models$p)) {
    univ.models$p[i] <- fmt.pval(as.numeric(univ.models$p[i]), digits=digits, include.p=FALSE, latex=FALSE)
  }
  
  if(is.null(rownames)) rownames <- rownames(univ.models)
  
  xt.um <- xtable(univ.models, caption=caption, digits=digits, label=label)
  align(xt.um) <- "lrrrrr"
  rownames(xt.um) <- rownames
  print(xt.um,
        include.rownames=TRUE,
        caption.placement="top",
        floating=TRUE,
        table.placement="H")
}


##################################################################################################
# Functions for survival analysis
##################################################################################################

# Formats estimates of survival probability at specified intervals.
survest <- function(sf, times, caption="-year survival probability", label="survtime") {
  for(i in 1:length(times)) {
    foo <- summary(sf, time=times[i])
    latex(data.frame(strata = foo$strata[],
                     time = foo$time,
                     n.risk = foo$n.risk,
                     n.event = foo$n.event,
                     survival.probability = foo$surv,
                     lowerCI = foo$lower,
                     upperCI = foo$upper), 
          digits=3, caption=paste0(times[i], caption), 
          label=paste0(label, times[i]),
          file="", 
          rowname=NULL, size="footnotesize", where="!htbp")  
  }
}

tidy.survest <- function(sf, times, caption="-year survival probability", label="survtime") {
  outt <- data.frame()
  for(i in 1:length(times)) {
    x <- summary(sf, time=times[i], extend=TRUE)
    outt <- rbind(outt, data.frame(strata = x$strata[],
                                   time = x$time,
                                   n.risk = x$n.risk,
                                   n.event = x$n.event,
                                   survival.probability = x$surv,
                                   lowerCI = x$lower,
                                   upperCI = x$upper))
  }
  return(outt)
}

# Produces a series of univariate Cox PH regression models and returns a tidy dataset.
tidy.univ.coxph <- function(timevar, death.indicator, preds, df, rownames=NULL, digits=2) {  
  univ.models <- data.frame(matrix(vector(), 0, 8))
  for(pred in preds) {
    surv.obj <- Surv(df[[timevar]], df[[death.indicator]])
    mdl <- coxph(formula(paste0("surv.obj ~ ", pred)), data=df)
    univ.models <- rbind(univ.models, cbind(n=mdl$n, summary(mdl)$coef, exp(confint(mdl))))
  }
  univ.models <- univ.models[!grepl("(Intercept)", row.names(univ.models)),]
  colnames(univ.models) <- c("n", "coef", "HR", "SE", "z", "p", "LB", "UB")
  univ.models <- univ.models[,c("n", "coef", "SE", "HR", "LB", "UB", "p")]
  
  for (i in 1:length(univ.models$p)) {
    univ.models$p[i] <- fmt.pval(as.numeric(univ.models$p[i]), digits=digits, include.p=FALSE, latex=FALSE)
  }
  
  if(is.null(rownames)) rownames <- rownames(univ.models)
  
  rownames(univ.models) <- rownames
  
  return(univ.models)
}

# Produces a series of univariate Cox PH regression models and wraps them in LaTeX.
univ.coxph <- function(timevar, death.indicator, preds, df, rownames=NULL, caption="", digits=2, label="", size="footnotesize") {  
  univ.models <- tidy.univ.coxph(timevar = timevar,
                                 death.indicator = death.indicator,
                                 preds = preds,
                                 df = df,
                                 rownames = rownames,
                                 digits = digits)
  
  xt.um <- xtable(univ.models, caption=caption, digits=c(0, 0, rep(digits, 6)), label=label)
  align(xt.um) <- "lrrrrrrr"
  rownames(xt.um) <- rownames
  print(xt.um,
        include.rownames=TRUE,
        caption.placement="top",
        floating=TRUE,
        table.placement="H",
        size=size)
}

# Produces a series of univariate Cox PH regression models on a multiply imputed dataset and wraps them in LaTeX.
univ.coxph.mids <- function(timevar, death.indicator, preds, mids, rownames=NULL, caption="", digits=2, label="", size="footnotesize", hl.col="red") {  
  univ.models <- data.frame(matrix(vector(), 0, 8))
  for(i in 1:length(preds)) {
    eval(parse(text=paste0("mdl <- pool(with(mids, coxph(Surv(mids$data[['", timevar, "']], mids$data[['", death.indicator, "']]) ~ ", preds[i], ")))")))
    mdl.summary <- as.data.frame(summary(mdl))
    univ.models <- rbind(univ.models, cbind(var=rownames(mdl.summary),
                                            Missing=mdl.summary$nmis, 
                                            ubar=rnd(mdl.summary$est, d=digits), 
                                            SE=rnd(mdl.summary$se, d=digits),
                                            HR=rnd(exp(mdl.summary$est), d=digits),
                                            LB=rnd(exp(mdl.summary$`lo 95`), d=digits),
                                            UB=rnd(exp(mdl.summary$`hi 95`), d=digits),
                                            p=fmt.pval(mdl.summary$`Pr(>|t|)`, digits=digits, include.p=FALSE, latex=TRUE),
                                            p.num=as.numeric(mdl.summary$`Pr(>|t|)`)))
  }
  colnames(univ.models) <- c("Variable", "Missing", "$\\bar{u}$", "SE", "HR", "LB", "UB", "p", "p.num")
  
  if(!is.null(rownames)) univ.models$Variable <- rownames
  
  for(colname in colnames(univ.models)) {
    univ.models[[colname]][as.numeric(univ.models$p.num)<0.05] <- 
      paste0("\\textcolor{", hl.col, "}{\\bf ", univ.models[[colname]][as.numeric(univ.models$p.num)<0.05], "}")
  }
  
  univ.models$p.num <- NULL
  univ.models$Missing[univ.models$Missing==paste0("\\textcolor{", hl.col, "}{\\bf NA}")] <- NA
  
  # if(is.null(rownames)) rownames <- univ.models$Variable

  xt.um <- xtable(univ.models[2:8], caption=caption, digits=c(0, 0, rep(digits, 6)), label=label)
  align(xt.um) <- "lrrrrrrr"
  rownames(xt.um) <- univ.models$Variable
  print(xt.um,
        include.rownames=TRUE,
        caption.placement="top",
        floating=TRUE,
        table.placement="H",
        size=size,
        sanitize.text.function = identity)
}

##################################################################################################
# Date manipulation functions
##################################################################################################
year.length <- function(year) {
  return(nchar(substr(ca$DOB, as.numeric(regexpr("/[^/]*$", ca$DOB))+1, nchar(ca$DOB))))
}

fix.Date <- function(x, fmt=c("%m/%d/%Y"), origin="1970-01-01") {
  
  if(is.character(x)) {
    fixed.date <- as.Date(parse_date_time(x, fmt))
  }
  else if(is.numeric(x)) {
    fixed.date <- as.Date(x, origin=origin)
  }
  else {
    fixed.date <- as.Date(NA)
  }
  class(fixed.date) <- "Date"
  return(as.Date(fixed.date))
}

fix.mixed.dates <- function(dates) {
  short <- as.Date(dates[year.length(dates)==4], format="%m/%d/%Y", origin="1970-01-01")
  long <- as.Date(dates[year.length(dates)==2], format="%m/%d/%y", origin="1970-01-01")
  return(c(short,long))
}

##################################################################################################
# Generate a random alphanumeric string of length n. These are not necessarily unique.
##################################################################################################
an.id <- function(n) {
  return(paste(replicate(n, c(letters, round(runif(26,0,9)))[round(runif(1,1,52))]), collapse=""))
}
