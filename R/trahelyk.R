options(stringsAsFactors = FALSE)
default.stringsAsFactors()

# Define root directory and set current directory
current.dir <- function() {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}


# Install a package if it's not already installed
lib <- function(pkg) {
  if (!(pkg %in% installed.packages())) install.packages(pkg)
  eval(paste0("library(", pkg, ")"))
}


not.in <- function(newlist, df) {
  return(newlist[!(newlist %in% colnames(df))])
}

# Generate a new empty data fram with the specified number of columns.
new.df <- function(cols) {
  data.frame(matrix(vector(), 
                    0, cols, 
                    dimnames=list(c(), 
                                  c(1:cols))), 
             stringsAsFactors=F)
}

factorny <- function(x) {
  return(factor(x, labels = c("No", "Yes")))
}

chunk_opts <- function(prefix="") {
  options(stringsAsFactors = FALSE)
  prefix = prefix
  #global chunk options
  #   opts_knit[["set"]](eval.after = "fig.cap",
  #                      cache.extra = c(dep_files,
  #                                      sapply(dep_files, function(f) {file.info(f)[["mtime"]]})))
  opts_knit[["set"]](eval.after = "fig.cap")
  opts_chunk[["set"]](cache.path = paste0("cache/", prefix, "-"),
                      # output.dir = ".",
                      fig.path = paste0("figure/", prefix, "-"),
                      cache = FALSE,
                      autodep = TRUE,
                      dev = "pdf",
                      fig.align = "center",
                      fig.pos = "H",
                      tidy = FALSE,
                      echo = FALSE,
                      results = "asis")
  dep_auto()
}

# Produce a table 1 using summary.formula from Hmisc.
table1 <- function(y, df, catTest=catTestchisq, size="footnotesize", caption="", label="table1", digits=3, latex=TRUE, continuous=7) {
  t1 <- summary(formula(paste0(y, " ~ .")),
                data = df,
                method = "reverse",
                continuous = continuous,
                catTest = catTest,
                test = TRUE,
                overall = TRUE)
  if(latex) {
    latex(t1,
          digits = digits,
          exclude1 = FALSE,
          long = TRUE,
          size = size,
          longtable = TRUE,
          lines.page = Inf,
          caption = caption,
          prtest = "P",
          label = label,
          file = "")
  }
  else {
    t1
  }
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



#Apply lables to a dataframe
apply.labels <- function(df, labels) {
  for(i in 1:length(df)) {
    label(df[[i]]) <- labels[i]
  }
  return(df)
}

#Cross-tabulation for latex output
# crossTab <- function(x,y, df, caption, label, catTest = catTestFisher) {
#   latex(summary(formula(paste0(y, "~ .")),
#                 data = df[,c(x,y)],
#                 method = "reverse",
#                 continuous = 7,
#                 catTest = catTest,
#                 p.simulate = p.simulate,
#                 test = TRUE,
#                 overall = TRUE),
#         prmsd=FALSE,
#         digits = 3,
#         exclude1 = FALSE,
#         long = FALSE,
#         size = "footnotesize",
#         longtable = TRUE,
#         lines.page = Inf,
#         caption = caption,
#         prtest = "P",
#         label = label,
#         file = "")
# }


################################################################################################################################
crossTab <- function(x, y, xlab="", ylab="", x.names=NA, y.names=NA, caption="", label="", size="footnotesize") {
  # x.names <- y.names <- NA
  # y <- ln$inst.recat
  # x <- ln$dn.harvested
  # ylab <- "PSLNs harvested"
  # xlab <- "site"
  if(is.na(x.names)) {
    x.names <- names(table(x))
  }
  if(is.na(y.names)) {
    y.names <- names(table(y))
  }
  tab <- table(x, y)
  
  out <- new.df(cols=length(names(table(y)))+2)
  # names(out) <- c(xlab, y.names, "~")
  out <- rbind.data.frame(out, c(xlab, y.names, "~"))
  
  for (i in 1:(length(x.names))) {  
    newcol.txt <- ""
    rowsum <- 0
    for (j in 1:length(y.names)) {
      newcol.txt <- paste0(newcol.txt, ", tab[", i, ", ", j, "]")
      rowsum <- rowsum + tab[i,j]
    }
    eval(parse(text=paste0("newrow <- data.frame(cbind(x.names[i]", newcol.txt, ", rowsum))")))
    names(newrow) <- names(out)
    out <- rbind(out, newrow)
  }
  
  # Calculate colsums
  colsums.txt <- ""
  grandtotal <- 0
  for(j in 1:length(y.names)) {
    colsum <- 0
    for(i in 1:length(x.names)) {
      colsum <- colsum + tab[i,j]
    }
    colsums.txt <- paste0(colsums.txt, ", ", colsum)
    grandtotal <- grandtotal + colsum
  }
  eval(parse(text=paste0("newrow <- data.frame(cbind('~'", colsums.txt, ", ", grandtotal, "))")))
  names(newrow) <- names(out)
  out <- rbind(out, newrow)
  
  # Add percentages
  for(i in 2:(length(x.names)+2)) {
    for(j in 2:(length(y.names)+1)) {
      out[i,j] <- paste0(out[i,j], " (", 
                         fmt.pct(as.numeric(out[i,j])/as.numeric(out[i,length(y.names)+2]), d=2, latex=TRUE), ")")
    }
  }
  
  # Generate the xtable object
  xt.out <- xtable(out, caption=caption, label=label)
  align(xt.out) <- paste0("ll", paste(rep("r", length(y.names)+1), collapse=""))
  
  # Configure the header
  addtorow <- list()
  addtorow$pos <- list(0)
  addtorow$command <- c(paste0("~ & \\multicolumn{", ncol(out)-2, "}{c}{{\\bf ", ylab, "}} & ~ \\\\\n"))
  
  # Print the table
  print(xt.out,
        include.rownames=FALSE,
        include.colnames=FALSE,
        caption.placement="top",
        floating=TRUE,
        table.placement="H",
        size=size,
        add.to.row=addtorow,
        hline.after=c(-1, 1, nrow(out)-1),
        sanitize.text.function = identity)
  
}

# crossTab(ln$dn.harvested, ln$sup.harvested,
#          xlab="PSLNs harvested", ylab="SSLNs harvested")
################################################################################################################################

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
  print(colnames(df)[grep(phrase, colnames(df), ignore.case=TRUE)])
}

# Rename a data frame
rename.df <- function(df) {
  foo <- df
  rm(df)
  return(foo)
}

# A simple rounding function that returns exactly the number of digits requested.
# This function is really just a wrapper for format and round. 
rnd <- function(x,d=3,as.numeric=FALSE) {
  if(as.numeric)
    return(as.numeric(format(round(x,d), nsmall=d)))
  else
    return(format(round(x,d), nsmall=d))
}

# Returns a character-formatted version of a p-value, including LaTeX markup
# to indicate when p is less than the minimum value in the specified number
# of decimal-place digits.
fmt.pval <- function(pval, digits=2, include.p=TRUE, latex=TRUE) {
  p.df <- as.data.frame(cbind(1:length(pval), pval))
  colnames(p.df) <- c("order", "p")
  if(latex) {
    lt <- "\\textless"
  }
  else{
    lt <- "<"
  }
  if(include.p) {
    prefix.1 <- paste0("p ",lt)
    prefix.2 <- "p ="
  }
  else{
    prefix.1 <- lt
    prefix.2 <- ""
  }
  p.df[p.df$p*(10^(digits)) < 1 & !is.na(p.df$p),c("p.fmt")] <- paste(prefix.1, as.character(1/(10^digits)))
  p.df[p.df$p*(10^(digits)) >= 1 & !is.na(p.df$p),c("p.fmt")] <- paste(prefix.2, 
                                                                       as.character(rnd(p.df$p[p.df$p*(10^(digits)) >= 1 & !is.na(p.df$p)],digits)))
  p.df[is.na(p.df$p),c("p.fmt")] <- ""
  
  return(p.df$p.fmt)
  #   if(pval*(10^(digits)) < 1)
  #     return(paste(prefix.1, as.character(1/(10^digits))))
  #   else
  #     return(paste(prefix.2, as.character(rnd(pval,digits))))
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

# Reports a 95% CI for model coefficients. Ideal for in-line R code in a knitr document.
# Allows the user to specify confidence level and whether results should be exponentiated.
inline.ci <- function(mdl, parm, digits=2, exp=FALSE, level=0.95) {
  if(exp)
  {
    return(paste0("OR: ",
                  as.character(rnd(exp(mdl$coefficients[[parm]]),digits)), 
                  ", 95\\% CI: ",
                  as.character(rnd(exp(confint(mdl, parm=parm, level=level))[1],digits)),
                  ", ",
                  as.character(rnd(exp(confint(mdl, parm=parm, level=level))[2],digits))))
  }
  else
  {
    return(paste0("OR: ",
                  as.character(rnd(mdl$coefficients[[parm]],digits)), 
                  ", 95\\% CI: ",
                  as.character(rnd(confint(mdl, parm=parm, level=level)[1],digits)),
                  ", ",
                  as.character(rnd(confint(mdl, parm=parm, level=level)[2],digits))))
  }
}

# Reports a 95% CI formatted to become part of a latex table
table.ci <- function(mdl, parm, digits=2, exp=FALSE, level=0.95, asymp.nml=FALSE) {
  if(asymp.nml) {
    ci <- confint.default(mdl, parm=parm, level=level)
  }
  else {
    ci <- confint(mdl, parm=parm, level=level) 
  }
  if(exp)
  {
    return(paste0(as.character(rnd(exp(mdl$coefficients[[parm]]),digits)), 
                  " (",
                  as.character(rnd(exp(ci)[1],digits)),
                  ", ",
                  as.character(rnd(exp(ci)[2],digits)),
                  ")"))
  }
  else
  {
    return(paste0(as.character(rnd(mdl$coefficients[[parm]],digits)), 
                  " (",
                  as.character(rnd(ci[1],digits)),
                  ", ",
                  as.character(rnd(ci[2],digits)),
                  ")"))
  }
}

# Summarize a linear regression model
lm.summary <- function(mdl, rownames=NULL, caption="", d=2, d.pval=3, latex=TRUE, label="") {
  if(is.null(rownames)) rownames = names(mdl$coef)[2:length(mdl$coef)]
  outt <- data.frame(matrix(vector(), 
                            0, 3, 
                            dimnames=list(c(), 
                                          c(1, 2, 3))), 
                     stringsAsFactors=F)
  
  for(i in 2:length(mdl$coef)) {
    outt <- rbind(outt,
                  cbind(rownames[i-1],
                        table.ci(mdl, parm=names(mdl$coef)[i], exp=FALSE, digits=d),
                        fmt.pval(summary(mdl)$coefficients[i,4], digits=d.pval)))
  }
  colnames(outt) <- c("", "Coefficient (95\\% CI)", "p-value")
  if(latex) {
    xt.outt <- xtable(outt, label=label)
    caption(xt.outt) <- caption
    align(xt.outt) <- "llrr"
    
    # Configure footer  
    footer <- paste0("\\hline \\multicolumn{1}{l}{{\\scriptsize \\em $R^2$ = ", rnd(summary(mdl)$r.squared, d=2), "}}")
    footer <- paste0(footer, "& \\multicolumn{2}{r}{{\\scriptsize \\em Adjusted $R^2$ = ", rnd(summary(mdl)$adj.r.squared, d=2), "}} \\\\ \n")
    footer <- paste0(footer, "\\multicolumn{1}{l}{{\\scriptsize \\em df = ", mdl$df.residual, "}}")
    footer <- paste0(footer, "& \\multicolumn{2}{r}{{\\scriptsize \\em F-test: p = ", rnd(1 - pf(2.34, 1, 23), d=3), "}} \\\\ \n")
    
    addtorow <- list()
    addtorow$pos <- list(nrow(xt.outt))
    addtorow$command <- c(footer)
    
    print(xt.outt,
          sanitize.text.function=function(x){x},
          caption.placement="top",
          table.placement="H",
          include.rownames=FALSE,
          add.to.row=addtorow,
          hline.after=c(-1, 0),
          floating=TRUE)
  }
  else {
    outt
  }
}

# Summarize the fixed effects from a linear mixed-effects model
lme.summary <- function(mdl, rownames=NULL, caption="", d=2, d.pval=3, latex=TRUE, label="") {
  if(is.null(rownames)) rownames = names(mdl$coefficients[["fixed"]])[2:length(mdl$coefficients[["fixed"]])]
  outt <- data.frame(matrix(vector(),
                            0, 3,
                            dimnames=list(c(),
                                          c(1, 2, 3))),
                     stringsAsFactors=F)

  for(i in 2:length(mdl$coefficients[["fixed"]])) {
    outt <- rbind(outt,
                  cbind(rownames[i-1],
                        paste0(as.character(rnd(intervals(mdl)$fixed[i,2], d=d)), " (",
                               as.character(rnd(intervals(mdl)$fixed[i,1], d=d)), ", ",
                               as.character(rnd(intervals(mdl)$fixed[i,3], d=d)), ")"),
                        fmt.pval(summary(mdl)$tTable[i,5], digits=d.pval)))
  }
  colnames(outt) <- c("", "Coefficient (95\\% CI)", "p-value")
  if(latex) {
    xt.outt <- xtable(outt, label=label)
    caption(xt.outt) <- caption
    align(xt.outt) <- "llrr"

    # Configure footer
    footer <- paste0("\\hline \\multicolumn{1}{l}{{\\scriptsize \\em N = ", summary(mdl)$dims$N, "}}")
    footer <- paste0(footer, "& \\multicolumn{2}{r}{{\\scriptsize \\em Groups = ", summary(mdl)$dims$ngrps[1], "}} \\\\ \n")
    footer <- paste0(footer, "\\multicolumn{1}{l}{{\\scriptsize \\em AIC = ", rnd(summary(mdl)$AIC, d=3), "}}")
    footer <- paste0(footer, "& \\multicolumn{2}{r}{{\\scriptsize \\em BIC = ", rnd(summary(mdl)$BIC, d=3), "}} \\\\ \n")

    addtorow <- list()
    addtorow$pos <- list(nrow(xt.outt))
    addtorow$command <- c(footer)

    print(xt.outt,
          sanitize.text.function=function(x){x},
          caption.placement="top",
          table.placement="H",
          include.rownames=FALSE,
          add.to.row=addtorow,
          hline.after=c(-1, 0),
          floating=TRUE)
  }
  else {
    outt
  }
}

# Summarize fixed effects from a logistic model with a random intercept
glmer.summary.fixed <- function(m, rownames=NULL, caption="", d=2, d.pval=3, latex=TRUE, label="") {
  mdl <- summary(m)
  
  if(is.null(rownames)) rownames = names(mdl$coefficients[,1])[2:length(mdl$coefficients[,1])]
  
  outt <- new.df(cols=3)
  
  se <- sqrt(diag(as.matrix(vcov(m))))
  # se <- c(0.606839330, 0.004886881, 0.423678897, 0.211224658, 0.348318033, 0.356941081, 0.284879990, 0.322248993, 0.033626248, 0.287406175, 0.302039076, 0.200729799, 0.246299290, 0.360411915, 0.218900515, 0.351459941, 0.213390958)
  
  # table of estimates with 95% CI
  tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 * se)
  tab <- exp(tab)
  
  for(i in 2:nrow(tab)) {
    outt <- rbind(outt, cbind(rownames[i-1],
                              paste0(rnd(tab[i, 1], d=d), " (", rnd(tab[i,2], d=d), ", ", rnd(tab[i,3], d=d), ")"),
                              fmt.pval(mdl$coefficients[i,4], digits=d.pval, include.p=FALSE)))
  }
  colnames(outt) <- c("", "OR (95\\% CI)", "p-value")
  
  if(latex) {
    xt.outt <- xtable(outt, label=label)
    caption(xt.outt) <- caption
    align(xt.outt) <- "llrr"

    # Configure footer
    footer <- paste0("\\hline \\multicolumn{1}{l}{{\\scriptsize \\em N = ", mdl$devcomp$dims["N"], "}}")
    footer <- paste0(footer, "& \\multicolumn{2}{r}{{\\scriptsize \\em Groups = ", mdl$ngrps[1], "}} \\\\ \n")
    footer <- paste0(footer, "\\multicolumn{1}{l}{{\\scriptsize \\em AIC = ", rnd(mdl$AICtab["AIC"], d=3), "}}")
    footer <- paste0(footer, "& \\multicolumn{2}{r}{{\\scriptsize \\em BIC = ", rnd(mdl$AICtab["BIC"], d=3), "}} \\\\ \n")

    addtorow <- list()
    addtorow$pos <- list(nrow(xt.outt))
    addtorow$command <- c(footer)

    print(xt.outt,
          sanitize.text.function=function(x){x},
          caption.placement="top",
          table.placement="H",
          include.rownames=FALSE,
          add.to.row=addtorow,
          hline.after=c(-1, 0),
          floating=TRUE)
  } else {
    outt
  }
}


# Summarize a logistic regression model
log.summary <- function(mdl, rownames=NULL, caption="", d=2, latex=TRUE, label="", asymp.nml = FALSE) {
  if(is.null(rownames)) rownames = names(mdl$coef)[2:length(mdl$coef)]
  outt <- data.frame(matrix(vector(), 
                            0, 3, 
                            dimnames=list(c(), 
                                          c(1, 2, 3))), 
                     stringsAsFactors=F)
  
  for(i in 2:length(mdl$coef)) {
    outt <- rbind(outt,
                  cbind(rownames[i-1],
                        table.ci(mdl, parm=names(mdl$coef)[i], exp=TRUE, digits=d, asymp.nml=asymp.nml),
                        fmt.pval(summary(mdl)$coefficients[i,4], digits=d, include.p=FALSE)))
  }
  colnames(outt) <- c("", "OR (95\\% CI)", "p-value")
  if(latex) {
    xt.outt <- xtable(outt, label=label)
    caption(xt.outt) <- caption
    align(xt.outt) <- "llrr"
    
    # Configure footer  
    footer <- paste0("\\hline \\multicolumn{3}{r}{{\\scriptsize \\em Null deviance = ", rnd(summary(mdl)$null.deviance, d=1), 
                     " on ",rnd(summary(mdl)$df.null, d=0), " degrees of freedom}} \\\\ \n")
    footer <- paste0(footer, "\\multicolumn{3}{r}{{\\scriptsize \\em Residual deviance = ", rnd(summary(mdl)$deviance, d=1),
                     " on ",rnd(summary(mdl)$df.residual, d=0), " degrees of freedom}} \\\\ \n")
    footer <- paste0(footer, "\\multicolumn{3}{r}{{\\scriptsize \\em AIC = ", rnd(summary(mdl)$aic, d=1), "}} \\\\ \n")
    
    addtorow <- list()
    addtorow$pos <- list(nrow(xt.outt))
    addtorow$command <- c(footer)
    
    print(xt.outt,
          sanitize.text.function=function(x){x},
          caption.placement="top",
          table.placement="H",
          include.rownames=FALSE,
          add.to.row=addtorow,
          floating=TRUE)
  }
  else {
    outt
  }
}

# # Summarize a logistic regression model fitted using logistf (Firth correction)
# log.summary <- function(mdl, rownames=NULL, caption="", d=2, latex=TRUE, label="", asymp.nml = FALSE) {
#   mdl <- glm.cc.firth
#   d=2
#   rownames=NULL
#   if(is.null(rownames)) rownames = names(mdl$coef)[2:length(mdl$coef)]
#   outt <- data.frame(matrix(vector(), 
#                             0, 3, 
#                             dimnames=list(c(), 
#                                           c(1, 2, 3))), 
#                      stringsAsFactors=F)
#   
#   for(i in 2:length(mdl$coef)) {
#     i <- 2
#     outt <- rbind(outt,
#                   cbind(rownames[i-1],
#                         paste0(as.character(exp(mdl$coef[[i]])), " (", 
#                                as.character(exp(mdl$ci.lower[[i]])), ", ", 
#                                as.character(exp(mdl$ci.upper[[i]])), ")"),
#                         fmt.pval(summary(mdl)$coefficients[i,4], digits=d)))
#   }
#   colnames(outt) <- c("", "OR (95\\% CI)", "p-value")
#   if(latex) {
#     xt.outt <- xtable(outt, label=label)
#     caption(xt.outt) <- caption
#     align(xt.outt) <- "llrr"
#     print(xt.outt,
#           sanitize.text.function=function(x){x},
#           caption.placement="top",
#           table.placement="H",
#           include.rownames=FALSE,
#           floating=TRUE)
#   }
#   else {
#     outt
#   }
# }

# Summarize a Cox proportional-hazards model
ph.summary <- function(mdl, rownames=NULL, caption="", d=2, label="", ...) {
  if(is.null(rownames)) rownames = names(mdl$coef)[1:length(mdl$coef)]
  outt <- data.frame(matrix(vector(), 
                            0, 3, 
                            dimnames=list(c(), 
                                          c(1, 2, 3))), 
                     stringsAsFactors=F)
  
  for(i in 1:length(mdl$coef)) {
    outt <- rbind(outt,
                  cbind(rownames[i],
                        table.ci(mdl, parm=names(mdl$coef)[i], exp=TRUE, digits=d),
                        fmt.pval(summary(mdl)$coefficients[i,5], digits=d)))
  }
  colnames(outt) <- c("", "HR (95\\% CI)", "p-value")
  xt.outt <- xtable(outt, label=label)
  caption(xt.outt) <- caption
  align(xt.outt) <- "llrr"
  
  # Configure footer  
  footer <- paste0("\\hline \\multicolumn{1}{l}{{\\scriptsize \\em n = ", mdl$n, "}}")
  footer <- paste0(footer, "& \\multicolumn{2}{r}{{\\scriptsize \\em Number of events = ", summary(mdl)$nevent, "}} \\\\ \n")
  footer <- paste0(footer, "\\multicolumn{1}{l}{{\\scriptsize \\em $R^2$ = ", rnd(summary(mdl)$rsq[1], d=2), " (max ", rnd(summary(mdl)$rsq[2], d=2), ")}}")
  footer <- paste0(footer, "& \\multicolumn{2}{r}{{\\scriptsize \\em LRT: p = ", rnd(summary(mdl)$logtest[3], d=3), "}} \\\\ \n")
  
  addtorow <- list()
  addtorow$pos <- list(nrow(xt.outt))
  addtorow$command <- c(footer)
  
  print(xt.outt,
        sanitize.text.function=function(x){x},
        caption.placement="top",
        include.rownames=FALSE,
        add.to.row=addtorow,
        hline.after=c(-1, 0),
        floating=TRUE,
        ...)
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

# Supply a function for Fisher's exact test for the Hmisc::summary.formula function.
catTestFisher <- function (tab) 
{
  st <- if (!is.matrix(tab) || nrow(tab) < 2 | ncol(tab) < 2) 
    list(p.value = NA, statistic = NA, parameter = NA)
  else {
    rowcounts <- tab %*% rep(1, ncol(tab))
    tab <- tab[rowcounts > 0, ]
    if (!is.matrix(tab)) 
      list(p.value = NA, statistic = NA, parameter = NA)
    else fisher.test(x=tab)
  }
  list(P = st$p.value, stat = 1, df = 1, 
       testname = "Fisher's exact", statname = "", latexstat = "p", 
       plotmathstat = "p")
}

# Supply a function for Fisher's exact test for the Hmisc::summary.formula function (simulated p-values).
catTestFisher.psim <- function (tab) 
{
  st <- if (!is.matrix(tab) || nrow(tab) < 2 | ncol(tab) < 2) 
    list(p.value = NA, statistic = NA, parameter = NA)
  else {
    rowcounts <- tab %*% rep(1, ncol(tab))
    tab <- tab[rowcounts > 0, ]
    if (!is.matrix(tab)) 
      list(p.value = NA, statistic = NA, parameter = NA)
    else fisher.test(x=tab, simulate.p.value = TRUE)
  }
  list(P = st$p.value, stat = 1, df = 1, 
       testname = "Fisher's exact (Monte Carlo simulated p-value)", statname = "", latexstat = "p", 
       plotmathstat = "p")
}

# Summarizes categorical variables by a specified grouping variable.
summary.cat <- function(fm,df,rowlabel="",rowname=NULL, digits=0, test=catTestchisq) {
  ss <- "$^a$"
  a <- summary(fm, data=df, method="reverse", test=TRUE, catTest=test)
  if(rowlabel=="") rowlabel <- a$labels
  if(is.null(rowname)) rowname <- attr(a$stats[[1]], "dimnames")[[1]]
  n.grp1 <- sum(a$stats[[1]][,1])
  n.grp2 <- sum(a$stats[[1]][,2])
  n.combined <- n.grp1 + n.grp2
  out <- cbind(rowlabel, as.character(a$n), "~",  "~", "~", paste0(fmt.pval(a$testresults[[1]]$P),ss))
  for(i in 1:length(attr(a$stats[[1]], "dimnames")[[1]])) {
    #print(rowname[i])
    out <- rbind(out, cbind(paste0("~~~",rowname[i]), "~", 
                            paste0(fmt.pct(a$stats[[1]][i,1]/n.grp1, digits), 
                                   " (", a$stats[[1]][i,1], ")"),
                            paste0(fmt.pct(a$stats[[1]][i,2]/n.grp2, digits), 
                                   " (", a$stats[[1]][i,2], ")"),
                            paste0(fmt.pct((a$stats[[1]][i,1]+a$stats[[1]][i,2])/n.combined, digits), 
                                   " (", (a$stats[[1]][i,1]+a$stats[[1]][i,2]), ")"),
                            "~"))
  }
  return(out)
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

# Produces a series of univariate logistic regression models and wraps them in LaTeX.
univ.logistic <- function(outcome, preds, df, rownames=NULL, caption="", digits=2, label="", asymp.nml=FALSE) {  
  univ.models <- data.frame(matrix(vector(), 0, 6))
  for(pred in preds) {
    mdl <- glm(formula(paste0(outcome, " ~ ", pred)), family=binomial(logit), data=df)
    if(asymp.nml) {
      ci <- confint.default(mdl)
    } else {
      ci <- confint(mdl)
    }
    univ.models <- rbind(univ.models, cbind(summary(mdl)$coef, exp(ci)))
  }
  univ.models <- univ.models[!grepl("(Intercept)", row.names(univ.models)),]
  colnames(univ.models) <- c("coef", "SE", "z", "p", "LB", "UB")
  univ.models$OR <- exp(univ.models$coef)
  univ.models <- univ.models[,c("coef", "SE", "OR", "LB", "UB", "p")]
  
  for (i in 1:length(univ.models$p)) {
    univ.models$p[i] <- fmt.pval(as.numeric(univ.models$p[i]), digits=digits, include.p=FALSE, latex=FALSE)
  }
  
  if(is.null(rownames)) rownames <- rownames(univ.models)
  
  xt.um <- xtable(univ.models, caption=caption, digits=digits, label=label)
  align(xt.um) <- "lrrrrrr"
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

# Produces a series of univariate Cox PH regression models and wraps them in LaTeX.
univ.coxph <- function(timevar, death.indicator, preds, df, rownames=NULL, caption="", digits=2, label="", size="footnotesize") {  
  univ.models <- data.frame(matrix(vector(), 0, 8))
  for(pred in preds) {
    surv.obj <- Surv(df[[timevar]], df[[death.indicator]])
    #     mdl <- coxph(formula(paste0(timevar, " ~ ", pred)), data=df)
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

