####################################################################################################
# This file contains functions that are disused, outdated, or of questionable integrity. They 
# remain a part of the package for backward compatibility.
####################################################################################################

# I used to use this to set knitr options in a literate document header. 
chunk_opts <- function(prefix="") {
  options(stringsAsFactors = FALSE)
  prefix = prefix
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



################################################################################################################################
crossTab <- function(x, y, xlab="X", ylab="Y", x.names=NULL, y.names=NULL, caption="", label="") {
  if(is.null(x.names)) {
    x.names <- names(table(x))
  }
  if(is.null(y.names)) {
    y.names <- names(table(y))
  }
  tab <- table(x, y)
  
  out <- new.df(cols=length(names(table(y)))+2)
  # names(out) <- c(xlab, y.names, "~")
  out <- rbind.data.frame(out, c(xlab, y.names, " "))
  
  for (i in 1:(length(x.names))) {  
    newcol.txt <- ""
    rowsum <- 0
    for (j in 1:length(y.names)) {
      newcol.txt <- paste0(newcol.txt, ", as.character(tab[", i, ", ", j, "])")
      rowsum <- rowsum + tab[i,j]
    }
    eval(parse(text=paste0("newrow <- data.frame(cbind(x.names[i]", newcol.txt, ", rowsum), stringsAsFactors=FALSE)")))
    names(newrow) <- names(out)
    out <- rbind(out, newrow)
  }
  
  # Convert from factors to character vectors
  for(colname in colnames(out)) {
    out[[colname]] <- as.character(out[[colname]])
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
  eval(parse(text=paste0("newrow <- data.frame(cbind(' '", colsums.txt, ", ", grandtotal, "))")))
  names(newrow) <- names(out)
  out <- rbind(out, newrow)
  
  # Add percentages
  for(i in 2:(length(x.names)+2)) {
    for(j in 2:(length(y.names)+1)) {
      out[i,j] <- paste0(out[i,j], " (", 
                         fmt.pct(as.numeric(as.character(out[i,j]))/as.numeric(as.character(out[i,length(y.names)+2])), 
                                 d=2, latex=FALSE), ")")
    }
  }
  
  # Move first row to colnames
  colnames(out) <- out[1,]
  out <- out[2:nrow(out),]
  
  return(as_tibble(out))
}


################################################################################################################################

# Rename a data frame
rename.df <- function(df) {
  foo <- df
  rm(df)
  return(foo)
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