require(broom)
# -------------------------------------------------------------------------------- 
# Define a function that accepts a model object and produces a markdown diplay
# of the model summary, including fixed effects, random effects, and model statistics.
# -------------------------------------------------------------------------------- 

#' Generic function to format and present a LaTeX table that summarizes a model object.
#' 
#' @param mdl A model object
#' @param coef_head Header text to place at the top of the coefficients column in the formatted table.
#' @param link Function to be applied to beta coefficients to yield, for example, odds ratios. I know this is not the link function. Defaults to identity.
#' @param footer Text to add to the table footer, printed below the formatted LaTeX table.
#' @param d An integer specifying the number of significant digits to be presented in the point estimate and interval. Defaults to 3.
#' @param intercept Logical indicating whether the intercept should be included in the presentation of the model. Defaults to TRUE.
#' @param varnames Character vector containing the English-version names of the independent variables in themodel. Defaults to names(mdl$coefficients)
#' @param lbl LaTeX label to attach to the presented table.
#' @param caption Caption to print above the table. 
#' @param corPars Logical indicating whether correlation parameters should be included in the presentation of the model (for GEE models). Defaults to FALSE.

#' @return A formatted LaTeX table containing estimates of model parameters and some simple fit statistics.
present_mdl <- function(mdl, coef.head="", link=identity, footer=c(), d=3, intercept=TRUE, varnames, lbl="", caption="", corPars=FALSE) UseMethod("present_mdl")

present_mdl.default <- function(mdl, coef.head="", link=identity, footer=c(), d=3, intercept=TRUE, varnames, lbl="", caption="") {
  # Define the line-break character and footer format based on output type
  if(is.null(opts_knit$get("rmarkdown.pandoc.to"))) {
    br <- "\n"
    small <- ""
    unsmall <- ""
  } else if(opts_knit$get("rmarkdown.pandoc.to")=="html") {
    br <- "<br>"
    small <- "<small>"
    unsmall <- "</small>"
  } else {
    br <- "\n"
    small <- ""
    unsmall <- ""
  }
  
  # Define the estimates label
  est.lbl <- paste0(coef.head, " (95\\\\% CI)")
  
  # Reformat the coefficients table
  mdl.tbl <- cbind(tidy(mdl), confint_tidy(mdl))
  mdl.tbl[[est.lbl]] <- paste0(rnd(mdl.tbl$estimate, d), " (", 
                               rnd(mdl.tbl$conf.low, d), ", ", 
                               rnd(mdl.tbl$conf.high, d), ")")
  
  # Inverse link function
  mdl.tbl$estimate <- link(mdl.tbl$estimate)
  mdl.tbl$conf.low <- link(mdl.tbl$conf.low) 
  mdl.tbl$conf.high <- link(mdl.tbl$conf.high)
  
  # Remove intercept if requested to do so and adjust varnames accordingly
  if(!intercept) {
    mdl.tbl <- mdl.tbl[2:nrow(mdl.tbl),]
    if (is.null(varnames)) {
      varnames <- names(mdl$coefficients)[2:length(mdl$coefficients)]
    }
  } else if(is.null(varnames)) {
    varnames <- names(mdl$coefficients)
  }
  
  # Apply variable names
  mdl.tbl[,1] <- varnames
  
  # Reformat the footer
  # footer[seq_along(head(footer, -1))] <- paste0(footer[seq_along(head(footer, -1))], ";")
  footer[seq_along(footer)] <- paste0(small, "_", footer[seq_along(footer)], "_", unsmall, br)
  
  # Print the table caption
  cat(paste0("\n\n**", caption, "**\n"))
  
  # Print the table
  eval(parse(text=paste0("print(kable(tibble(` ` = mdl.tbl[,c('term')], ",
                         "`", est.lbl, "` = paste0(rnd(mdl.tbl$estimate, d), ' (', rnd(mdl.tbl$conf.low, d), ', ', rnd(mdl.tbl$conf.high, d), ')'), ",
                         "p = fmt.pval(mdl.tbl$p.value, digits=d, include.p=FALSE, latex=FALSE, md=TRUE))))")))
                         # "check.names=FALSE))")))
  
  # Print the footer
  cat("\n***\n", footer, "\n\n\n\n", sep=" ")
  cat(paste0(br))
}

#' Format and present a LaTeX table that summarizes a linear model object.
#' 
#' @param mdl A lm object produced by stats::lm()
#' @param coef_head Header text to place at the top of the coefficients column in the formatted table.
#' @param link Function to be applied to beta coefficients to yield, for example, odds ratios. I know this is not the link function. Defaults to identity.
#' @param footer Text to add to the table footer, printed below the formatted LaTeX table.
#' @param d An integer specifying the number of significant digits to be presented in the point estimate and interval. Defaults to 3.
#' @param intercept Logical indicating whether the intercept should be included in the presentation of the model. Defaults to TRUE.
#' @param varnames Character vector containing the English-version names of the independent variables in themodel. Defaults to names(mdl$coefficients)
#' @param lbl LaTeX label to attach to the presented table.
#' @param caption Caption to print above the table. 

#' @return A formatted LaTeX table containing estimates of model parameters and some simple fit statistics.
present_mdl.lm <- function(mdl, d=3, intercept=TRUE, varnames=NULL, lbl="", caption="") {
  # Reformat the model fit statistics
  mdl.fit <- glance(mdl)
  
  footer <- c()
  footer <- c(footer, 
              paste0("R^2^ = ", rnd(mdl.fit$r.squared, d)), 
              paste0("Adj R^2^ = ", rnd(mdl.fit$adj.r.squared, d)),
              paste0("AIC = ", rnd(mdl.fit$AIC, d)),
              paste0("BIC = ", rnd(mdl.fit$BIC, d)),
              fmt.pval(mdl.fit$p.value, digits=d, latex=FALSE))
  
  present_mdl.default(mdl = mdl,
                      coef.head = "Coefficient",
                      link = identity,
                      footer = footer,
                      d = d,
                      intercept = intercept,
                      varnames = varnames,
                      lbl = lbl,
                      caption = caption)
}

#' Format and present a LaTeX table that summarizes a glm model object
#' 
#' @param mdl A glm object produced by stats::glm()
#' @param d An integer specifying the number of significant digits to be presented in the point estimate and interval. Defaults to 3.
#' @param intercept Logical indicating whether the intercept should be included in the presentation of the model. Defaults to FALSE.
#' @param varnames Character vector containing the English-version names of the independent variables in themodel. Defaults to names(mdl$coefficients)
#' @param lbl LaTeX label to attach to the presented table.
#' @param caption Caption to print above the table. 
#' @param link Function to be applied to beta coefficients to yield, for example, odds ratios. I know this is not the link function. Defaults to exp.
#' @return A formatted LaTeX table containing estimates of model parameters and some simple fit statistics.
present_mdl.glm <- function(mdl, d=3, intercept=FALSE, varnames=NULL, lbl="", caption="", link=exp) {
  # Reformat the model fit statistics
  mdl.fit <- glance(mdl)
  
  footer <- c()
  footer <- c(footer, 
              paste0("Null deviance = ", rnd(mdl.fit$null.deviance, d)),
              paste0("Deviance = ", rnd(mdl.fit$deviance, d)),
              paste0("AIC = ", rnd(mdl.fit$AIC, d)), 
              paste0("BIC = ", rnd(mdl.fit$BIC)))
  present_mdl.default(mdl = mdl,
                      coef.head = "OR",
                      link = link,
                      footer = footer,
                      d = d,
                      intercept = intercept,
                      varnames=varnames,
                      lbl = lbl,
                      caption = caption)
}

present_mdl.geeglm <- function(mdl, d=3, intercept=TRUE, varnames=NULL, lbl="", caption="", coef.head="", link=identity, corPars=TRUE) {
  # Define the line-break character and footer format based on output type
  if(is.null(opts_knit$get("rmarkdown.pandoc.to"))) {
    br <- "\n"
    small <- ""
    unsmall <- ""
  } else if(opts_knit$get("rmarkdown.pandoc.to")=="html") {
    br <- "<br>"
    small <- "<small>"
    unsmall <- "</small>"
  } else {
    br <- "\n"
    small <- ""
    unsmall <- ""
  }
  
  # Reformat the model summary
  se <- switch(mdl$std.err,
               san.se = "Sandwich estimator",
               jack = "Approximate jackknife variance estimator",
               j1s = "1-step jackknife variance estimator",
               fij = "Fully iterated jackknife variance estimator")
  
  corParsTbl <- summary(mdl)$geese$correlation
  
  footer <- c()
  footer <- c(footer, 
              paste0("Scale parameter = ", rnd(summary(mdl)$geese$scale[1], d), " (", fmt.pval(summary(mdl)$geese$scale[5], d), ")"),
              paste0("Correlation structure = ", mdl$modelInfo$corstr),
              paste0("Variance distribution = ", mdl$modelInfo$variance),
              paste0("Link function = ", mdl$modelInfo$mean.link))
  
  present_mdl.default(mdl = mdl,
                      coef.head = coef.head,
                      link = link,
                      footer = footer,
                      d = d,
                      intercept = intercept,
                      varnames=varnames,
                      lbl = lbl,
                      caption = caption)
  
  if(corPars) {
    cat(paste0("\n***\n ", br, "\n**Correlation parameters**\n\n***"))
    cat(md(corParsTbl, row.names = TRUE))
  }
}

present_mdl.coxph <- function(mdl, d=3, varnames=NULL, lbl="", caption="") {
  # Reformat the model fit statistics
  mdl.fit <- glance(mdl)
  
  footer <- c()
  footer <- c(footer, 
              paste0("N = ", rnd(mdl.fit$n, 0)),
              paste0("Number of events = ", rnd(mdl.fit$nevent, 0)),
              paste0("Concordance = ", rnd(mdl.fit$concordance, d)))
  present_mdl.default(mdl = mdl,
                      coef.head = "HR",
                      link = exp,
                      footer = footer,
                      d = d,
                      intercept = TRUE,
                      varnames=varnames,
                      lbl = lbl,
                      caption = caption)
}

present_mdl.lme <- function(mdl, d=3, intercept=TRUE, varnames=NULL, lbl="", caption="", coef.head="Coefficient", link=identity) {
  # Define the line-break character and footer format based on output type
  if(is.null(opts_knit$get("rmarkdown.pandoc.to"))) {
    br <- "\n"
    small <- ""
    unsmall <- ""
  } else if(opts_knit$get("rmarkdown.pandoc.to")=="html") {
    br <- "<br>"
    small <- "<small>"
    unsmall <- "</small>"
  } else {
    br <- "\n"
    small <- ""
    unsmall <- ""
  }
  
  # Reformat the model fit statistics
  mdl.fit <- glance(mdl)
  
  footer <- c()
  footer <- c(footer, 
              paste0("sigma = ", rnd(mdl.fit$sigma, d)), 
              paste0("AIC = ", rnd(mdl.fit$AIC, d)),
              paste0("BIC = ", rnd(mdl.fit$BIC, d)))
  
  # Summarize the random effects
  tidy.rnd <- as.data.frame(unclass(VarCorr(mdl)))
  
  # Define the estimates label
  est.lbl <- paste0(coef.head, " (95\\\\% CI)")
  
  # Reformat the coefficients table for fixed effects
  mdl.tbl <- cbind(tidy(mdl, effects = "fixed"), intervals(mdl, which="fixed")$fixed[,c(1,3)])
  mdl.tbl[[est.lbl]] <- paste0(rnd(mdl.tbl$estimate, d), " (", 
                               rnd(mdl.tbl$lower, d), ", ", 
                               rnd(mdl.tbl$upper, d), ")")
  
  # Inverse link function
  mdl.tbl$estimate <- link(mdl.tbl$estimate)
  mdl.tbl$lower <- link(mdl.tbl$lower) 
  mdl.tbl$upper <- link(mdl.tbl$upper)
  
  # Remove intercept if requested to do so and adjust varnames accordingly
  if(!intercept) {
    mdl.tbl <- mdl.tbl[2:nrow(mdl.tbl),]
    if (is.null(varnames)) {
      varnames <- names(mdl$coefficients$fixed)[2:length(mdl$coefficients$fixed)]
    }
  } else if(is.null(varnames)) {
    varnames <- names(mdl$coefficients$fixed)
  }
  
  # Apply variable names
  mdl.tbl[,1] <- varnames
  
  # Reformat the footer
  footer[seq_along(head(footer, -1))] <- paste0(footer[seq_along(head(footer, -1))], ";")
  footer[seq_along(footer)] <- paste0("_", footer[seq_along(footer)], "_\ \ \ ")
  
  # Print the table caption
  cat("\n\n", caption, "\n\n ***\n\n")
  
  # Print the random-effects table
  cat("Random effects\n\n***")
  cat(md(tidy.rnd, row.names = TRUE))
  
  # Print the fixed-effects table
  cat("Fixed effects\n\n***")
  eval(parse(text=paste0("print(kable(tibble(` ` = mdl.tbl[,c('term')], ",
                         "`", est.lbl, "` = paste0(rnd(mdl.tbl$estimate, d), ' (', rnd(mdl.tbl$lower, d), ', ', rnd(mdl.tbl$upper, d), ')'), ",
                         "p = fmt.pval(mdl.tbl$p.value, digits=d, include.p=FALSE, latex=FALSE, md=TRUE)), ",
                         "check.names=FALSE))")))
  
  # Print the footer
  cat("\n***\n", footer, "\n\n\n\n", sep=" ")
  cat(paste0(br))
}

present_mdl.gls <- function(mdl, d=3, intercept=TRUE, varnames=list(vf_names=NULL, cs_names=NULL, fe_names=NULL, anova_names=NULL), lbl="", caption="", coef.head="Coefficient", link=identity) {
  # Define the line-break character and footer format based on output type
  if(is.null(opts_knit$get("rmarkdown.pandoc.to"))) {
    br <- "\n"
    small <- ""
    unsmall <- ""
  } else if(opts_knit$get("rmarkdown.pandoc.to")=="html") {
    br <- "<br>"
    small <- "<small>"
    unsmall <- "</small>"
  } else {
    br <- "\n"
    small <- ""
    unsmall <- ""
  }
  
  # Reformat the model fit statistics
  mdl.sum <- summary(mdl)
  mdl.fit <- data.frame(sigma = mdl.sum$sigma,
                        AIC = mdl.sum$AIC,
                        BIC = mdl.sum$BIC,
                        logLik = mdl.sum$logLik)
  
  footer <- c()
  footer <- c(footer, 
              paste0("sigma = ", rnd(mdl.fit$sigma, d)), 
              paste0("AIC = ", rnd(mdl.fit$AIC, d)),
              paste0("BIC = ", rnd(mdl.fit$BIC, d)))
  
  # Inverse link function
  fe.tbl <- data.frame(coef <- link(intervals(mdl)$coef[,2]),
                       lb <- link(intervals(mdl)$coef[,1]),
                       ub <- link(intervals(mdl)$coef[,3]),
                       p = fmt.pval(mdl.sum$tTable[,4], d=3)) %>% `colnames<-`(c("coef", "lb", "ub", "p"))
  
  # Summarize the variance function
  if(is.null(varnames$vf_names)) {
    varnames$vf_names <- rownames(intervals(mdl)$varStruct)
  }
  tidy.vf <- data.frame(cbind(Parameter = varnames$vf_names, 
                              Estimate = paste0(rnd(intervals(mdl)$varStruct[,2], d), " (", 
                                                rnd(intervals(mdl)$varStruct[,1], d), ", ", 
                                                rnd(intervals(mdl)$varStruct[,3], d), ")"))) %>% `colnames<-`(c("Parameter", "Estimate (95\\\\% CI)"))
  
  # Summarize the correlation structure
  if(is.null(varnames$cs_names)) {
    varnames$cs_names <- rownames(intervals(mdl)$corStruct)
  }
  tidy.cs <- data.frame(cbind(Parameter = varnames$cs_names, 
                              Estimate = paste0(rnd(intervals(mdl)$corStruct[,2], d), " (", 
                                                rnd(intervals(mdl)$corStruct[,1], d), ", ", 
                                                rnd(intervals(mdl)$corStruct[,3], d), ")"))) %>% `colnames<-`(c("Parameter", "Estimate (95\\\\% CI)"))
  
  # Reformat the coefficients table for fixed effects
  if(is.null(varnames$fe_names)) {
    varnames$fe_names <- rownames(intervals(mdl)$coef)
  }
  tidy.fe <- data.frame(cbind(Parameter = varnames$fe_names, 
                              Estimate = paste0(rnd(fe.tbl$coef, d), " (", 
                                                rnd(fe.tbl$lb, d), ", ", 
                                                rnd(fe.tbl$ub, d), ")"),
                              p = as.character(fe.tbl$p))) %>% `colnames<-`(c("Parameter", "Estimate (95\\\\% CI)", "p"))
  
  # Reformat the F-tests
  if(is.null(varnames$anova_names)) {
    varnames$anova_names <- rownames(anova(mdl))
  }
  tidy.anova <- cbind(varnames$anova_names, anova(mdl)) %>% 
    `colnames<-`(c("Effect", "DF", "F-value", "p")) %>% 
    mutate(p = fmt.pval(p, d=3, include.p=FALSE))
  
  # Remove intercept if requested to do so and adjust varnames accordingly
  if(!intercept) {
    tidy.fe <- tidy.fe[2:nrow(tidy.fe),]
    tidy.anova <- tidy.anova[2:nrow(tidy.anova),]
  }
  
  # Reformat the footer
  footer[seq_along(head(footer, -1))] <- paste0(footer[seq_along(head(footer, -1))], ";")
  footer[seq_along(footer)] <- paste0("_", footer[seq_along(footer)], "_\ \ \ ")
  
  # Print the table caption
  cat("\n\n", caption, "\n\n ***\n\n")
  
  # Print the variance function summary
  cat("**Variance function**\n\n***")
  cat(md(tidy.vf, row.names = FALSE))
  
  # Print the correlation structure summary
  cat("\n***\n ", br, "**Correlation structure**\n\n***")
  cat(md(tidy.cs, row.names = FALSE))
  
  # Print the fixed effects
  cat("\n***\n ", br, "**Fixed effects**\n\n***")
  cat(md(tidy.fe, row.names = FALSE))
  
  # Print the F-tests
  cat("\n***\n ", br, "**F-tests of fixed effects**\n\n***")
  cat(md(tidy.anova, row.names = FALSE))
  
  # Print the footer
  cat("\n***\n", footer, "\n\n\n\n", sep=" ")
  cat(paste0(br, br))
}