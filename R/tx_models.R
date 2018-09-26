require(broom)
tx_mdl <- function(mdl, d=3, intercept=TRUE, varnames=NULL, lbl="", caption="", coef.head="Coefficient", FUN=identity, print_mdl=TRUE, footer=c()) UseMethod("tx_mdl")

tx_mdl.default <- function(mdl, d=3, intercept=TRUE, varnames=NULL, lbl="", caption="", coef.head="Coefficient", FUN=identity, print_mdl=TRUE, footer=c()) {
  # Define the estimates label
  est.lbl <- paste0(coef.head, " (95\\% CI)")

  # Establish the coefficients table
  mdl.tbl <- bind_cols(tidy(mdl), confint_tidy(mdl))
  
  # Apply FUN function
  mdl.tbl$estimate <- FUN(mdl.tbl$estimate)
  mdl.tbl$conf.low <- FUN(mdl.tbl$conf.low) 
  mdl.tbl$conf.high <- FUN(mdl.tbl$conf.high)
    
  # Reformat the coefficients table
  mdl.tbl[[est.lbl]] <- paste0(rnd(mdl.tbl$estimate, d), " (", rnd(mdl.tbl$conf.low, d), ", ", rnd(mdl.tbl$conf.high, d), ")")
  mdl.tbl %<>%
    rename(Term = term) %>%
    mutate(p = fmt.pval(p.value, digits=d, include.p=FALSE, latex=TRUE))
  
  eval(parse(text=paste0("mdl.tbl %<>% dplyr::select(Term, `", 
                         gsub(x=est.lbl, pattern="\\%", replacement="\\\\%"),
                         "`, p)")))
  
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
  
  if(print_mdl) {
    # Reformat the footer
    footer_txt <- paste0("\\hline \n", paste(paste0("{\\em \\scriptsize ", footer[seq_along(footer)], "}\\\\ \n"), sep="", collapse=""))
    
    outt <- xtable(mdl.tbl, 
                   caption=caption, digits=3)
    
    align(outt) <- paste0("ll", paste(rep("r", 2), collapse=""))
    
    # Configure header and footer  
    addtorow <- list()
    addtorow$pos <- list(nrow(outt))
    addtorow$command <- c(footer_txt)
    
    
    # Print the table
    print(outt,
          include.rownames=FALSE,
          caption.placement="top",
          floating=TRUE,
          table.placement="H",
          add.to.row=addtorow,
          size="footnotesize",
          hline.after=c(-1, 0),
          sanitize.text.function = identity)
  } else return(mdl.tbl)
}

#' Present a lm-produced model object in a LaTeX table
#' 
#' @param mdl A lm object
#' @param d An integer specifying the number of significant digits to be presented in the table.
#' @param intercept Logical indicating whether the table should include a row for the fixed intercept. Default is TRUE.
#' @param varnames Character vector of length length(names(mdl$coefficients)) specifying the names of terms in the model.
#' @param lbl LaTeX label for the table.
#' @param caption Caption for the table.
#' @param coef.head Word or phrase to use as a header for coefficient estimates. Default is "OR".
#' @param FUN Function to apply to coefficients before presenting. Default is identity().
#' @param print_mdl Logical indicating whether the summary table should be wrapped in LaTeX. Default is TRUE.
#' @return A LaTeX or clear-text table.
tx_mdl.lm <- function(mdl, d=3, intercept=TRUE, varnames=NULL, lbl="", caption="", coef.head="Coefficient", footer=c(), FUN=identity, print_mdl=TRUE) {
  # Reformat the model fit statistics
  mdl.fit <- glance(mdl)
  
  footer <- c()
  footer <- c(footer, 
              paste0("R^2^ = ", rnd(mdl.fit$r.squared, d)), 
              paste0("Adj R^2^ = ", rnd(mdl.fit$adj.r.squared, d)),
              paste0("AIC = ", rnd(mdl.fit$AIC, d)),
              paste0("BIC = ", rnd(mdl.fit$BIC, d)),
              fmt.pval(mdl.fit$p.value, digits=d, latex=FALSE))
  
  tx_mdl.default(mdl = mdl,
                      coef.head = "Coefficient",
                      FUN = FUN,
                      footer = footer,
                      d = d,
                      intercept = intercept,
                      varnames = varnames,
                      lbl = lbl,
                      caption = caption)
}

#' Present a glm-produced model object in a LaTeX table
#' 
#' @param mdl A glm object
#' @param d An integer specifying the number of significant digits to be presented in the table.
#' @param intercept Logical indicating whether the table should include a row for the fixed intercept. Default is FALSE.
#' @param varnames Character vector of length length(names(mdl$coefficients)) specifying the names of terms in the model.
#' @param lbl LaTeX label for the table.
#' @param caption Caption for the table.
#' @param coef.head Word or phrase to use as a header for coefficient estimates. Default is "OR".
#' @param FUN Function to apply to coefficients before presenting. Default is exp().
#' @param print_mdl Logical indicating whether the summary table should be wrapped in LaTeX.
#' @return A LaTeX or clear-text table.
tx_mdl.glm <- function(mdl, d=3, intercept=FALSE, varnames=NULL, lbl="", caption="", coef.head="OR", FUN=exp, footer=c(), print_mdl=TRUE) {
  # Reformat the model fit statistics
  mdl.fit <- glance(mdl)
  
  footer <- c(footer, 
              paste0("Null deviance = ", rnd(mdl.fit$null.deviance, d)),
              paste0("Deviance = ", rnd(mdl.fit$deviance, d)),
              paste0("AIC = ", rnd(mdl.fit$AIC, d)), 
              paste0("BIC = ", rnd(mdl.fit$BIC)))
  tx_mdl.default(mdl = mdl,
                      coef.head = coef.head,
                      FUN = exp,
                      footer = footer,
                      d = d,
                      intercept = intercept,
                      varnames=varnames,
                      lbl = lbl,
                      caption = caption)
}

#' Present a coxph-produced model object in a LaTeX table
#' 
#' @param mdl A coxph object
#' @param d An integer specifying the number of significant digits to be presented in the table.
#' @param varnames Character vector of length length(names(mdl$coefficients)) specifying the names of terms in the model.
#' @param lbl LaTeX label for the table.
#' @param caption Caption for the table.
#' @param coef.head Word or phrase to use as a header for coefficient estimates. Default is "HR".
#' @param FUN Function to apply to coefficients before presenting. Default is exp().
#' @param print_mdl Logical indicating whether the summary table should be wrapped in LaTeX.
#' @return A LaTeX or clear-text table.
tx_mdl.coxph <- function(mdl, d=3, varnames=NULL, lbl="", caption="", coef.head="HR", FUN=exp, footer=c(), print_mdl=TRUE) {
  # Reformat the model fit statistics
  mdl.fit <- glance(mdl)
  
  intercept <- TRUE
  
  footer <- c(footer, 
              paste0("N = ", rnd(mdl.fit$n, 0)),
              paste0("Number of events = ", rnd(mdl.fit$nevent, 0)),
              paste0("Concordance = ", rnd(mdl.fit$concordance, d)))
  
  tx_mdl.default(mdl = mdl,
                 coef.head = coef.head,
                 FUN = exp,
                 footer = footer,
                 d = d,
                 intercept = intercept,
                 varnames=varnames,
                 lbl = lbl,
                 caption = caption)
}

#' Present an nlme-produced lme model object in a LaTeX table
#' 
#' @param mdl An lme object
#' @param d An integer specifying the number of significant digits to be presented in the table.
#' @param intercept Logical indicating whether the table should include a row for the fixed intercept. Default is TRUE.
#' @param varnames Character vector of length length(names(mdl$coefficients$fixed)) specifying the names of fixed effects in the model.
#' @param lbl LaTeX label for the table.
#' @param caption Caption for the table.
#' @param coef.head Word or phrase to use as a header for coefficient estimates.
#' @param FUN Function to apply to coefficients before presenting. Default is identity().
#' @param print_mdl Logical indicating whether the summary table should be wrapped in LaTeX.
#' @return A LaTeX or clear-text table.
tx_mdl.lme <- function(mdl, d=3, intercept=TRUE, varnames=NULL, lbl="", caption="", coef.head="Coefficient", FUN=identity, print_mdl=TRUE) {
  n_observations <- nrow(summary(mdl)$groups)
  n_groups <- length(names(table(summary(mdl)$groups)))
  
  # Reformat the model fit statistics
  mdl.fit <- glance(mdl)
  
  footer <- c()
  footer <- c(footer, 
              paste0("Number of observations: ", n_observations),
              paste0("Number of groups: ", n_groups),
              paste0("sigma = ", rnd(mdl.fit$sigma, d)), 
              paste0("AIC = ", rnd(mdl.fit$AIC, d)),
              paste0("BIC = ", rnd(mdl.fit$BIC, d)))
  
  # Summarize the random effects
  rf_df <- unclass(VarCorr(mdl))
  tidy.rnd <- bind_rows(tibble(Variance = "Variance", StdDev = "StdDev"),
                        as_tibble(rf_df) %>%
                          mutate(Variance = as.character(round(as.numeric(Variance), d)),
                                 StdDev = as.character(round(as.numeric(StdDev), d)))) %>%
    mutate(term = c("{\\bf Random effects}", rownames(rf_df))) %>%
    select(term, Variance, StdDev)
  
  # Define the estimates label
  est.lbl <- paste0(coef.head, " (95\\% CI)")
  
  # Reformat the coefficients table for fixed effects
  mdl.tbl <- cbind(tidy(mdl, effects = "fixed"), intervals(mdl, which="fixed")$fixed[,c(1,3)])
  mdl.tbl[[est.lbl]] <- paste0(rnd(mdl.tbl$estimate, d), " (", 
                               rnd(mdl.tbl$lower, d), ", ", 
                               rnd(mdl.tbl$upper, d), ")")
  
  # Apply FUN function
  mdl.tbl$estimate <- FUN(mdl.tbl$estimate)
  mdl.tbl$lower <- FUN(mdl.tbl$lower) 
  mdl.tbl$upper <- FUN(mdl.tbl$upper)
  
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
  
  # Append the RF table to the bottom of the FE table.
  colnames(tidy.rnd) <- c("term",
                          "Coefficient (95\\% CI)",
                          "p-value")
  # tidy.rnd[1,2:3] <- paste0("{\\bf ", tidy.rnd[1,2:3], "}")
  mdl.tbl %<>% select(term,
                      `Coefficient (95\\% CI)`,
                      p.value) %>%
    rename(`p-value` = p.value) %>%
    mutate(`p-value` = fmt.pval(`p-value`, d, include.p=FALSE, latex=TRUE)) %>%
    bind_rows(bind_rows(tibble(term = NA_character_,
                               `Coefficient (95\\% CI)` = NA_character_,
                               `p-value` = NA_character_),
                        tidy.rnd)) %>%
    rename(`{\\bf Fixed effects}` = term)
  
  if(print_mdl) {
    # Reformat the footer
    footer_txt <- "\\hline \n"
    for(footline in 1:length(footer)) {
      footer_txt <- paste0(footer_txt, "{\\em \\scriptsize ", footer[footline], "}\\\\ \n")
    }
    
    fe_out <- xtable(mdl.tbl %>% select(`{\\bf Fixed effects}`,
                                        `Coefficient (95\\% CI)`,
                                        `p-value`), caption=caption, digits=3)
    align(fe_out) <- paste0("ll", paste(rep("r", 2), collapse=""))
    
    # Configure header and footer  
    addtorow <- list()
    addtorow$pos <- list(nrow(fe_out))
    addtorow$command <- c(footer_txt)
    
    
    # Print the table
    print(fe_out,
          include.rownames=FALSE,
          caption.placement="top",
          floating=TRUE,
          table.placement="H",
          add.to.row=addtorow,
          size="footnotesize",
          hline.after=c(-1, 0, nrow(mdl.tbl) - nrow(tidy.rnd)),
          sanitize.text.function = identity)
  } else return(mdl.tbl)
}