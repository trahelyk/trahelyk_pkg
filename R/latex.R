# -------------------------------------------------------------------------------- 
# Define a generic function for formatting things in LaTeX
# --------------------------------------------------------------------------------
tx <- function(x, caption, d, include_rownames, tbl_align, footer, footer_align, size, 
               intercept, varnames, lbl, coef.head, link, print_mdl) UseMethod("tx")
tx.default <- function(x) {
  print(x)
}

# -------------------------------------------------------------------------------- 
# Function that wraps a data.frame object in LaTeX
# --------------------------------------------------------------------------------
tx.data.frame <- function(x, caption="", d=3, include_rownames=FALSE, tbl_align=NULL, footer=NULL, footer_align="l", size="footnotesize") {
  xt.out <- xtable(x, caption=caption, digits=d)
  
  if(length(tbl_align) == 0) {
    tbl_align <- paste0("ll", paste(rep("r", ncol(x)-1), collapse=""))
  }
  align(xt.out) <- tbl_align
  
  # Configure header and footer  
  footer <- paste0("\\multicolumn{", ncol(x), "}{", footer_align, "}{\\em ", footer, "} \\\\ \n ")
  
  # Print the table
  print(xt.out,
        include.rownames=include_rownames,
        caption.placement="top",
        floating=TRUE,
        table.placement="H",
        add.to.row=list(pos = list(nrow(x)),
                        command = c(paste0("\\hline ", paste(footer, collapse="")))),
        size=size,
        sanitize.text.function = identity)
}

# -------------------------------------------------------------------------------- 
# Function that wraps a tableone object in LaTeX
# --------------------------------------------------------------------------------

tx.tableone <- function(x, size="footnotesize") {
  # Combine the method with the p-value and format it as a superscript.
  x$table$p <- paste0("$", x$table$p, "^", x$table$method, "$")
  x$table$method <- NULL
  x$table$p[x$table$p=="$.^$"] <- "$\\cdot$~~"
  x$table$p[x$table$p=="$ ^$"] <- "~"
  
  # Reformat +/- in the table
  for(i in 3:5) {
    x$table[,i] <- gsub("\\+/-", "$\\\\pm$", x$table[,i])
    x$table[,i] <- gsub("%", "\\\\%", x$table[,i])
    x$table[,i] <- gsub("- ", "~~~~~~~~~~", x$table[,i])
  }
  
  x$table[,1] <- gsub("- ", "~~~~~~~~~~", x$table[,1])
  
  # Bold the table headers in LaTeX.
  colnames(x$table)[c(2,6)] <- paste0("{\\bf ", colnames(x$table)[c(2,6)], "}") 
  colnames(x$table)[3] <- paste0("{\\bf ", colnames(x$table)[3], "}")
  colnames(x$table)[4] <- paste0("{\\bf ", colnames(x$table)[4], "}")
  colnames(x$table)[5] <- paste0("{\\bf ", colnames(x$table)[5], "}")
  # colnames(x$table)[4:5] <- paste0("**", colnames(x$table)[3:5], " (n=", x$ "**")
  
  # Reformat the footer for LaTeX.
  for(l in letters[1:10]) {
    x$footer <- gsub(paste0("\\(", l, "\\)"), paste0("$^", l, "$"), x$footer)
  }
  x$footer <- gsub("\\+/-", "$\\\\pm$", x$footer)
  x$footer <- paste0("\\multicolumn{6}{l}{\\em ", x$footer, "} \\\\ \n ")
  
  footer <- ""
  for(footline in 1:length(x$footer)) {
    footer <- paste0(footer, x$footer[footline])
  }
  
  xt.out <- xtable(x$table, caption=x$caption, digits=3)
  align(xt.out) <- paste0("ll", paste(rep("r", 5), collapse=""))
  
  # Configure header and footer  
  addtorow <- list()
  addtorow$pos <- list(0, nrow(x$table))
  df <- x$table
  addtorow$command <- c(paste0("~ & ~ & {\\em (n = ", x$n$n.grp1, 
                               ")} & {\\em (n = ", x$n$n.grp2,
                               ")} & {\\em (n = ", x$n$n.combined,
                               ")} & ~ \\\\\n"),
                        paste0("\\hline ", footer))
  
  # Print the table
  print(xt.out,
        include.rownames=FALSE,
        caption.placement="top",
        floating=TRUE,
        table.placement="H",
        add.to.row=addtorow,
        size=size,
        hline.after=c(-1, 0),
        sanitize.text.function = identity)
}

#' Present a tableoneway object in a LaTeX table
#' 
#' @param x A tableoneway object
#' @param size Font size
#' @return A LaTeX or clear-text table.
tx.tableoneway <- function(x, size="footnotesize") {
  # Reformat +/- in the table
  for(i in 1:3) {
    x$table[,i] <- gsub("\\+/-", "$\\\\pm$", x$table[,i])
    x$table[,i] <- gsub("%", "\\\\%", x$table[,i])
    x$table[,i] <- gsub("- ", "~~~~~~~~~~", x$table[,i])
  }
  
  # Bold the table headers in LaTeX.
  colnames(x$table)[c(2,3)] <- paste0("{\\bf ", colnames(x$table)[c(2,3)], "}") 
  
  # Reformat the footer for LaTeX.
  for(l in letters[1:10]) {
    x$footer <- gsub(paste0("\\(", l, "\\)"), paste0("$^", l, "$"), x$footer)
  }
  x$footer <- gsub("\\+/-", "$\\\\pm$", x$footer)
  x$footer <- paste0("\\multicolumn{3}{l}{\\em ", x$footer, "} \\\\ \n ")
  
  footer <- ""
  for(footline in 1:length(x$footer)) {
    footer <- paste0(footer, x$footer[footline])
  }
  
  xt.out <- xtable(x$table, caption=x$caption, digits=3)
  align(xt.out) <- paste0("ll", paste(rep("r", 2), collapse=""))
  
  # Configure header and footer  
  addtorow <- list()
  addtorow$pos <- list(nrow(x$table))
  df <- x$table
  addtorow$command <- c(paste0("\\hline ", footer))
  
  # Print the table
  print(xt.out,
        include.rownames=FALSE,
        caption.placement="top",
        floating=TRUE,
        table.placement="H",
        add.to.row=addtorow,
        size=size,
        hline.after=c(-1, 0),
        sanitize.text.function = identity)
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
#' @param link Link function. Default is identity().
#' @param print_mdl Logical indicating whether the summary table should be wrapped in LaTeX.
#' @return A LaTeX or clear-text table.
tx.lme <- function(x, d=3, intercept=TRUE, varnames=NULL, lbl="", caption="", coef.head="Coefficient", link=identity, print_mdl=TRUE) {
  mdl <- x
  n_observations <- nrow(summary(x)$groups)
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
  mdl.tbl <- cbind(tidy(mdl, effects = "fixed"), intervals(mdl)$fixed[,c(1,3)])
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