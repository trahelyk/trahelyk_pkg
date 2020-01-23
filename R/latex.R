# -------------------------------------------------------------------------------- 
# Define a generic function for formatting things in LaTeX
# --------------------------------------------------------------------------------
tx <- function(x, caption, d, include_rownames, tbl_align, footer, footer_align, size, 
               intercept, varnames, lbl, coef.head, link, print_mdl) UseMethod("tx")
tx.default <- function(x) {
  print(x)
}

sanity <- function(df) {
  for(i in 1:ncol(df)) {
    df[,i] <- gsub("\\+/-", "$\\\\pm$", df[,i])
    df[,i] <- gsub("%", "\\\\%", df[,i])
    df[,i] <- gsub("- ", "", df[,i])
  }
  return(df)
}

# -------------------------------------------------------------------------------- 
# Function that wraps a data.frame object in LaTeX
# --------------------------------------------------------------------------------
tx.data.frame <- function(x, caption="", d=3, include_rownames=FALSE, tbl_align=NULL, footer=NULL, footer_align="l", size="footnotesize") {
  # Replace special characters
  for(i in 1:nrow(x)) {
    for(j in 1:ncol(x)) {
      x[i,j] <- gsub("\\+/-", "$\\\\pm$", x[i,j])
      x[i,j] <- gsub("<", "\\\\textless ", x[i,j]) 
    }
  } 
  
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

#' Present two tableone objects in a stratified LaTeX table
#' 
#' @param t1 A tableone object
#' @param t2 A second tableone object
#' @param head1 Header for the first stratum
#' @param head2 Header for the second stratum
#' @param size Font size
#' @return A LaTeX table.
tx.tableone_stratified <- function(t1, t2, head1, head2, size="footnotesize") {
  warning("This function assumes hypothesis tests use the same rowwise methods in both tables, and it uses the footer from t2.")
  ########## 
  # Clean up table 1
  ##########
  
  # Combine the method with the p-value and format it as a superscript.
  t1$table$p <- paste0("$", t1$table$p, "^", t1$table$method, "$")
  t1$table$method <- NULL
  t1$table$p[t1$table$p=="$.^$"] <- "$\\cdot$~~"
  t1$table$p[t1$table$p=="$ ^$"] <- "~"
  
  # Reformat +/- in the table
  for(i in 3:5) {
    t1$table[,i] <- gsub("\\+/-", "$\\\\pm$", t1$table[,i])
    t1$table[,i] <- gsub("%", "\\\\%", t1$table[,i])
    t1$table[,i] <- gsub("- ", "~~~~~~~~~~", t1$table[,i])
  }
  
  t1$table[,1] <- gsub("- ", "~~~~~~~~~~", t1$table[,1])
  
  # Bold the table headers in LaTeX.
  colnames(t1$table)[c(2,6)] <- paste0("{\\bf ", colnames(t1$table)[c(2,6)], "}") 
  colnames(t1$table)[3] <- paste0("{\\bf ", colnames(t1$table)[3], "}")
  colnames(t1$table)[4] <- paste0("{\\bf ", colnames(t1$table)[4], "}")
  colnames(t1$table)[5] <- paste0("{\\bf ", colnames(t1$table)[5], "}")
  
  ########## 
  # Clean up table 2
  ##########
  # Combine the method with the p-value and format it as a superscript.
  t2$table$p <- paste0("$", t2$table$p, "^", t2$table$method, "$")
  t2$table$method <- NULL
  t2$table$p[t2$table$p=="$.^$"] <- "$\\cdot$~~"
  t2$table$p[t2$table$p=="$ ^$"] <- "~"
  
  # Reformat +/- in the table
  for(i in 3:5) {
    t2$table[,i] <- gsub("\\+/-", "$\\\\pm$", t2$table[,i])
    t2$table[,i] <- gsub("%", "\\\\%", t2$table[,i])
    t2$table[,i] <- gsub("- ", "~~~~~~~~~~", t2$table[,i])
  }
  
  t2$table[,1] <- gsub("- ", "~~~~~~~~~~", t2$table[,1])
  
  # Bold the table headers in LaTeX.
  colnames(t2$table)[c(2,6)] <- paste0("{\\bf ", colnames(t2$table)[c(2,6)], "}") 
  colnames(t2$table)[3] <- paste0("{\\bf ", colnames(t2$table)[3], "}")
  colnames(t2$table)[4] <- paste0("{\\bf ", colnames(t2$table)[4], "}")
  colnames(t2$table)[5] <- paste0("{\\bf ", colnames(t2$table)[5], "}")
  
  ########## 
  # Combine the two tables
  ##########
  tbl_stratified <- cbind(t1$table %>% select(-`{\\bf n}`), 
                          tibble(`~` = rep("~", nrow(t1$table))),
                          t2$table %>% select(-`{\\bf n}`, -` `))
  
  # Reformat the footer for LaTeX.
  for(l in letters[1:10]) {
    t2$footer <- gsub(paste0("\\(", l, "\\)"), paste0("$^", l, "$"), t2$footer)
  }
  t2$footer <- gsub("\\+/-", "$\\\\pm$", t2$footer)
  t2$footer <- paste0("\\multicolumn{6}{l}{\\em ", t2$footer, "} \\\\ \n ")
  
  footer <- ""
  for(footline in 1:length(t2$footer)) {
    footer <- paste0(footer, t2$footer[footline])
  }
  
  xt.out <- xtable(tbl_stratified, caption=t2$caption, digits=3)
  align(xt.out) <- paste0("ll", paste(rep("r", 9), collapse=""))
  
  # Configure header and footer  
  addtorow <- list()
  addtorow$pos <- list(-1,0, nrow(tbl_stratified))
  
  addtorow$command <- c(paste0("\\hline \n & \\multicolumn{4}{c}{\\bf ", head1, "} & ~ & \\multicolumn{4}{c}{\\bf ", head2, "} \\\\\\cmidrule{2-5} \\cmidrule{7-10} \n "),
                        paste0("~ & {\\em (n = ", t1$n$n.grp1, 
                               ")} & {\\em (n = ", t1$n$n.grp2,
                               ")} & {\\em (n = ", t1$n$n.combined,
                               ")} & ~ & ~ & {\\em (n = ", t2$n$n.grp1, 
                               ")} & {\\em (n = ", t2$n$n.grp2,
                               ")} & {\\em (n = ", t2$n$n.combined,
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
        hline.after=c(0),
        sanitize.text.function = identity)
}

#' Present a tableoneway object in a LaTeX table
#' 
#' @param x A tableoneway object
#' @param size Font size
#' @param include_n Include a column showing non-missing count for each variable? Logical; default=FALSE.
#' @return A LaTeX or clear-text table.
tx.tableoneway <- function(x, size="footnotesize", include_n=FALSE) {
  # Reformat +/- in the table
  for(i in 1:3) {
    x$table[,i] <- gsub("\\+/-", "$\\\\pm$", x$table[,i])
    x$table[,i] <- gsub("%", "\\\\%", x$table[,i])
    x$table[,i] <- gsub("- ", "~~~~~~~~~~", x$table[,i])
  }
  
  if(!include_n) {
    tex_cols <- 2
    x$table <- x$table[,c(1,3)]
    tex_hdrs <- c(2)
  } else {
    tex_cols <- 3
    tex_hdrs <- c(2,3)
  }
  
  # Bold the table headers in LaTeX.
  colnames(x$table)[tex_hdrs] <- paste0("{\\bf ", colnames(x$table)[tex_hdrs], "}") 
  
  # Reformat the footer for LaTeX.
  for(l in letters[1:10]) {
    x$footer <- gsub(paste0("\\(", l, "\\)"), paste0("$^", l, "$"), x$footer)
  }
  x$footer <- gsub("\\+/-", "$\\\\pm$", x$footer)
  x$footer <- paste0("\\multicolumn{", tex_cols, "}{l}{\\em ", x$footer, "} \\\\ \n ")
  
  footer <- ""
  for(footline in 1:length(x$footer)) {
    footer <- paste0(footer, x$footer[footline])
  }
  
  xt.out <- xtable(x$table, caption=x$caption, digits=3)
  align(xt.out) <- paste0("ll", paste(rep("r", tex_cols-1), collapse=""))
  
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

#' Present a glm model object in a LaTeX table
#' 
#' @param mdl A glm object
#' @param d An integer specifying the number of significant digits to be presented in the table.
#' @param intercept Logical indicating whether the table should include a row for the fixed intercept. Default is FALSE.
#' @param varnames Character vector of length length(names(mdl$coefficients$fixed)) specifying the names of fixed effects in the model.
#' @param lbl LaTeX label for the table.
#' @param caption Caption for the table.
#' @param coef.head Word or phrase to use as a header for coefficient estimates. Default is OR.
#' @param transf Function to transform beta coefficients. Default is exp().
#' @param print_mdl Logical indicating whether the summary table should be wrapped in LaTeX.
#' @return A LaTeX or clear-text table.
tx.glm <- function(x, d=3, intercept=FALSE, varnames=NULL, lbl="", caption="", coef.head="OR", transf=exp, print_mdl=TRUE) {
  mdl <- x
  n_observations <- nrow(summary(x)$groups)
  
  # Reformat the model fit statistics
  mdl.fit <- glance(mdl)
  
  footer <- c()
  footer <- c(footer, 
              paste0("Null deviance: ", rnd(mdl.fit$null.deviance, d)),
              paste0("DF (null): ", mdl.fit$df.null),
              paste0("Deviance = ", rnd(mdl.fit$deviance, d)), 
              paste0("DF (resid): ", mdl.fit$df.residual),
              paste0("AIC = ", rnd(mdl.fit$AIC, d)),
              paste0("BIC = ", rnd(mdl.fit$BIC, d)))
  
  # Define the estimates label
  est.lbl <- paste0(coef.head, " (95\\% CI)")
  
  # Reformat the coefficients table and transform the beta coefficients
  mdl.tbl <- cbind(tidy(mdl), confint.default(mdl) %>% `colnames<-`(c("lower", "upper")))
  
  # Transform the beta coefficients
  mdl.tbl$estimate <- transf(mdl.tbl$estimate)
  mdl.tbl$lower <- transf(mdl.tbl$lower) 
  mdl.tbl$upper <- transf(mdl.tbl$upper)
  
  mdl.tbl[[est.lbl]] <- paste0(rnd(mdl.tbl$estimate, d), " (", 
                               rnd(mdl.tbl$lower, d), ", ", 
                               rnd(mdl.tbl$upper, d), ")")
  
  
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
    footer_txt <- "\\hline \n"
    for(footline in 1:length(footer)) {
      footer_txt <- paste0(footer_txt, "{\\em \\scriptsize ", footer[footline], "}\\\\ \n")
    }
    
    outt <- xtable(mdl.tbl %>% select(est.lbl,
                                      `p.value`), caption=caption, digits=3)
    align(outt) <- paste0("ll", paste(rep("r", 1), collapse=""))
    
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
          hline.after=c(-1, 0, nrow(mdl.tbl)),
          sanitize.text.function = identity)
  } else return(mdl.tbl)
}

#' Display a trahble-based tabulation in LaTeX
#'
#' @param A trahble object, produced by trahbulate.
#'
#' @return NULL
#' @export
#'
#' @examples
tx.trahble <- function(x) {
  # Reformat +/- in the table
  for(i in 3:(ncol(x$table)-1)) {
    x$table[,i] <- gsub("\\+/-", "$\\\\pm$", x$table[,i])
  }
  
  # Remove the method column
  x$table$method <- NULL
  
  # Bold the table headers in markdown.
  colnames(x$table)[c(2)] <- paste0("**", colnames(x$table)[c(2)], "**") 
  colnames(x$table)[3:(ncol(x$table)-1)] <- map_chr(3:(ncol(x$table)-1), 
                                                    function(i) paste0("**", colnames(x$table)[i], " (n=", x$n[[i-2]], ")**"))
  colnames(x$table)[ncol(x$table)] <- paste0("**", colnames(x$table)[ncol(x$table)], " (n=", x$n$combined, ")**")
  
  # Reformat the footer for markdown.
  x$footer <- gsub("\\+/-", "$\\\\pm$", x$footer)
  x$footer <- paste0("_", x$footer, "_")
  x$footer[x$footer=="_ _"] <- ""
  
  # Print the table caption.
  word_style(word=word, wordstyles=wordstyles, style="tablecaption", cat_txt = paste0(tbls(x$label, x$caption)))
  
  # Print the table. Note that we must wrap kable in a print function if we want to have a footer (weirdness with the kable function).
  print(kable(x$table,
              format = "markdown",
              row.names = FALSE))
  
  # Print the footer.
  word_style(word=word, wordstyles=wordstyles, style="tablefooter", cat_txt = x$footer)
  cat("  <br>*****<br>")
}