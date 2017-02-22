require(broom)
# -------------------------------------------------------------------------------- 
# Define a function that accepts a model object and produces a markdown diplay
# of the model summary, including fixed effects, random effects, and model statistics.
# -------------------------------------------------------------------------------- 

present_mdl <- function(mdl, coef.head="", link=identity, footer=c(), d=3, intercept=TRUE, varnames, lbl="", caption="") UseMethod("present_mdl")

present_mdl.default <- function(mdl, coef.head="", link=identity, footer=c(), d=3, intercept=TRUE, varnames, lbl="", caption="") {
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
  footer[seq_along(head(footer, -1))] <- paste0(footer[seq_along(head(footer, -1))], ";")
  footer[seq_along(footer)] <- paste0("_", footer[seq_along(footer)], "_\ \ \ ")
  
  # Print the table caption
  cat(paste0("\n\n##### ", caption, "\n***"))
  
  # Print the table
  eval(parse(text=paste0("print(kable(tibble(` ` = mdl.tbl[,c('term')], ",
                         "`", est.lbl, "` = paste0(rnd(mdl.tbl$estimate, d), ' (', rnd(mdl.tbl$conf.low, d), ', ', rnd(mdl.tbl$conf.high, d), ')'), ",
                         "p = fmt.pval(mdl.tbl$p.value, digits=d, include.p=FALSE, latex=FALSE, md=TRUE)), ",
                         "check.names=FALSE))")))
  
  # Print the footer
  cat("\n***\n", footer, "\n\n\n\n", sep=" ")
  cat("<br><br><br>")
}

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
                      lbl = lbl,
                      caption = caption)
}

present_mdl.glm <- function(mdl, d=3, intercept=FALSE, varnames=NULL, lbl="", caption="") {
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
                      link = exp,
                      footer = footer,
                      d = d,
                      intercept = intercept,
                      varnames=varnames,
                      lbl = lbl,
                      caption = caption)
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

