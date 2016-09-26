# -------------------------------------------------------------------------------- 
# Define a function that accepts a model object and produces a markdown diplay
# of the model summary, including fixed effects, random effects, and model statistics.
# -------------------------------------------------------------------------------- 

present.mdl <- function(mdl, mdl.type="linear", random=FALSE, d=3, intercept=TRUE, lbl="", caption="") {
  # Reformat the coefficients table
  mdl.tbl <- cbind(tidy(mdl), confint_tidy(mdl))
  mdl.tbl[["Coefficient (95\\% CI)"]] <- paste0(rnd(mdl.tbl$estimate, d), " (", 
                                                rnd(mdl.tbl$conf.low, d), ", ", 
                                                rnd(mdl.tbl$conf.high, d), ")")
  
  # Exponentiate if this is a logistic or PH model
  if(mdl.type %in% c("logistic", "cox", "ph")) {
    mdl.tbl$estimate <- exp(mdl.tbl$estimate)
    mdl.tbl$conf.low <- exp(mdl.tbl$conf.low) 
    mdl.tbl$conf.high <- exp(mdl.tbl$conf.high)
  }
  
  # Reformat the model fit statistics
  mdl.fit <- glance(mdl)
  footer <- c()
  if(mdl.type == "linear") {
    footer <- c(footer, 
                paste0("R^2^ = ", rnd(mdl.fit$r.squared, d)), 
                paste0("Adj R^2^ = ", rnd(mdl.fit$adj.r.squared, d)),
                paste0("AIC = ", rnd(mdl.fit$AIC, d)),
                paste0("BIC = ", rnd(mdl.fit$BIC, d)),
                fmt.pval(mdl.fit$p.value, digits=d, latex=FALSE))
  } else if(mdl.type == "logistic") {
    footer <- c(footer, 
                paste0("Null deviance = ", rnd(mdl.fit$null.deviance, d)),
                paste0("Deviance = ", rnd(mdl.fit$deviance, d)),
                paste0("AIC = ", rnd(mdl.fit$AIC, d)), 
                paste0("BIC = ", rnd(mdl.fit$BIC)))
  }
  footer[1:length(footer)-1] <- paste0("_", footer[1:length(footer)-1], ";_\ \ \ ")
  
  # Print the table caption
  cat(paste0("\n\n##### ", tbls(lbl, caption), "\n***"))
  
  # Print the table
  print(kable(data.frame(` ` = mdl.tbl[,c("term")],
                         `Coefficient (95\\% CI)` = paste0(rnd(mdl.tbl$estimate, d), " (",
                                                           rnd(mdl.tbl$conf.low, d), ", ",
                                                           rnd(mdl.tbl$conf.high, d), ")"),
                         p = fmt.pval(mdl.tbl$p.value, digits=d, include.p=FALSE, latex=FALSE, md=TRUE),
                         check.names=FALSE)))
  
  # Print the footer
  cat("\n***\n", footer, "\n\n\n\n", sep=" ")
  cat("<br><br><br>")
}