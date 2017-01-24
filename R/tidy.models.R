require(tibble)

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
  
  outt <- as_tibble(cbind(rownames, univ.models, stringsAsFactors=FALSE)) %>%
    rename(" " = rownames)
  
  return(outt)
}
