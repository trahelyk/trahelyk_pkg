####################################################################################################
# Functions for generating and plotting ROC curves
####################################################################################################
library(pROC)

#' Generate roc_obj object that is handy for summarizing discrimination statistics and for passing
#' to other functions for presentation
#' 
#' @param x Numeric or ordinal vector of predictors
#' @param y Factor, numeric, or character vector of responses
#' @param units Character specifying units for formatting Youden's threshold
#' @param ci Logical indicating whether bootstrapped confidence intervals should be calculated
#' @param rnd_thresh Rounding precision (in decimal places) for Youden's threshold (default is 0)
#' @param rnd_discr Rounding precision for discrimination statistics (sensitivity and specificity; default is 2)
#' @return A roc_obj object
roc_obj <- function(x, y, xlab="", units="", ci=FALSE, rnd_thresh=0, rnd_discr=2) {
  if(xlab!="") {
    xlab <- paste0(xlab, "=")
  }
  robj <- roc(predictor=x, response=y)
  roc_tbl <- tibble(sens = robj$sens,
                    spec = robj$spec,
                    thresh = robj$thresholds,
                    sum = robj$specificities + (robj$sensitivities)) %>%
    arrange(sens)
  youden <- roc_tbl %>% arrange(desc(sum)) %>% slice(1)
  if(ci) {
    sens_ci = ci.se(roc=robj, specificities=youden$spec)[c(1,3)]
    spec_ci = ci.sp(roc=robj, sensitivities=youden$sens)[c(1,3)]
    youden %<>% mutate(se_lb = sens_ci[1],
                       se_ub = sens_ci[2],
                       sp_lb = spec_ci[1],
                       sp_ub = spec_ci[2])
    sens_txt <- with(youden, paste0(rnd(sens, rnd_discr), " (95\\% CI: ", rnd(se_lb, rnd_discr), ", ", rnd(se_ub, rnd_discr), ")"))
    spec_txt <- with(youden, paste0(rnd(spec, rnd_discr), " (95\\% CI: ", rnd(sp_lb, rnd_discr), ", ", rnd(sp_ub, rnd_discr), ")"))
  } else {
    sens_txt <- spec_txt <- NA
  }
  
  youden_txt <- c(paste0(xlab, rnd(youden$thresh, rnd_thresh), units),
                  paste0("(Spec=", rnd(youden$spec, rnd_discr), ", Sens=", rnd(youden$sens, rnd_discr), ")"))
  return(list(obj = robj,
              tbl = roc_tbl,
              youden = list(tbl = youden,
                            txt = youden_txt,
                            sens_txt = sens_txt,
                            spec_txt = spec_txt)))
}


plot_roc <- function(roc_obj, thresh_pos="top") {
  offset <- switch(thresh_pos,
                   top = c(0.08, 0.03),
                   bottom = c(-0.04, -0.10))
  ggplot(roc_obj$tbl, aes(x=(1-spec), y=sens)) +
    geom_line(size=1.5) +
    geom_point(x = (1-roc_obj$youden$tbl$spec),
               y = roc_obj$youden$tbl$sens,
               size=4, color="darkred") +
    labs(x="1 - specificity", y="Sensitivity") +
    annotate("text", 
             x = (1-roc_obj$youden$tbl$spec) + 0.01,
             y = roc_obj$youden$tbl$sens + offset[1],
             label = roc_obj$youden$txt[1],
             hjust="left",
             size=7) +
    annotate("text", 
             x = (1-roc_obj$youden$tbl$spec) + 0.01,
             y = roc_obj$youden$tbl$sens + offset[2],
             label = roc_obj$youden$txt[2],
             hjust="left",
             size=5) +
    theme(panel.background = element_rect(fill = "ivory"),
          panel.grid.major = element_line(color = "ivory2"),
          axis.title = element_text(size=15),
          axis.text = element_text(size=10))
}