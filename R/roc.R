####################################################################################################
# Functions for generating and plotting ROC curves
####################################################################################################

# Generate roc object
roc_obj <- function(x, y, xlab="") {
  if(xlab!="") {
    xlab <- paste0(xlab, "=")
  }
  roc_obj <- roc(predictor=x, response=y)
  roc_tbl <- tibble(sens = roc_obj$sens,
                    spec = roc_obj$spec,
                    thresh = roc_obj$thresholds,
                    sum = roc_obj$specificities + (roc_obj$sensitivities)) %>%
    arrange(sens)
  youden <- roc_tbl %>% arrange(desc(sum)) %>% slice(1)
  youden_txt <- c(paste0(xlab, rnd(youden$thresh, 0)),
                  paste0("(Spec=", rnd(youden$spec, 2), ", Sens=", rnd(youden$sens, 2), ")"))
  return(list(tbl = roc_tbl,
              youden = list(tbl = youden,
                            txt = youden_txt)))
}

plot_roc <- function(roc_obj, thresh_pos="top") {
  offset <- switch(thresh_pos,
                   top = c(0.07, 0.03),
                   bottom = c(-0.03, -0.07))
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