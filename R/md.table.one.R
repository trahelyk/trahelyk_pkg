library(DT)

md.table <- function(dataset, out.type="", filter_opt = 'none', rownames=FALSE, cap="", footer="") {
  if (out.type == "html") {
    out.table <- DT::datatable(dataset, 
                               filter = filter_opt, 
                               rownames=rownames,
                               caption = cap,
                               # caption = htmltools::tags$caption(
                               #   style = 'caption-side: bottom; text-align: left;',
                               #   htmltools::em(footer)),
                               escape = FALSE)
  } else {
    out.table <- knitr::kable(dataset,
                              row.names = FALSE,
                              booktabs = TRUE)
  }
  return(out.table)
}


md.table.one <- function(df, grpvar, testTypes=NULL, d=1, p.digits=3, cap="", lab="", size="footnotesize", inc.hist=FALSE, spacing=700, fisher.simulate.p=FALSE, out.type="") {
  # df = rbc
  # grpvar = "rbc.old"
  # testTypes=NULL
  # d=1
  # p.digits=3
  # cap=""
  # lab=""
  # size="footnotesize"
  # inc.hist=FALSE
  # spacing=700
  # fisher.simulate.p=FALSE
  # i <- 5
  # j <- 1
  
  # If the user specified a list of test types of valid length for continuous data, use them; otherwise, set all to "auto".
  if(length(testTypes)!=length(colnames(df))) {
    testTypes <- rep("auto", length(colnames(df)))
  }
  
  test.df <- data.frame(col.name = colnames(df),
                        test.type = testTypes,
                        stringsAsFactors = FALSE)
  
  # Create the table, beginning with an empty data frame
  out <- new.df(cols=8)
  for (i in 1:(length(colnames(df))-1)) {  
    # Identify the variable to summarize for the current iteration
    sumvar <- colnames(df)[!(colnames(df) %in% grpvar)][i]
    
    # Make sure the current variable is labeled; throw an error if it is not.
    if(class(df[[sumvar]])[1] != "labelled") stop("Must label all covariates.")
    
    # If the variable is continuous, analyze by Wilcoxon or t-test
    if(class(df[[sumvar]])[2] %in% c("integer", "numeric")) {
      # If the user did not specify a test type then use a Shapiro-Wilk normality test to decide between Wilcoxon or t-test
      if(test.df$test.type[test.df$col.name==sumvar] == "auto") {
        if(sw.test.robust(df[[sumvar]]) > 0.05) {
          test.df$test.type[test.df$col.name==sumvar] <- "t"
        } else {
          test.df$test.type[test.df$col.name==sumvar] <- "w"
        }
      }
      
      # t-test
      if(test.df$test.type[test.df$col.name==sumvar]=="t") {
        test <- t.test(formula(paste0(sumvar, "~", grpvar)), data=df)
        method <- "Student's t-test (two-sided)"
        newrow <- data.frame(cbind(label(df[[sumvar]]),
                                   sum(!is.na(df[[sumvar]])),
                                   paste0(rnd(mean(df[df[[grpvar]]==levels(df[[grpvar]])[1], c(sumvar)], na.rm=TRUE), d=d), 
                                          " ± ", 
                                          rnd(sd(df[df[[grpvar]]==levels(df[[grpvar]])[1], c(sumvar)], na.rm=TRUE), d=d)),
                                   paste0(rnd(mean(df[df[[grpvar]]==levels(df[[grpvar]])[2], c(sumvar)], na.rm=TRUE), d=d), 
                                          " ± ", 
                                          rnd(sd(df[df[[grpvar]]==levels(df[[grpvar]])[2], c(sumvar)], na.rm=TRUE), d=d)),
                                   paste0(rnd(mean(df[, c(sumvar)], na.rm=TRUE), d=d), 
                                          " ± ", 
                                          rnd(sd(df[, c(sumvar)], na.rm=TRUE), d=d)),
                                   paste0(fmt.pval(test$p.value, include.p=FALSE, latex=FALSE, digits=p.digits), ""),
                                   method,
                                   inline.hist(df[[sumvar]], spacing)))
        names(newrow) <- names(out)
        out <- rbind(out, newrow)
      } else {
        # Wilcoxon rank-sum test
        test <- tryCatch({
          wc.test <- wilcox.test(formula(paste0(sumvar, "~", grpvar)), data=df)
        }, warning = function(w) {
          return(suppressWarnings(wilcox.test(formula(paste0(sumvar, "~", grpvar)), data=df)))
        }, error = function(e) {
          return("err")
        }, finally = function() {
          return(wc.test)
        })
        if(test[[1]]=="err") {
          wc.pval <- "."
        } else {
          wc.pval <- fmt.pval(test$p.value, include.p=FALSE, latex=FALSE, digits=p.digits)
        }
        method <- "Wilcoxon rank-sum test with continuity correction"
        newrow <- data.frame(cbind(label(df[[sumvar]]),
                                   sum(!is.na(df[[sumvar]])),
                                   paste0(rnd(median(df[df[[grpvar]]==levels(df[[grpvar]])[1], c(sumvar)], na.rm=TRUE), d=d), 
                                          " (", 
                                          rnd(quantile(df[df[[grpvar]]==levels(df[[grpvar]])[1], c(sumvar)], 0.25, na.rm=TRUE), d=d),
                                          ", ",
                                          rnd(quantile(df[df[[grpvar]]==levels(df[[grpvar]])[1], c(sumvar)], 0.75, na.rm=TRUE), d=d),
                                          ")"),
                                   paste0(rnd(median(df[df[[grpvar]]==levels(df[[grpvar]])[2], c(sumvar)], na.rm=TRUE), d=d), 
                                          " (", 
                                          rnd(quantile(df[df[[grpvar]]==levels(df[[grpvar]])[2], c(sumvar)], 0.25, na.rm=TRUE), d=d),
                                          ", ",
                                          rnd(quantile(df[df[[grpvar]]==levels(df[[grpvar]])[2], c(sumvar)], 0.75, na.rm=TRUE), d=d),
                                          ")"),
                                   paste0(rnd(median(df[, c(sumvar)], na.rm=TRUE), d=d), 
                                          " (", 
                                          rnd(quantile(df[, c(sumvar)], 0.25, na.rm=TRUE), d=d),
                                          ", ",
                                          rnd(quantile(df[, c(sumvar)], 0.75, na.rm=TRUE), d=d),
                                          ")"),
                                   wc.pval,
                                   method,
                                   inline.hist(df[[sumvar]], spacing)))
        names(newrow) <- names(out)
        out <- rbind(out, newrow)
      }
    }
    
    # If the variable is categorical, analyze by chi-squared or Fisher's exact
    if(class(df[[sumvar]])[2] %in% c("logical", "factor")) {
      
      # Construct a contingency table and run a chi-squared test
      tb <- table(df[[sumvar]], df[[grpvar]])
      test <- tryCatch({
        cs.test <- chisq.test(df[[sumvar]], df[[grpvar]])
      }, warning = function(w) {
        return(suppressWarnings(chisq.test(df[[sumvar]], df[[grpvar]])))
      }, error = function(e) {
        return("err")
      }, finally = function() {
        return(cs.test)
      })
      if(test[[1]]=="err") {
        pval <- "."
        method <- ""
      } else {
        pval <- fmt.pval(test$p.value, include.p=FALSE, latex=FALSE, digits=p.digits)
        method <- "Pearson's chi-squared test"
      }
      
      # If any cells have expected value < 5 and the chi2 test didn't error out, switch to Fisher's exact test.
      if(test[[1]]!="err") {
        if (min(test$expected) < 5) {
          test <- tryCatch({
            fish.test <- fisher.test(df[[sumvar]], df[[grpvar]], simulate.p.value=fisher.simulate.p)
          }, warning = function(w) {
            return(suppressWarnings(fisher.test(df[[sumvar]], df[[grpvar]])))
          }, error = function(e) {
            return("err")
          }, finally = function() {
            return(fish.test)
          })
          if(test[[1]]=="err") {
            pval <- fmt.pval(fisher.test(df[[sumvar]], df[[grpvar]], simulate.p.value=TRUE)$p.value,
                             include.p = FALSE, latex = FALSE, digits = p.digits)
            method <- "Fisher's exact test with simulated p-value"
          } else {
            pval <- fmt.pval(test$p.value, include.p=FALSE, latex=FALSE, digits=p.digits)
            method <- "Fisher's exact test"
          }
          if(class(test)=="try-error") {
            test <- fisher.test(df[[sumvar]], df[[grpvar]], simulate.p.value=TRUE)
          }
        }
      }
      newrow <- data.frame(cbind(label(df[[sumvar]]),
                                 sum(!is.na(df[[sumvar]]) & !is.na(df[[grpvar]])),
                                 " ",
                                 " ",
                                 " ",
                                 pval,
                                 method,
                                 " "))
      names(newrow) <- names(out)
      out <- rbind(out, newrow)
      
      # Identify level labels:
      if(class(df[[sumvar]])[2]=="logical") {
        level.labs <- c("FALSE", "TRUE")
      } else {
        level.labs <- levels(df[[sumvar]])
      }
      
      method <- ""
      for (j in 1:nrow(tb)) {
        newrow <- data.frame(cbind(paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", level.labs[j]),
                                   " ",
                                   paste0(fmt.pct(tb[j,1]/sum(tb[,1]), latex=FALSE), " (", tb[j,1], ")"),
                                   paste0(fmt.pct(tb[j,2]/sum(tb[,2]), latex=FALSE), " (", tb[j,2], ")"),
                                   paste0(fmt.pct(sum(tb[j,])/sum(tb[,]), latex=FALSE), " (", sum(tb[j,]), ")"),
                                   "　", method, " "))
        names(newrow) <- names(out)
        out <- rbind(out, newrow)
      }
    }
  }
  
  
  for (l in 1:ncol(out)) {
    out[,l] <- as.character(out[,l])
  }
  
  colnames(out) <- c("var.name", "n", "grp1", "grp2", "combined", "p", "method.name", "histogram")
  
  # Insert subscripts to identify methods used
  methods.used <- data.frame(method.name = unique(out$method.name[out$method.name!=""]),
                             subscript = letters[seq(1:length(unique(out$method.name[out$method.name!=""])))],
                             stringsAsFactors = FALSE)
  out$sort <- seq(1:nrow(out))
  out <- merge(out, methods.used,
               by="method.name",
               all.x=TRUE)
  out <- out[order(out$sort),]
  out$sort <- NULL
  out$subscript[is.na(out$subscript)] <- ""
  out$p <- paste0(out$p, " <sup>", out$subscript, "</sup>")
  out$method.name <- NULL
  out$subscript <- NULL
  out$p[out$p=="　 <sup></sup>"] <- " "
  out$p[out$p==". <sup></sup>"] <- "."
  # Replace NaN% with 0%
  out$grp1 <- gsub(pattern="NaN", 
                   replacement="0",
                   x=out$grp1)
  out$grp2 <- gsub(pattern="NaN", 
                   replacement="0",
                   x=out$grp2)
  
  if(!inc.hist) {
    out <- out[,1:6]
    col.cnt <- 6
    colnames(out) <- c(" ", "N", levels(df[[grpvar]])[1], levels(df[[grpvar]])[2], "Combined", "p-value")
  } else {
    col.cnt <- 7
    colnames(out) <- c(" ", "N", levels(df[[grpvar]])[1], levels(df[[grpvar]])[2], "Combined", "p-value", "Histogram")
  }
  
  # Generate footer
  footer <- ""
  if("Wilcoxon rank-sum test with continuity correction" %in% methods.used$method.name) {
    footer <- paste0(footer, "x.x (x.x, x.x) indicates median and inter-quartile range.<br>")
  } 
  if("Student's t-test (two-sided)" %in% methods.used$method.name) {
    footer <- paste0(footer, "x ± x indicates mean ± standard deviation.<br>")
  }
  for(k in 1:nrow(methods.used)) {
    footer <- paste0(footer, "<sup>", methods.used$subscript[k], "</sup>", methods.used$method.name[k], "<br>")
  }
  
  xt.out <- xtable(out, caption=footer, digits=d, label=NULL)
  align(xt.out) <- paste0("ll", paste(rep("r", col.cnt-1), collapse=""))
  
  # Configure header and footer  
  addtorow <- list()
  addtorow$pos <- list(nrow(out))
  addtorow$command <- c(footer)
  
  # Print the table
  print(xt.out,
        type="html",
        include.rownames=FALSE,
        caption.placement="bottom",
        floating=TRUE,
        table.placement="H",
        size=size,
        booktabs=TRUE,
        # add.to.row=addtorow,
        hline.after=c(-1, 0),
        width=50,
        html.table.attributes="frame=hsides",
        sanitize.text.function = identity)
}

# md.table.one(rbc,
#              grpvar = "rbc.old")
