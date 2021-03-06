# -------------------------------------------------------------------------------- 
# Generate new class to hold the return object, comprising a tidy data frame
# for the summary statistics and a character vector that contains definitions
# of displays of summary statistics in the main table and a list of methods
# used to test for differences across groups in each variable.
# -------------------------------------------------------------------------------- 
tbl1 <- function(table, n, footer, caption, label) {
  if (!is.data.frame(table) | !is.list(n) | !is.vector(footer) | 
      !is.character(caption) | !is.character(label)) {
    stop("Wrong data type(s) passed to constructor.")
  }
  structure(list(table=table, 
                 n=n,
                 footer=footer,
                 caption=caption,
                 label=label), class = "tbl1")
}

# -------------------------------------------------------------------------------- 
# A similar class, but for one-way tables
# -------------------------------------------------------------------------------- 
tbl1w <- function(table, n, footer, caption, label) {
  if (!is.data.frame(table) | !is.list(n) | !is.vector(footer) | 
      !is.character(caption) | !is.character(label)) {
    stop("Wrong data type(s) passed to constructor.")
  }
  structure(list(table=table, 
                 n=n,
                 footer=footer,
                 caption=caption,
                 label=label), class = "tbl1w")
}

# -------------------------------------------------------------------------------- 
# A similar class, but for N-way tables
# -------------------------------------------------------------------------------- 
tblnw <- function(table, n, footer, caption, label) {
  if (!is.data.frame(table) | !is.list(n) | !is.vector(footer) | 
      !is.character(caption) | !is.character(label)) {
    stop("Wrong data type(s) passed to constructor.")
  }
  structure(list(table=table, 
                 n=n,
                 footer=footer,
                 caption=caption,
                 label=label), class = "tblnw")
}


#' Produces a tbl1 object, which contains a two-way table grouped by the specificed grouping variable.
#' 
#' @param df A data frame or tibble with Hmisc-brand labels.
#' @param grpvar The quoted name of a grouping variable that splits df into two cohorts.
#' @param testTypes A vector of length nrow(df) containing "t" for a t-test or "w" for a Wilcoxon rank-sum test; applies only to continuous variables. Leave NULL to allow the function to choose for you based on the distance between the mean and median. 
#' @param d Number of significant digits to report in (non-p-value) numbers in the tbl1 object.
#' @param p.digits Number of significant digits to report in p-values.
#' @param fisher.simulate.p If TRUE use simulated p-values for Fisher's exact test. Default is FALSE.
#' @param trunc_binary If TRUE print only one row for categorical variables with 2 levels. If FALSE, binary variables are printed on two rows, with the variable name and p-value on a row above. Default is TRUE.
#' @param lbl LaTeX label to be passed to tx() for labeling the table in a LaTeX document.
#' @param caption Text to be passed to tx() or md() to caption the table. 
#' @return A tbl1 object that can be formatted by tx() or md().
#' @examples
#' foo <- as_tibble(iris) %>% 
#'   filter(Species!="setosa") %>% 
#'     apply.labels(c("Sepal length", 
#'                    "Sepal width",
#'                    "Petal length",
#'                    "Petal width",
#'                    "Species")) %>%
#'     mutate(Species = fct_drop(Species))
#' md(tableone(foo, grpvar="Species"))
#' 
#' md(tableone(foo, grpvar="Species",
#'                  testTypes=c("t", "t", "t", "t", "t")))
#' 
#' md(tableone(foo, grpvar="Species",
#'                  testTypes=c("t", "t", "w", "w", "w")))

tableone <- function(df, grpvar, testTypes=NULL, d=1, p.digits=3, fisher.simulate.p=FALSE, trunc_binary=TRUE, lbl="", caption="") {
  df <- as.data.frame(df)
  
  if (lbl=="") lbl <- an.id(9)
  
  # If the user specified a list of test types of valid length for continuous data, use them; otherwise, set all to "auto".
  if(length(testTypes)!=length(colnames(df))) {
    testTypes <- rep("auto", length(colnames(df)))
  }
  
  test.df <- data.frame(col.name = colnames(df),
                        test.type = testTypes,
                        stringsAsFactors = FALSE)
  
  # Create the table, beginning with an empty data frame
  out <- new.df(cols=7)
  for (i in seq_along(cols.except(df, grpvar))) {  
    # Identify the variable to summarize for the current iteration
    sumvar <- colnames(df)[!(colnames(df) %in% grpvar)][i]
    
    # Make sure the current variable is labeled; throw an error if it is not.
    if(class(df[[sumvar]])[1] != "labelled") stop("Must label all covariates.")
    
    # If the variable is continuous, analyze by Wilcoxon or t-test
    if(class(df[[sumvar]])[2] %in% c("integer", "numeric")) {
      # If the user did not specify a test type then use a Wilcoxon rank-sum test if the distance between the mean and 
      # median is > 0.25 standard deviations; otherwise use a t-test.
      if(test.df$test.type[test.df$col.name==sumvar] == "auto") {
        if(abs(mean(df[[sumvar]], na.rm=TRUE) - median(df[[sumvar]], na.rm=TRUE))/sd(df[[sumvar]], na.rm=TRUE) <= 0.25) {
          test.df$test.type[test.df$col.name==sumvar] <- "t"
        } else {
          test.df$test.type[test.df$col.name==sumvar] <- "w"
        }
      }
      
      # The old version based on a shapiro-wilks test:
      # if(test.df$test.type[test.df$col.name==sumvar] == "auto") {
      #   if(sw.test.robust(df[[sumvar]]) > 0.05) {
      #     test.df$test.type[test.df$col.name==sumvar] <- "t"
      #   } else {
      #     test.df$test.type[test.df$col.name==sumvar] <- "w"
      #   }
      # }
      
      # t-test
      if(test.df$test.type[test.df$col.name==sumvar]=="t") {
        test <- t.test(formula(paste0(sumvar, "~", grpvar)), data=df)
        method <- "Student's t-test (two-sided)"
        newrow <- data.frame(cbind(label(df[[sumvar]]),
                                   sum(!is.na(df[[sumvar]])),
                                   paste0(rnd(mean(df[df[[grpvar]]==levels(df[[grpvar]])[1], c(sumvar)], na.rm=TRUE), d=d), 
                                          " +/- ", 
                                          rnd(sd(df[df[[grpvar]]==levels(df[[grpvar]])[1], c(sumvar)], na.rm=TRUE), d=d)),
                                   paste0(rnd(mean(df[df[[grpvar]]==levels(df[[grpvar]])[2], c(sumvar)], na.rm=TRUE), d=d), 
                                          " +/- ", 
                                          rnd(sd(df[df[[grpvar]]==levels(df[[grpvar]])[2], c(sumvar)], na.rm=TRUE), d=d)),
                                   paste0(rnd(mean(df[, c(sumvar)], na.rm=TRUE), d=d), 
                                          " +/- ", 
                                          rnd(sd(df[, c(sumvar)], na.rm=TRUE), d=d)),
                                   paste0(fmt.pval(test$p.value, include.p=FALSE, latex=FALSE, digits=p.digits), ""),
                                   method))
        names(newrow) <- names(out)
        out <- rbind(out, newrow)
      } else {
        # Wilcoxon rank-sum test
        test <- tryCatch({
          wc.test <- wilcox.test(formula(paste0(sumvar, "~", grpvar), correct=FALSE), data=df)
        }, warning = function(w) {
          return(suppressWarnings(wilcox.test(formula(paste0(sumvar, "~", grpvar)), data=df, correct=FALSE)))
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
        method <- "Wilcoxon rank-sum test"
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
                                   method))
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
      
      # Identify level labels:
      if(class(df[[sumvar]])[2]=="logical") {
        level.labs <- c("FALSE", "TRUE")
      } else {
        level.labs <- levels(df[[sumvar]])
      }
      
      if(nrow(tb)==2 & trunc_binary) {
        newrow <- data.frame(cbind(label(df[[sumvar]]),
                                   sum(!is.na(df[[sumvar]]) & !is.na(df[[grpvar]])),
                                   paste0(fmt.pct(tb[2,1]/sum(tb[,1]), latex=FALSE), " (", tb[2,1], ")"),
                                   paste0(fmt.pct(tb[2,2]/sum(tb[,2]), latex=FALSE), " (", tb[2,2], ")"),
                                   paste0(fmt.pct(sum(tb[2,])/sum(tb[,]), latex=FALSE), " (", sum(tb[2,]), ")"),
                                   pval,
                                   method))
        names(newrow) <- names(out)
        out <- rbind(out, newrow)
      } else {
        newrow <- data.frame(cbind(label(df[[sumvar]]),
                                   sum(!is.na(df[[sumvar]]) & !is.na(df[[grpvar]])),
                                   " ",
                                   " ",
                                   " ",
                                   pval,
                                   method))
        names(newrow) <- names(out)
        out <- rbind(out, newrow)
        
        method <- ""
        for (j in 1:nrow(tb)) {
          newrow <- data.frame(cbind(paste0(" - ", level.labs[j]),
                                     " ",
                                     paste0(fmt.pct(tb[j,1]/sum(tb[,1]), latex=FALSE), " (", tb[j,1], ")"),
                                     paste0(fmt.pct(tb[j,2]/sum(tb[,2]), latex=FALSE), " (", tb[j,2], ")"),
                                     paste0(fmt.pct(sum(tb[j,])/sum(tb[,]), latex=FALSE), " (", sum(tb[j,]), ")"),
                                     " ", method))
          names(newrow) <- names(out)
          out <- rbind(out, newrow)
        }
      }
    }
  }
  
  
  for (l in 1:ncol(out)) {
    out[,l] <- as.character(out[,l])
  }
  
  colnames(out) <- c(" ", "n", levels(df[[grpvar]])[1], levels(df[[grpvar]])[2], "Combined", "p", "method.name")
  
  # Identify methods used
  methods.used <- data.frame(method.name = unique(out$method.name[out$method.name!=""]),
                             method = letters[seq(1:length(unique(out$method.name[out$method.name!=""])))],
                             stringsAsFactors = FALSE)
  out$sort <- seq(1:nrow(out))
  out <- merge(out, methods.used,
               by="method.name",
               all.x=TRUE)
  out <- out[order(out$sort),]
  out$sort <- NULL
  out$method[is.na(out$method)] <- ""
  out$method.name <- NULL
  
  # Replace NaN% with 0%
  out[[levels(df[[grpvar]])[1]]] <- gsub(pattern="NaN", 
                                         replacement="0",
                                         x=out[[levels(df[[grpvar]])[1]]])
  out[[levels(df[[grpvar]])[1]]] <- gsub(pattern="NaN", 
                                         replacement="0",
                                         x=out[[levels(df[[grpvar]])[1]]])
  
  # Generate footer
  footer <- c()
  if("Wilcoxon rank-sum test" %in% methods.used$method.name) {
    footer <- c(footer, "x.x (x.x, x.x) indicates median and inter-quartile range.")
  } 
  if("Student's t-test (two-sided)" %in% methods.used$method.name) {
    footer <- c(footer, "x +/- x indicates mean +/- standard deviation.")
  }
  for(k in 1:nrow(methods.used)) {
    footer <- c(footer, paste0("(", methods.used$method[k], ") ", methods.used$method.name[k]))
  }
  
  # Calculate n for each group
  n <- list(n.grp1 = nrow(df[!is.na(df[[grpvar]]) & df[[grpvar]]==levels(df[[grpvar]])[1],]),
            n.grp2 = nrow(df[!is.na(df[[grpvar]]) & df[[grpvar]]==levels(df[[grpvar]])[2],]),
            n.combined = nrow(df[!is.na(df[[grpvar]]),]))
  
  return(tbl1(table = out,
                  n = n,
                  footer = footer,
                  caption = caption,
                  label = lbl))
}






#' Produces a tbl1w object, which contains summary statistics without grouping or bivariate tests.
#' 
#' @param df A data frame or tibble with Hmisc-brand labels.
#' @param testTypes A vector of length nrow(df) containing "t" for a t-test or "w" for a Wilcoxon rank-sum test; applies only to continuous variables. Leave NULL to allow the function to choose for you based on the distance between the mean and median. 
#' @param d Number of significant digits to report in (non-p-value) numbers in the tableone object.
#' @param p.digits Number of significant digits to report in p-values.
#' @param fisher.simulate.p If TRUE use simulated p-values for Fisher's exact test. Default is FALSE.
#' @param trunc_binary Logical. If TRUE print only one row for categorical variables with 2 levels. If FALSE, binary variables are printed on two rows. Default is TRUE. Alternatively, supply a named list of logicals indicating which variables should be printed on one row. Unnamed variables in the list will be printed on one row. 
#' @param lbl LaTeX label to be passed to tx() for labeling the table in a LaTeX document.
#' @param caption Text to be passed to tx() or md() to caption the table. 
#' @return A tbl1w object that can be formatted by tx() or md().
#' @examples
tableoneway <- function(df, summaryTypes=NULL, d=1, p.digits=3, fisher.simulate.p=FALSE, trunc_binary=TRUE, lbl="", caption="") {
  df %<>% as_tibble()
  
  # If the user specified a list of summary types of valid length for continuous data, use them; otherwise, set all to "auto".
  st <- rep("auto", length(colnames(df)))
  if(!is.null(summaryTypes)) {
    for(i in seq_along(summaryTypes)) {
      if(!names(summaryTypes)[[i]] %in% colnames(df)) {
        stop(paste0("Invalid summaryTypes name ", names(summaryTypes)[[i]]))
      } else {
        st[colnames(df) == names(summaryTypes)[[i]]] <- summaryTypes[[i]]
      }
    }
  }
  summaryTypes <- st
  
  # Convert trunc_binary to a vector the length of the list of summary variables
  if(is.logical(trunc_binary)) {
    trunc_binary <- rep(as.list(trunc_binary), ncol(df))
    names(trunc_binary) <- colnames(df)
  } else {
    missing_cols <- colnames(df)[!colnames(df) %in% names(trunc_binary)]
    newlist <- rep(as.list(TRUE), length(missing_cols))
    names(newlist) <- missing_cols
    trunc_binary <- append(trunc_binary, newlist)
  }
  
  summary_df <- tibble(col_name = colnames(df),
                       summary_type = summaryTypes)
  
  out <- map_dfr(colnames(df), 
                 function(cn) {
                   # Make sure the current variable is labeled; use the variable name if not.
                   if(label(df[[cn]]) == "") label(df[[cn]]) <- cn
                   
                   # CONTINUOUS VARIABLES
                   if(class(df[[cn]])[2] %in% c("integer", "numeric")) {
                     # If the user did not specify a summary type then just use a mean
                     summary_df %<>%
                       mutate(summary_type = case_when(col_name == cn & summary_type == "auto" ~ "mean",
                                                       TRUE ~ summary_type))
                     # mean +/- SD
                     if(summary_df$summary_type[summary_df$col_name==cn]=="mean") {
                       method <- "Mean"
                       newrow <- tibble(var = label(df[[cn]]),
                                        n = as.character(sum(!is.na(df[[cn]]))),
                                        summary = paste0(rnd(mean(df %>% pull(cn), na.rm=TRUE), d=d), 
                                                         " +/- ", 
                                                         rnd(sd(df %>% pull(cn), na.rm=TRUE), d=d)),
                                        method = method)
                     } else {
                       # Median +/- IQR
                       method <- "Median"
                       newrow <- tibble(var = label(df[[cn]]),
                                        n = as.character(sum(!is.na(df[[cn]]))),
                                        summary = paste0(rnd(median(df %>% pull(cn), na.rm=TRUE), d=d), 
                                                         " (", 
                                                         rnd(quantile(df %>% pull(cn), 0.25, na.rm=TRUE), d=d),
                                                         ", ",
                                                         rnd(quantile(df %>% pull(cn), 0.75, na.rm=TRUE), d=d),
                                                         ")"),
                                        method = method)
                       names(newrow) <- names(out)
                     }
                   } else { 
                     # CATEGORICAL VARIABLES
                     if(class(df[[cn]])[2] %in% c("logical", "factor")) {
                       method <- "Percent" # Why would i need to do this? 
                       
                       # Construct a contingency table 
                       tb <- table(df[[cn]])
                       
                       # Binary variables
                       if(nrow(tb)==2 & trunc_binary[[cn]]) {
                         newrow <- tibble(var = label(df[[cn]]),
                                          n = as.character(sum(!is.na(df[[cn]]))),
                                          summary = paste0(fmt.pct(tb[2]/sum(tb), latex=FALSE), " (", tb[2], ")"),
                                          method = method)
                         
                         # Variables with 3+ level categoricals or binaries when trunc_binary = FALSE
                       } else { 
                         # Header row
                         newrow <- tibble(var = label(df[[cn]]),
                                          n = as.character(sum(!is.na(df[[cn]]))),
                                          summary =  " ", 
                                          method = method) %>%
                           bind_rows(map_dfr(if(class(df[[cn]])[2]=="logical") { # Identify level labels
                             c("FALSE", "TRUE")
                           } else {
                             levels(df[[cn]])
                           },
                           function(lvl) { # Add one row for each level
                             tibble(var = paste0(" ", lvl),
                                    n = " ",
                                    summary = paste0(fmt.pct(sum(tb[lvl])/sum(tb), latex=FALSE), " (", sum(tb[lvl]), ")"),
                                    method = " ")
                           }))
                       }
                     }
                   }
                   return(newrow)
                 })
  
  # Generate footer
  footer <- c("")
  if("Median" %in% out$method) {
    footer <- c(footer, "x.x (x.x, x.x) indicates median and inter-quartile range.")
  } 
  if("Mean" %in% out$method) {
    footer <- c(footer, "x +/- x indicates mean +/- standard deviation.")
  }
  
  # Calculate n for each group
  n <- list(n.combined = nrow(df))
  
  return(tbl1w(table = out %>% select(-method),
               n = n,
               footer = footer,
               caption = caption,
               label = lbl))
}


#' Tabulate a set of variables by an N-dimensional grouping variable.
#'
#' @param df A data frame or tibble with Hmisc-brand labels.
#' @param grpvar The quoted name of a grouping variable that splits df into N cohorts.
#' @param testTypes A vector of length nrow(df) containing "mean" for means and SDs or "median" medians and IQRs; applies only to continuous variables. Leave NULL to allow the function to choose for you based on the distance between the mean and median. 
#' @param d Number of significant digits to report in numbers in the tbl1 object.
#' @param p.digits Not used.
#' @param fisher.simulate.p Not used.
#' @param trunc_binary If TRUE print only one row for categorical variables with 2 levels. If FALSE, binary variables are printed on two rows, with the variable name on a row above. Default is TRUE.
#' @param lbl LaTeX label to be passed to tx() for labeling the table in a LaTeX document.
#' @param caption Text to be passed to tx() or md() to caption the table. 
#'
#' @return A tbl1 object that can be formatted by tx() or md().
#' @export
#'
#' @examples
tablenway <- function(df, grpvar, testTypes=NULL, d=1, p.digits=3, fisher.simulate.p=FALSE, trunc_binary=TRUE, combined=FALSE, lbl="", caption="") {
  df <- as.data.frame(df)
  
  # Define indentations for HTML vs other types of knitr outputs
  if(is_html_output()) {
    indent_start <- "<span style='margin-left:30px;'>"
    indent_end <- "</span>"
  } else {
    indent_start <- " - "
    indent_end <- ""
  }
  
  # Assign a LaTeX label if there isn't one already
  if (lbl=="") lbl <- an.id(9)
  
  # If the user specified a list of test types of valid length for continuous data, use them; otherwise, set all to "auto".
  if(length(testTypes)!=length(colnames(df))) {
    testTypes <- rep("auto", length(colnames(df)))
  }
  
  test.df <- data.frame(col.name = colnames(df),
                        test.type = testTypes,
                        stringsAsFactors = FALSE)
  
  # Create the table, beginning with an empty data frame
  out <- new.df(cols=length(levels(df[[grpvar]]))+4)
  for (i in seq_along(cols.except(df, grpvar))) {  
    # Identify the variable to summarize for the current iteration
    sumvar <- colnames(df)[!(colnames(df) %in% grpvar)][i]
    
    # Make sure the current variable is labeled; use the variable name if not.
    if(label(df[[sumvar]]) == "") label(df[[sumvar]]) <- sumvar
    
    # If the variable is continuous, present either a median or a mean
    if(any(class(df[[sumvar]]) %in% c("integer", "numeric"))) {
      # If the user did not specify a measure of center then use a median if the distance between the mean and 
      # median is > 0.25 standard deviations; otherwise use a mean.
      if(test.df$test.type[test.df$col.name==sumvar] == "auto") {
        if(abs(mean(df[[sumvar]], na.rm=TRUE) - median(df[[sumvar]], na.rm=TRUE))/sd(df[[sumvar]], na.rm=TRUE) <= 0.25) {
          test.df$test.type[test.df$col.name==sumvar] <- "mean"
        } else {
          test.df$test.type[test.df$col.name==sumvar] <- "median"
        }
      }
      
      # Mean/median
      if(test.df$test.type[test.df$col.name==sumvar]=="mean") {
        suppressWarnings(center <- map_dfr(levels(df[[grpvar]]), 
                                           function(lvl) data.frame(ctr = paste0(rnd(mean(df[[sumvar]][df[[grpvar]]==lvl], na.rm=TRUE), d=d), 
                                                                                 " +/- ", 
                                                                                 rnd(sd(df[[sumvar]][df[[grpvar]]==lvl], na.rm=TRUE), d=d)))))
        overall_ctr <- paste0(rnd(mean(df[[sumvar]], na.rm=TRUE), d=d), 
                              " $\\pm$ ", 
                              rnd(sd(df[[sumvar]], na.rm=TRUE), d=d))
        method <- "Means"
      } else {
        suppressWarnings(center <- map_dfr(levels(df[[grpvar]]), 
                                           function(lvl) data.frame(ctr = paste0(rnd(median(df[[sumvar]][df[[grpvar]]==lvl], na.rm=TRUE), d=d), 
                                                                                 paste0(" (", paste(rnd(quantile(df[[sumvar]][df[[grpvar]]==lvl], 
                                                                                                                 c(.25, .75), na.rm=TRUE), 
                                                                                                        d=d), 
                                                                                                    collapse=", "), ")")))))
        overall_ctr <- paste0(rnd(mean(df[[sumvar]], na.rm=TRUE), d=d), 
                              paste0(" (", paste(rnd(quantile(df[[sumvar]], 
                                                              c(.25, .75), na.rm=TRUE), 
                                                     d=d), 
                                                 collapse=", "), ")"))
        method <- "Medians"
      }
      newrow <- as.data.frame(cbind(label(df[[sumvar]]),
                                    sum(!is.na(df[[sumvar]]) & !is.na(df[[grpvar]])),
                                    t(center),
                                    overall_ctr,
                                    method))
      
      names(newrow) <- names(out)
      out <- rbind(out, newrow)
    }
    
    # If the variable is categorical, present Ns and percentages
    if(any(class(df[[sumvar]]) %in% c("logical", "factor"))) {
      
      method <- "pct"
      
      # Construct a contingency table 
      tb <- table(df[[sumvar]], df[[grpvar]])
      
      # Identify level labels:
      if(any(class(df[[sumvar]])=="logical")) {
        level.labs <- c("FALSE", "TRUE")
      } else {
        level.labs <- levels(df[[sumvar]])
      }
      
      if(nrow(tb)==2 & trunc_binary) {   
        newrow <- data.frame(cbind(label(df[[sumvar]]),
                                   sum(!is.na(df[[sumvar]]) & !is.na(df[[grpvar]])),
                                   suppressWarnings(cnts <- t(map_dfr(seq_along(levels(df[[grpvar]])), 
                                                                      function(lvl) data.frame(cnt = paste0(fmt.pct(tb[2,lvl]/sum(tb[,lvl]), 
                                                                                                                    latex=FALSE), 
                                                                                                            " (", tb[2,lvl], ")"))))),
                                   paste0(fmt.pct(sum(tb[2,])/sum(tb[,]), latex=FALSE), " (", sum(tb[2,]), ")"),
                                   method))
        names(newrow) <- names(out)
        out <- rbind(out, newrow)
      } else {
        newrow <- data.frame(cbind(label(df[[sumvar]]),
                                   sum(!is.na(df[[sumvar]]) & !is.na(df[[grpvar]])),
                                   t(rep(" ", length(levels(df[[grpvar]]))+1)),
                                   method))
        names(newrow) <- names(out)
        out <- rbind(out, newrow)
        
        method <- ""
        for (j in 1:nrow(tb)) {
          newrow <- data.frame(cbind(paste0(level.labs[j]),
                                     " ",
                                     suppressWarnings(cnts <- t(map_dfr(seq_along(levels(df[[grpvar]])), 
                                                                        function(lvl) data.frame(cnt = paste0(fmt.pct(tb[j,lvl]/sum(tb[,lvl]), 
                                                                                                                      latex=FALSE), 
                                                                                                              " (", tb[j,lvl], ")"))))),
                                     paste0(fmt.pct(sum(tb[j,])/sum(tb[,]), latex=FALSE), " (", sum(tb[j,]), ")"),
                                     method))
          names(newrow) <- names(out)
          out <- rbind(out, newrow)
        }
      }
    }
  }
  
  
  for (l in 1:ncol(out)) {
    out[,l] <- as.character(out[,l])
  }
  
  colnames(out) <- c(" ", "n", levels(df[[grpvar]]), "Combined", "method.name")
  
  # Identify methods used
  methods.used <- data.frame(method.name = unique(out$method.name[out$method.name!=""]),
                             method = letters[seq(1:length(unique(out$method.name[out$method.name!=""])))],
                             stringsAsFactors = FALSE)
  out$sort <- seq(1:nrow(out))
  out <- merge(out, methods.used,
               by="method.name",
               all.x=TRUE)
  out <- out[order(out$sort),]
  out$sort <- NULL
  out$method[is.na(out$method)] <- ""
  out$method.name <- NULL
  
  # Replace NaN% with 0%
  out[[levels(df[[grpvar]])[1]]] <- gsub(pattern="NaN", 
                                         replacement="0",
                                         x=out[[levels(df[[grpvar]])[1]]])
  out[[levels(df[[grpvar]])[1]]] <- gsub(pattern="NaN", 
                                         replacement="0",
                                         x=out[[levels(df[[grpvar]])[1]]])
  
  # Generate footer
  footer <- c()
  if("Medians" %in% methods.used$method.name) {
    footer <- c(footer, "x.x (x.x, x.x) indicates median and inter-quartile range.")
  } 
  if("Means" %in% methods.used$method.name) {
    footer <- c(footer, "x +/- x indicates mean +/- standard deviation.")
  }
  if("pct" %in% methods.used$method.name) {
    footer <- c(footer, " ")
  }
  
  # Calculate n for each group
  N <- purrr::map(seq_along(levels(df[[grpvar]])),
           function(i) nrow(df[!is.na(df[[grpvar]]) & df[[grpvar]]==levels(df[[grpvar]])[i],]))
  N$combined <- nrow(df[!is.na(df[[grpvar]]),])
  
  names(N) <- c(levels(df[[grpvar]]), "combined")
  
  if(!combined) {
    out %<>% select(-Combined)
    N[["combined"]] <- NULL
  }
  
  return(tblnw(table = out,
                  n = N,
                  footer = footer,
                  caption = caption,
                  label = lbl))
}

