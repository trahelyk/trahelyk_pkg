# -------------------------------------------------------------------------------- 
# Define counter functions for tables and figures
# --------------------------------------------------------------------------------
library("captioner")
tbls <- captioner(prefix="Table")
figs <- captioner(prefix="Figure")

# -------------------------------------------------------------------------------- 
# Define a generic function for formatting things in RMarkdown
# --------------------------------------------------------------------------------
md <- function(x, row.names=FALSE) UseMethod("md")
md.default <- function(x) {
  print(x)
}

# -------------------------------------------------------------------------------- 
# Define a function that displays a tableone object in RMarkdown
# --------------------------------------------------------------------------------
md.tableone <- function(x) {
  # Combine the method with the p-value and format it as a superscript.
  x$table$p <- paste0(x$table$p, "^", x$table$method, "^")
  x$table$method <- NULL
  x$table$p[x$table$p==". "] <- "."
  x$table$p[x$table$p==" ^^"] <- " "
  
  # Reformat +/- in the table
  for(i in 3:5) {
    x$table[,i] <- gsub("\\+/-", "$\\\\pm$", x$table[,i])
  }
  
  # Bold the table headers in markdown.
  colnames(x$table)[c(2,6)] <- paste0("**", colnames(x$table)[c(2,6)], "**") 
  colnames(x$table)[3] <- paste0("**", colnames(x$table)[3], " (n=", x$n$n.grp1, ")**")
  colnames(x$table)[4] <- paste0("**", colnames(x$table)[4], " (n=", x$n$n.grp2, ")**")
  colnames(x$table)[5] <- paste0("**", colnames(x$table)[5], " (n=", x$n$n.combined, ")**")
  # colnames(x$table)[4:5] <- paste0("**", colnames(x$table)[3:5], " (n=", x$ "**")
  
  # Reformat the footer for markdown.
  for(l in letters[1:10]) {
    x$footer <- gsub(paste0("\\(", l, "\\)"), paste0("^", l, "^"), x$footer)
  }
  x$footer <- gsub("\\+/-", "$\\\\pm$", x$footer)
  x$footer <- paste0("_", x$footer, "_")
  
  # Print the table caption.
  cat(paste0("##### ", tbls(x$label, x$caption), "\n***"))
  
  # Print the table. Note that we must wrap kable in a print function if we want to have a footer (weirdness with the kable function).
  print(kable(x$table,
              format = "markdown",
              row.names = FALSE))
  
  # Print the footer.
  cat("\n***\n", x$footer, sep="\n\n")
  cat("<br><br><br>")
}

# -------------------------------------------------------------------------------- 
# Wrap a data frame in RMarkdown
# --------------------------------------------------------------------------------
md.tbl_df <- function(x, row.names=FALSE) {
  print(kable(x,
              format = "markdown",
              row.names = row.names))
  cat("\n\n")
}

md.data.frame <- function(x, row.names=FALSE) {
  print(kable(x,
              format = "markdown",
              row.names = row.names))
  cat("\n\n")
}