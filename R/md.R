# -------------------------------------------------------------------------------- 
# Define counter functions for tables and figures
# --------------------------------------------------------------------------------
library("captioner")
tbls <- captioner(prefix="Table")
tblcap <- function(caption, name=an.id(10)) {
  tbls(name=name, caption=caption)
}
figs <- captioner(prefix="Figure")
figcap <- function(caption, name=an.id(10)) {
  figs(name=name, caption=caption)
}

# -------------------------------------------------------------------------------- 
# Add vertical space
# --------------------------------------------------------------------------------
vspace <- function(x) {
  cat(rep("<br>", x))
}

# -------------------------------------------------------------------------------- 
# Define a generic function for formatting things in RMarkdown
# --------------------------------------------------------------------------------
md <- function(x, word=FALSE, row.names=FALSE, ...) UseMethod("md")
md.default <- function(x) {
  print(x)
}

# -------------------------------------------------------------------------------- 
# Define a function that customizes or omits style divs for Word
# --------------------------------------------------------------------------------
word_style <- function(word, wordstyles, style, cat_txt) {
  if(word) {
    div_open <- paste0("<div custom-style='", wordstyles[[style]], "'>")
    div_close <- "</div>"
  } else {
    div_open <- div_close <- ""
  }
  return(cat(div_open, cat_txt, div_close, sep="  \n"))
}

#' Display a tableone table in RMarkdown
#'
#' @param A tableone object, produced by make_tableone
#' @param word Logical indicating whether the output should be formatted for an MS Word 
#' @param wordstyles Name of the MS Word stylesheet to be used for formatting. NA if word==FALSE.
#'
#' @return Side effects.
#' @export
#'
#' @examples
md.tableone <- function(x, word=FALSE, wordstyles=NA) {
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
  word_style(word=word, wordstyles=wordstyles, style="tablecaption", cat_txt = paste0(tbls(x$label, x$caption)))
  
  # Print the table. Note that we must wrap kable in a print function if we want to have a footer (weirdness with the kable function).
  print(kable(x$table,
              format = "markdown",
              row.names = FALSE))
  
  # Print the footer.
  word_style(word=word, wordstyles=wordstyles, style="tablefooter", cat_txt = x$footer)
  cat("<br>")
}

# -------------------------------------------------------------------------------- 
# Define a function that displays a tableoneway object in RMarkdown
# --------------------------------------------------------------------------------
md.tableoneway <- function(x, include_n=FALSE) {
  # Reformat +/- in the table
  for(i in 3:ncol(x$table)) {
    x$table[,i] <- gsub("\\+/-", "$\\\\pm$", x$table[,i])
  }
  
  # Bold the table headers in markdown.
  colnames(x$table)[c(2,3)] <- paste0("**", colnames(x$table)[c(2,6)], "**") 
  
  # Reformat the footer for markdown.
  for(l in letters[1:10]) {
    x$footer <- gsub(paste0("\\(", l, "\\)"), paste0("^", l, "^"), x$footer)
  }
  x$footer <- gsub("\\+/-", "$\\\\pm$", x$footer)
  x$footer <- paste0("_", x$footer, "_")
  
  # Print the table caption.
  cat(paste0("\ \n\ \n\ \ \n", tbls(x$label, x$caption), "\n"))
  
  # Suppress the "n" column if specified
  if(!include_n) {
    x$table <- x$table[,c(1,3)]
  }
  
  # Print the table. Note that we must wrap kable in a print function if we want to have a footer (weirdness with the kable function).
  print(kable(x$table,
              format = "markdown",
              row.names = FALSE))
  
  # Print the footer.
  cat("\n***\n", x$footer, sep="  \n")
  cat("<br>")
}

#' Display a trahble-based tabulation in RMarkdown
#'
#' @param A trahble object, produced by trahbulate
#' @param word Logical indicating whether the output should be formatted for an MS Word 
#' @param wordstyles Name of the MS Word stylesheet to be used for formatting. NA if word==FALSE.
#'
#' @return NULL
#' @export
#'
#' @examples
md.trahble <- function(x, word=FALSE, wordstyles=NA) {
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
  
  # Print the table caption.
  word_style(word=word, wordstyles=wordstyles, style="tablecaption", cat_txt = paste0(tbls(x$label, x$caption)))
  
  # Print the table. Note that we must wrap kable in a print function if we want to have a footer (weirdness with the kable function).
  print(kable(x$table,
              format = "markdown",
              row.names = FALSE))
  
  # Print the footer.
  word_style(word=word, wordstyles=wordstyles, style="tablefooter", cat_txt = x$footer)
  cat("<br>")
}


# -------------------------------------------------------------------------------- 
# Wrap a data frame in RMarkdown
# --------------------------------------------------------------------------------
md.data.frame <- function(x, row.names=FALSE, col.names=NA) {
  print(kable(x,
              format = "markdown",
              row.names = row.names,
              col.names = col.names))
  cat("\n\n")
}