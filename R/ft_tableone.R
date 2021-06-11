#' Present a tableone table as a flextable
#'
#' @param x A tbl1 object
#' @param col_keys columns names/keys to display (passed on to flextable())
#' @param include_combined logical indicating whether the table should include the combined column.
#'
#' @return A flextable object
#' @export
ft.tbl1 <- function(x, col_keys="", include_combined=FALSE) {
  # Add Ns to the column headers
  colnames(x$table)[3:(ncol(x$table) - 2)] <- map_chr(3:(ncol(x$table) - 2), 
                                                      function(i) paste0(colnames(x$table)[i], "\n (n=", 
                                                                         scales::comma(x$n[[i - 2]]), ")"))
  
  # Figure out which columns to show
  excl_cols <- c("n", "method", "rownums")
  if(!include_combined) excl_cols <- c(excl_cols, 
                                       colnames(x$table)[str_detect(colnames(x$table), "[Cc]ombined")])
  if(col_keys == "") {
    col_keys <- colnames(x$table)[!(colnames(x$table) %in% excl_cols)]
  }
  
  # Figure out which rows should be bolded, which indented to show category levels
  x$table$rownums <- 1:nrow(x$table)
  main_rows <- x$table$rownums[nchar(str_trim(x$table$n, "both")) > 0]
  lvl_rows <- x$table$rownums[!(x$table$rownums %in% main_rows)]
  
  # Return the flextable object
  return(flextable(x$table,
                   col_keys = col_keys) %>%
           valign(valign="bottom", part="all") %>%
           # autofit() %>%
           hrule(rule = "exact") %>%
           # height_all(height=0.1, part="all") %>%
           # height(i = main_rows[main_rows>1], height = 0.3, part="body") %>%
           align(j = 2:length(col_keys), align = "center", part="all") %>%
           bold(bold=TRUE, part="header") %>%
           bold(bold=TRUE, j=1, i=main_rows) %>%
           padding(i = main_rows[main_rows>1], 
                   padding.top = 4, 
                   padding.bottom = 0,
                   part="body") %>%
           padding(i = c(1,lvl_rows), 
                   padding.top = 0, 
                   padding.bottom = 0,
                   part="body") %>%
           padding(j=1, i=lvl_rows, padding.left = 20) %>%
           font(fontname = "Cambria", part="all") %>%
           fontsize(size=10, part="all") %>%
           height_all(height=0.21, part="all") %>%
           hrule(rule = "exact") %>%
           autofit())
}

#' Present a one-way table as a flextable
#'
#' @param x A tbl1w object
#' @param col_keys columns names/keys to display (passed on to flextable())
#'
#' @return A flextable object
#' @export
ft.tbl1w <- function(x, col_keys="") {
  
  # Figure out which columns to show
  if(col_keys == "") {
    col_keys <- colnames(x$table)[!(colnames(x$table) %in% c("n", "method", "rownums"))]
  }
  
  # Figure out which rows should be boldface, which indented to show category levels
  x$table$rownums <- 1:nrow(x$table)
  main_rows <- x$table$rownums[nchar(str_trim(x$table$n, "both")) > 0]
  lvl_rows <- x$table$rownums[!(x$table$rownums %in% main_rows)]
  
  # Return the flextable object
  return(flextable(x$table,
                   col_keys = col_keys) %>%
           valign(valign="bottom", part="all") %>%
           # autofit() %>%
           hrule(rule = "exact") %>%
           # height_all(height=0.1, part="all") %>%
           # height(i = main_rows[main_rows>1], height = 0.3, part="body") %>%
           align(j = 2:length(col_keys), align = "center", part="all") %>%
           bold(bold=TRUE, part="header") %>%
           bold(bold=TRUE, j=1, i=main_rows) %>%
           padding(i = main_rows[main_rows>1], 
                   padding.top = 4, 
                   padding.bottom = 0,
                   part="body") %>%
           padding(i = c(1,lvl_rows), 
                   padding.top = 0, 
                   padding.bottom = 0,
                   part="body") %>%
           padding(j=1, i=lvl_rows, padding.left = 10) %>%
           font(fontname = "Cambria", part="all") %>%
           fontsize(size=10, part="all") %>%
           height_all(height=0.21, part="all") %>%
           hrule(rule = "exact") %>%
           autofit())
}

#' Present an n-way table as a flextable
#'
#' @param x A tbl1w object
#' @param col_keys columns names/keys to display (passed on to flextable())
#'
#' @return A flextable object
#' @export
ft.tblnw <- function(x, col_keys="") {
  # Add Ns to the column headers
  colnames(x$table)[3:(ncol(x$table) - 1)] <- map_chr(3:(ncol(x$table) - 
                                                           1), function(i) paste0(colnames(x$table)[i], "\n (n=", 
                                                                                  scales::comma(x$n[[i - 2]]), ")"))
  
  # Figure out which columns to show
  if(col_keys == "") {
    col_keys <- colnames(x$table)[!(colnames(x$table) %in% c("n", "method", "rownums"))]
  }
  
  # Figure out which rows should be bolded, which indented to show category levels
  x$table$rownums <- 1:nrow(x$table)
  main_rows <- x$table$rownums[nchar(str_trim(x$table$n, "both")) > 0]
  lvl_rows <- x$table$rownums[!(x$table$rownums %in% main_rows)]
  
  # Return the flextable object
  return(flextable(x$table,
                   col_keys = col_keys) %>%
           valign(valign="bottom", part="all") %>%
           # autofit() %>%
           hrule(rule = "exact") %>%
           # height_all(height=0.1, part="all") %>%
           # height(i = main_rows[main_rows>1], height = 0.3, part="body") %>%
           align(j = 2:length(col_keys), align = "center", part="all") %>%
           bold(bold=TRUE, part="header") %>%
           bold(bold=TRUE, j=1, i=main_rows) %>%
           padding(i = main_rows[main_rows>1], 
                   padding.top = 4, 
                   padding.bottom = 0,
                   part="body") %>%
           padding(i = c(1,lvl_rows), 
                   padding.top = 0, 
                   padding.bottom = 0,
                   part="body") %>%
           padding(j=1, i=lvl_rows, padding.left = 20) %>%
           font(fontname = "Cambria", part="all") %>%
           fontsize(size=10, part="all") %>%
           height_all(height=0.21, part="all") %>%
           hrule(rule = "exact") %>%
           autofit())
}
