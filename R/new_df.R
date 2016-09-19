# Generate a new empty data fram with the specified number of columns.
new.df <- function(cols) {
  data.frame(matrix(vector(), 
                    0, cols, 
                    dimnames=list(c(), 
                                  c(1:cols))), 
             stringsAsFactors=F)
}
