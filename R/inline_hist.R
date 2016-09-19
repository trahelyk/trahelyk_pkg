# inline.hist <- function(x) {
#   counts <- as.vector(table(cut(x, breaks=100)))
#   maxcounts <- max(counts)
#   
#   cat("\\setlength{\\unitlength}{0.0008in}\\hfill", 
#       "\\begin{picture}(0,0)(800,0)", "\\linethickness{1pt}\n", 
#       sep = "", file = "", append = TRUE)
#   for (i in (1:100)[counts > 0]) {
#     cat("\\put(", round(1000 * (i - 1) * 1.5/100), ",0){\\line(0,1){", 
#         max(1, round(1000 * counts[i]/maxcounts * 0.1)), 
#         "}}\n", sep = "", file = "", append = TRUE)
#   }
#   cat("\\end{picture}\n", file = "", append = TRUE)
# }

inline.hist <- function(x, spacing=1000) {
  counts <- as.vector(table(cut(x, breaks=100)))
  maxcounts <- max(counts)
  
  out.txt <- paste0("\\setlength{\\unitlength}{0.0008in}\\hfill", 
      "\\begin{picture}(0,0)(", spacing, ",0)", "\\linethickness{1pt}\n")
  for (i in (1:100)[counts > 0]) {
    out.txt <- paste0(out.txt , "\\put(", round(1000 * (i - 1) * 1.5/100), ",0){\\line(0,1){", 
        max(1, round(1000 * counts[i]/maxcounts * 0.1)), 
        "}}\n")
  }
  return(paste0(out.txt, "\\end{picture}\n"))
}
