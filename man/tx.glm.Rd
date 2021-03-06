% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latex.R
\name{tx.glm}
\alias{tx.glm}
\title{Present a glm model object in a LaTeX table}
\usage{
\method{tx}{glm}(
  x,
  d = 3,
  intercept = FALSE,
  varnames = NULL,
  lbl = "",
  caption = "",
  coef.head = "OR",
  transf = exp,
  print_mdl = TRUE
)
}
\arguments{
\item{d}{An integer specifying the number of significant digits to be presented in the table.}

\item{intercept}{Logical indicating whether the table should include a row for the fixed intercept. Default is FALSE.}

\item{varnames}{Character vector of length length(names(mdl$coefficients$fixed)) specifying the names of fixed effects in the model.}

\item{lbl}{LaTeX label for the table.}

\item{caption}{Caption for the table.}

\item{coef.head}{Word or phrase to use as a header for coefficient estimates. Default is OR.}

\item{transf}{Function to transform beta coefficients. Default is exp().}

\item{print_mdl}{Logical indicating whether the summary table should be wrapped in LaTeX.}

\item{mdl}{A glm object}
}
\value{
A LaTeX or clear-text table.
}
\description{
Present a glm model object in a LaTeX table
}
