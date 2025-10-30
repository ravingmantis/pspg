\name{pspg}
\alias{pspg}
\alias{pspg.data.frame}
\alias{pspg.list}
\alias{pspg.default}

\title{View data structures with PSPG}
\description{
  Use Postgres Pager to view data structures
}

\usage{
  pspg(x, ...)
}

\arguments{
  \item{x}{Object to view, will be translated into CSV}
  \item{...}{Additional PSPG parameters}
}

\details{
  TODO: Notes about how we translate to CSV
}

\value{\var{x}, invisibly (i.e. so you can use it in a pipeline)}

\examples{
if (interactive()) {
  # View a data.frame
  pspg(data.frame(a=1:10, b=2:11 * 20))

  # View a list
  pspg(list(a=1:1000))
}
}
