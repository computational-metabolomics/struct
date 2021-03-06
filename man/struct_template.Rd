% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/struct_templates.R
\name{struct_template}
\alias{struct_template}
\title{StRUCT templates}
\usage{
struct_template(
  template = "model",
  output,
  in_editor = TRUE,
  overwrite = FALSE
)
}
\arguments{
\item{template}{the type of object you want a template for e.g. 'model'}

\item{output}{the name/path of the output file}

\item{in_editor}{TRUE/FALSE to open the created file in the default editor}

\item{overwrite}{= TRUE/FALSE to overwrite file if exists already}
}
\value{
A template is created at the output location specified
}
\description{
Create a struct template
}
\examples{
\dontrun{
struct_template('model','example.R',FALSE)
}

}
