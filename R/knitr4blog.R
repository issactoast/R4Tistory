# filename should have no space
# recomend you have a '_' sign instead of having a blank between
# words
# example of the use of function:
# knitr4blog("fileName.Rmd")

#' Change code className in html file
#'
#' @param  fileName A name of your html file
#' @param  className A className that your file to be
#' @return html file but diffent className for the r code chunk
#' @examples
#' change_codeclass("abc.html", "language-r")
change_codeclass <- function(fileName, className){
  myweb <- readLines(fileName)
  myweb <- gsub("pre class=\"r\"",
                paste0("pre class=\"", className,"\""),
                myweb)
  myweb <- paste(as.character(myweb), collapse = "\n")
  write.table(myweb,
              file = fileName,
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
}

#' knitr your Rmd file with className = "language-r"
#'
#' @param  fileName A name of your Rmd file
#' @param  className A className that your R code class to be
#' @return kniting your Rmd file
#' @examples
#' # fileName should not have blank space
#' knitr4blog("abc.Rmd")
knitr4blog <- function(fileName, className = "language-r"){
  if (grep(" ", fileName) != 0) {
    warning("your fileName has blank, please use '_' instead of the blank. ex: 'file_name.Rmd'")
    return(NA)
  }
  rmarkdown::render(fileName)
  change_codeclass(paste0(gsub(".Rmd","", fileName),".html"),
                   className)
}
