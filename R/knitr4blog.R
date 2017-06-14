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
#' # change_codeclass("abc.html", "language-r")
#' @export
change_codeclass <- function(fileName, className){
  if (className == "r") {
    return(NA)
  } else {
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
}

#' knitr your Rmd file with className = "language-r"
#'
#' @param  fileName A name of your Rmd file
#' @param  className A className that your R code class to be
#' @return kniting your Rmd file
#' @examples
#' # fileName should not have blank space
#' # knitr4blog("abc.Rmd")
#' @importFrom rmarkdown render
#' @export
knitr4blog <- function(fileName, className = "r"){
  if (length(grep(" ", fileName)) != 0) {
    warning("your fileName has blank, please use '_' instead of the blank. ex: 'file_name.Rmd'")
    return(NA)
  } else {
    render(fileName)
    change_codeclass(paste0(gsub(".Rmd","", fileName),".html"),
                     className)
  }
}

#  URL maker (Client-side flow - Implicit Grant)
#' Tistory token-page url maker
#' @param  client_id the id that is obtained from Tistory
#' @param  redirect_uri the redirect uri that is submitted to Tistory
#' @return browser automatically will be open containing url that token for blogging
#' @examples
#' my_id <- "your client_id here"
#' my_uri <- "your redirect_uri address here"
#' token_url_maker(my_id, my_uri)
#' @export
token_url_maker <- function(client_id = "3a5ef7ff3d180eac94d5df5e58ba1768",
                            redirect_uri = "http://issactoast.com/77"){
  base_url <- "https://www.tistory.com/oauth/authorize"
  url <- sprintf("%s?client_id=%s&redirect_uri=%s&response_type=token",
                 base_url, client_id, redirect_uri)
  browseURL(url, browser = getOption("browser"))
}


#  Posting Rmd using httr package
#' Post your Rmd file to Tistory
#' @param  fileName the .Rmd file name you want to post to your blog
#' @param  my_blogName your blogName xxx from blog address "http://xxx.tistory.com"
#' @param  token the token you have obtained from the url generated from token_url_maker function
#' @param  modify default value is NULL. When modify set to NULL it means your Rmd will be posted as a new post
#' If you want to modify the existing post, feed the number of the post to the modify variable.
#' @param  tag the tag of your Rmd file. you can use multiple tags using c("tagA", "tagB", ...)
#' @param  ... you can use className = "language-r" option in knitr4blog function
#' @return posted blog on your blog
#' @examples
#' # fileName should not have blank space
#' # Assume your file name is "test.Rmd"
#' # Assume your blog address is "http://issactoast.tistory.com"
#' my_token <- "your obtained token here"
#' # post2Tistory("test.Rmd", "issactoast", my_token)
#' # post2Tistory("test.Rmd", "issactoast", my_token, modify = 42)
#' @importFrom httr POST
#' @export
post2Tistory <- function(fileName,
                         my_blogName,
                         token,
                         modify = NULL,
                         tag = NULL,
                         ...){
  knitr4blog(fileName, ...)

  # html read    
  my_contents <- readLines(paste0(gsub(".Rmd","", fileName),".html"))
  
  # grab title
  my_title <- my_contents[grep("<title>", my_contents)]
  my_title <- gsub("<title>|</title>", "", my_title)
  
  # grab meta tag and h1 tag and delete
  my_contents <- my_contents[-grep("<meta", my_contents)]
  my_contents <- my_contents[grep("</h1>", my_contents)]

  # make html again  
  my_contents <- paste(as.character(my_contents), collapse = "\n")

  base_url  <- "https://www.tistory.com/apis/post/write"
  fbody <- list(access_token = token,
                blogName= my_blogName,
                title = my_title,
                content= my_contents)
  if (is.null(modify) != TRUE) {
    base_url <- "https://www.tistory.com/apis/post/modify"
    fbody$postId <- as.character(modify)
  }
  if (is.null(tag) != TRUE) {
      fbody$tag <- as.character(tag)
  }
  POST(base_url, body = fbody, encode = "form")
}
