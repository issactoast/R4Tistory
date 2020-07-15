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

#' Extract and change HTML to Tistory input html
#'
#' @param  fileName A name of your html file
#' @param  className A className that your file to be
#' @return html file but diffent className for the r code chunk
#' @examples
#' # change2Tistoryhtml("abc.html")
#' # change2Tistoryhtml("abc.html", "language-r")
#' @export
change2Tistoryhtml <- function(fileName, className = "language-r"){
  myweb <- readLines(fileName)
  
  # body part grab
  myweb <- myweb[(grep("<h4 class=\"date\">", myweb)+1):(grep("<!-- code folding -->", myweb)-1)]
  
  # h tag change
  myweb <- gsub("<h1>",
                "<h2>",
                myweb)
  
  myweb <- gsub("</h1>",
                "</h2>",
                myweb)
  
  myweb <- gsub("<h2>",
                "<h2 data-ke-size=\"size26\">",
                myweb)
  myweb <- gsub("<h3>",
                "<h3 data-ke-size=\"size26\">",
                myweb)
  myweb <- gsub("<h4>",
                "<h4 data-ke-size=\"size26\">",
                myweb)
  
  # p tag change
  myweb <- gsub("<p>",
                "<p data-ke-size=\"size18\">",
                myweb)
  
  # R code change
  myweb <- gsub("pre class=\"r",
                paste0("pre class=\"", className),
                myweb)
  
  # ul tag change
  myweb <- gsub("<ul>",
                "<ul style=\"list-style-type: disc;\" data-ke-list-type=\"disc\">",
                myweb)
  
  # hr tag change
  myweb <- gsub("<hr />",
                "<hr contenteditable=\"false\" data-ke-type=\"horizontalRule\" data-ke-style=\"style5\" />",
                myweb)
  
  # blockquote tag change
  myweb <- gsub("<blockquote>",
                "<blockquote data-ke-size=\"size18\" data-ke-style=\"style3\">",
                myweb)
  
  # title capture
  # my_title <- myweb[grep("<title>", myweb)]
  # my_title <- gsub("<title>|</title>", "", my_title)
  
  # div tag delete
  myweb <- gsub("<div[^>]*>|</div>", "", myweb)
  
  myweb <- paste(as.character(myweb), collapse = "\n")
  write.table(myweb,
              file = paste0(fileName,"Tistory"),
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
}


#' knitr your Rmd file with className = "language-r"
#'
#' @param  fileName A name of your Rmd file
#' @param  className A className that your R code class to be
#' @param  ... you can use other options for rmarkdown::render such as encoding option
#' @return kniting your Rmd file
#' @examples
#' # fileName should not have blank space
#' # knitr4blog("abc.Rmd")
#' @importFrom rmarkdown render
#' @export
knitr4blog <- function(fileName, className = "r", ...){
  if (length(grep(" ", fileName)) != 0) {
    warning("your fileName has blank, please use '_' instead of the blank. ex: 'file_name.Rmd'")
    return(NA)
  } else {
    render(fileName, ...)
    change_codeclass(paste0(gsub(".Rmd","", fileName),".html"),
                     className)
  }
}

#  URL maker (Client-side flow - Implicit Grant)
#' Tistory token-page url maker
#' @param  NoInput this function has no inputs.
#' @return browser automatically will be open containing url that token for
#'  posting. the redirected web page will be taining news of this package
#' @examples
#' # Getting token from url
#' # token_url_maker()
#' @export
token_url_maker <- function(NoInput = NULL){
  if (is.null(NoInput) != TRUE) {
    print("version.0.1.4 provides client_id and uri")
    print("please type only 'token_url_maker()'.")
  }
  print("redirect to package website. copy token from browser's URL.")
  client_id = "3a5ef7ff3d180eac94d5df5e58ba1768"
  redirect_uri = "http://issactoast.com/84"
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
#' @param  tag the tag of your Rmd file. you can use multiple tags using "tagA, tagB, tagC"
#' @param  encoding switch encoding. defalut option follows system default
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
                         encoding = getOption("encoding"),
                         ...){
  knitr4blog(fileName, encoding = encoding, ...)

  # html read
  my_contents <- readLines(paste0(gsub(".Rmd","", fileName),".html"), encoding = encoding)

  # grab title
  my_title <- my_contents[grep("<title>", my_contents)]
  my_title <- gsub("<title>|</title>", "", my_title)

  # grab meta tag and h1 tag and delete
  my_contents <- my_contents[-grep("</h1>", my_contents)]

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
      fbody$tag <- tag
  }
  POST(base_url, body = fbody, encode = "form")
}


word2Tistory <- function(fileName,
                         my_blogName,
                         token,
                         modify = NULL,
                         tag = NULL,
                         encoding = getOption("encoding"),
                         ...){
  fileName <- "README.md"
  # grab filename without dot
  fileNameNoDot <- gsub(".docx","", fileName)

  # transform word to md
  system(paste0("pandoc -s ", fileNameNoDot, ".docx -t markdown -o ", fileNameNoDot, ".md"))

  # read md
  my_md <- readLines(paste0(fileNameNoDot,".md"), encoding = encoding)

  # grab title & subtitle
  my_subtitle <- my_md[grep("^subtitle:$", my_contents)]
  my_title <- my_md[grep("^title:$", my_contents)]


  my_test <- rbind(my_md[3], my_md[1])
  paste(as.character(my_test), collapse = "\n")
  mode(my_md)

  # transform md
  knitr4blog(paste0(fileNameNoDot,".Rmd"), encoding = encoding, ...)

  # html read
  my_contents <- readLines(paste0(gsub(".Rmd","", fileName),".html"), encoding = encoding)

  # grab title
  my_title <- my_contents[grep("<title>", my_contents)]
  my_title <- gsub("<title>|</title>", "", my_title)

  # grab meta tag and h1 tag and delete
  my_contents <- my_contents[-grep("</h1>", my_contents)]

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
    fbody$tag <- tag
  }
  POST(base_url, body = fbody, encode = "form")
}

