attach2Tistory <- function(path, my_blogName, token){
    base_url  <- "https://www.tistory.com/apis/post/attach"
    file <- httr::upload_file(path)
    fbody <- list(access_token = token,
                  blogName= my_blogName,
                  uploadedfile = file)
    httr::POST(base_url, body = fbody)
}
getReplacer <- function(responce){
  result <- xml2::read_xml(responce$content)
  xml2::xml_text(xml2::xml_find_first(result, "//replacer"))
}
# response <- attach2Tistory
# getReplacer(response)
