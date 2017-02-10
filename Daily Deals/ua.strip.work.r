## uarecode function ---------------------------------------------------------
uar.strip<- function(wordstostrip = NULL) {
  data             <- tolower(str_replace_all((wordstostrip), "[^[:alnum:]]", ""))
  Encoding(data) <- "UTF-8"
  return(iconv(data, "UTF-8", "UTF-8",sub='')) ## replace any non UTF-8 by '' 
}