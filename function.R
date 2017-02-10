"soundex"<-
  function(x) {
    # 1. extract the last word of surnames and translate
    # to all upper case
    base <- gsub("[^A-Z]", "", toupper(gsub("^.*[ \t]",
                                            "", gsub("[ \t]*$", "", x))))
    # 2. encode the surnames (last word) using the
    # soundex algorithm
    basecode <- gsub("[AEIOUY]", "", gsub("[R]+", "6",
                                          gsub("[MN]+", "5", gsub("[L]+", "4",
                                                                  gsub("[DT]+", "3", gsub("[CGJKQSXZ]+", "2",
                                                                                          gsub("[BFPV]+", "1", gsub("[HW]", "", base))))))))
    # 3. deal with the 1st letter and generate the
    # final coding padded with 0
    sprintf("%4.4s", paste(substring(base, 1, 1),
                           ifelse(regexpr("^[HWAEIOUY]", base) == 1,
                                  basecode, substring(basecode, 2)),
                           "000", sep = ""))
  }


uarecodes <- function(dsin, qlist, searchstring, method) {
  ua.raw	<- bd.coerce(dsin[,qlist])
  
  if(method == "both" | method == "soundslike") {
    massUA	<- function(i,searchstring,ua.raw) { soundex(searchstring)==soundex(ua.raw[i,]) }
    ua.sl	<- if(length(qlist) == 1) as.numeric(soundex(searchstring)==soundex(ua.raw))
    else as.numeric(apply(data.frame(t(sapply(1:nrow(ua.raw),massUA,searchstring=searchstring,ua.raw=ua.raw))),1,max))
  }
  
  if(method == "both" | method == "exact") {
    
    if(length(qlist) == 1) {
      ua.exact <- matrix(0, nrow=length(ua.raw), ncol=length(qlist))
      for(i in 1:length(qlist)) {
        ua.exact <- regexpr(searchstring, ua.raw, ignore.case=T)
      }
      ua.exact.all <- ua.exact>0
    }
    else {
      ua.exact <- matrix(0, nrow=nrow(ua.raw), ncol=length(qlist))
      for(i in 1:length(qlist)) {
        ua.exact[,i] <- regexpr(searchstring, ua.raw[,i], ignore.case=T)
      }
      ua.exact.all <- apply(ua.exact, 1, max) > 0
    }
    
  }
  
  if(method == "both")			return(as.numeric(apply(data.frame(ua.sl, ua.exact.all), 1, max)))
  if(method == "soundslike")	return(as.numeric(ua.sl))
  if(method == "exact")		return(as.numeric(ua.exact.all))
}
