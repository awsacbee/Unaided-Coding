
uar.replace <- function(importpath = macro$dict,
                        dsin = macro$dsin,
                        brand = macro$brand,
                        varstolookat = macro$varstolookat){
  dic <- read.xlsx(file = importpath, 1)

  data.temp2 <- (apply(get(dsin)[,varstolookat],2,as.character))
  data.temp2[is.na(data.temp2)] <- 'blanks'
  data.temp2 <- as.data.frame(data.temp2)

  for(i in 1:length(varstolookat)) { #i <-1
    print(i)
    data.temp2[,paste("processed", varstolookat[i], sep=".")] <- uar.strip(data.temp2[,varstolookat[i]])
    for (j in 1:length(brand)){
      print(j) #j<-1
        temp <- as.character(dic[as.character(dic$change) == brand[j],"value"])
      data.temp2[,paste("ua", varstolookat[i],brand[j], sep = ".")]  <- as.numeric(data.temp2[,paste("processed", varstolookat[i], sep = ".")] %in% temp)
    }
  }


  data.temp2 <- as.data.frame(data.temp2)
  varstoadd <- grep("ua", names(data.temp2), value = T)
  #varsnottoadd <- grep("nomatch", varstoadd, value=T)

  uarecoded <- as.data.frame(data.temp2[,varstoadd])
  write.csv(uarecoded, paste(path, "ua.recoded", macro$pull.num, ".csv", sep = ""))

}