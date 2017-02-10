## uarecode function ---------------------------------------------------------
uar.find<- function(varstolookat = macro$varstolookat,
                    contains=macro$contains,
                    dsin = macro$dsin,
                    starts=macro$starts,
                    ends=macro$ends,
                    brand=macro$brand,
                    path=macro$exportpath,
                    importpath = macro$dic){
### reformat varstolookat
  combine<- NULL
  for (i in 1:length(varstolookat)){
    combine <- c(combine, list(get(dsin)[,varstolookat[i]]))
  }
  varstolookat <- as.character(unlist(combine))
### strip words and format output - if no previous pull
  if (!macro$prev.pull){
    data             <- uar.strip(varstolookat)
    data.raw         <- as.data.frame(unique(sort(data)))
    names(data.raw)  <- "value"
    data.raw$value   <- as.character(data.raw$value)
    data.raw$freq    <- as.numeric(table(sort(data)))

    data.temp        <- data.frame(rep(NA, length(data.raw$value)))
    for(i in 1:length(brand)){ #i <- 1
      data.temp[,i]  <- as.numeric(stringdist(brand[i], (data.raw[,"value"]),
                                              method="lv", maxDist=Inf))
    }
    names(data.temp) <- brand
    data.temp$value  <- data.raw$value
    data.temp[,c(paste(brand, ".brand.dist", sep=""))] <- data.temp[,brand]/nchar(brand)
  }
### strip words and format output - for new pull
  if ( macro$prev.pull ){
    dic <- apply(read.xlsx(file = importpath, 1), 2, as.character)
    dic <- dic[,c("change", "value")]
    data             <- uar.strip (varstolookat)
    data.raw         <- as.data.frame(unique(sort(data)))
    names(data.raw)  <- "value"
    data.raw$freq    <- as.numeric(table(sort(data)))
    data.temp        <- data.frame(rep(NA, length(data.raw$value)))
    findold <- list()
    findold <-  as.character(data.raw$value)[as.character(data.raw$value) %in% dic[,"value"]]
    findnew <-  as.character(data.raw$value)[!(as.character(data.raw$value) %in% dic[,"value"])]

    #data.new <- as.data.frame((data.raw[which(rowMaxs(as.data.frame(findold))==0),]))
    if(length(findnew) == 0){
      stop('dictionaries are the same')
    }

    if(length(findnew)>0){
      value = as.data.frame(findnew)
      freq = as.data.frame(data.raw[match(findnew,as.character(data.raw$value)),"freq"])
      newinfo <- as.data.frame(cbind(value, freq))
      names(newinfo) <- c("value", "freq")

      data.temp        <- as.data.frame(rep(NA, length(newinfo$value)))

      for(i in 1:length(brand)){ #i <- 1
        data.temp[,i]  <- as.numeric(stringdist(brand[i], sort(levels(newinfo$value)),
                                                method="lv", maxDist=Inf))
      }
      names(data.temp) <- brand
      data.temp$value  <- as.data.frame(sort(levels(newinfo$value)))
      data.temp[,c(paste(brand, ".brand.dist", sep=""))] <- data.temp[,brand]/nchar(brand)

      data.raw <- newinfo


    }
  } # end old pull if

  data.raw$min.diff<- rowMins(data.temp[,c(paste(brand, ".brand.dist", sep=""))])

  if(length(brand) > 1 ){
    nearlook<- data.temp[,grep("[.]brand[.]dist", names(data.temp), value=T)]
    data.raw[,"nearest"] <- rep('xxx', nrow(data.raw))
    for(i in which(rowMaxs(nearlook)==rowMins(nearlook))){
      data.raw[i,"nearest"] <- "same distance"
    }
    for(i in which(rowMaxs(nearlook)!= rowMins(nearlook))){
      nearlook1 <- which(nearlook==rowMins(nearlook), arr.ind=T)
      nears <- subset(nearlook1, !duplicated(nearlook1[,1]))
      nearlook2 <- which(nears[,1] %in% i)
      nearlook3 <- (nears[nearlook2,2])
      data.raw[i,"nearest"] <- brand[nearlook3[[1]]]
    }
  }

  if(length(brand) == 1){
    data.raw$nearest <- rep(brand, nrow(data.raw))
  }

  data.raw[,c(paste(brand, ".brand", sep=""))]   <- data.temp[,c(paste(brand, ".brand.dist", sep=""))]
  data.raw[,"change"]                            <- rep("", nrow(data.raw))
  data.raw[,c(paste(brand, ".dist", sep=""))]    <- data.temp[,brand]
  data.raw$min.diff.dist                         <- rowMins(data.temp[,brand])


  if(all(!is.na(contains))){
    for(i in 1:length(contains)){ # i<-2
      if(length(grep(contains[i], as.character(data.raw$value)))==0){
        data.raw[,paste("contains", contains[i], sep=".")]<- 0
      }
      if(length(grep(contains[i], as.character(data.raw$value)))>0){
        data.raw[grep(contains[i], as.character(data.raw$value)),paste("contains", contains[i], sep=".")]<- 1
        data.raw[-(grep(as.character(contains[i]), as.character(data.raw$value))),paste("contains", contains[i], sep=".")]<- 0
      }
    }#ends for contains
  } #ends if !is.na

  if(all(!is.na(starts))){
    for(i in 1:length(starts)){ # i<-1
      if(length((grep(paste("^", starts[i], sep=""), data.raw$value)))==0){
        data.raw[,paste("starts", starts[i], sep=".")]<- 0
      }
      if(length((grep(paste("^", starts[i], sep=""), data.raw$value)))>0){
        data.raw[(grep(paste("^", starts[i], sep=""), data.raw$value)),paste("starts", starts[i], sep=".")]<- 1
        data.raw[-(grep(paste("^", starts[i], sep=""), data.raw$value)),paste("starts", starts[i], sep=".")]<- 0
      }
    }
  }


  if(all(!is.na(ends))){
    for(i in 1:length(ends)){ # i<-1
      if(length(grep(paste(ends[i],"$", sep=""), data.raw$value))==0){
        data.raw[,paste("ends", ends[i], sep=".")]<- 0
      }
      if(length(grep(paste(ends[i],"$", sep=""), data.raw$value))>0){
        data.raw[(grep(paste(ends[i],"$", sep=""), data.raw$value)),paste("ends", ends[i], sep=".")]<- 1
        data.raw[-(grep(paste(ends[i],"$", sep=""), data.raw$value)),paste("ends", ends[i], sep=".")]<- 0
      }
    }
  }

  write.csv(data.raw, macro$exportpath)

}