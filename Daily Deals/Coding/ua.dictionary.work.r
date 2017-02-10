## uarecode function ---------------------------------------------------------
uar.dictionary <- function(varstolookat = macro$varstolookat,
                    brand=macro$brand,
                    path=macro$exportpath,
                    dsin = macro$dsin,
                    dicpath = macro$dic,
                    pull.num = macro$pull.num,
                    prev.pull = macro$prev.pull) {

  brand <- c(brand, "nomatch")
  lookup <-  as.data.frame(fread(path, data.table = F, encoding="UTF-8"))

  if (sum(lookup$change == "") > 0) {
      warning('some change values left blank')
       lookup <- lookup[lookup$change != "",]
  }

  splt.data <- strsplit(lookup$change,"[|]")

  lookup.new <- as.data.frame(matrix(NA, nrow = length(unlist(splt.data))))
  lookup.new$value <- rep(lookup$value, unlist(lapply(splt.data, length)))
  lookup.new$change <- uar.strip(unlist(splt.data))

  lookup <- as.data.frame(cbind(as.character(lookup.new$change),as.character(lookup.new$value), rep(pull.num, nrow(lookup.new))))
  names(lookup) <- c("change", "value", "pull.num")

  if (prev.pull == FALSE) {
      dictionary <- as.data.frame(lookup)
  }


  if (prev.pull == TRUE) {
   dictionary <- read.xlsx(macro$dict, sheetName = "Sheet1")
   dictionary <- as.data.frame(rbind(dictionary, as.data.frame(lookup)))
  }

write.xlsx(dictionary, dicpath, row.names = F)
}
