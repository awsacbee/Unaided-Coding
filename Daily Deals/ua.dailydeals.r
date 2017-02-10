## necessary packages ---------------------------------------------------------
options(stringsAsFactors = FALSE)
library(XLConnect)
library(stringdist)
library(fBasics)
library(stringr)
library(xlsx)
library(base)
library(data.table)

## Paths ----------------------------------------------------------------------
path <- data.path <- 
    "/home/awelden/Data/Unaided Coding/Daily Deals/"
scripts.path <-
    "/home/awelden/Anna Welden/Unaided-Coding/Daily Deals/"

getwd()
setwd(scripts.path)

to.source <- 
    list.files(scripts.path, pattern = '[.]r|[.]R')

to.source <- to.source[grep("work", to.source)]
lapply(to.source, source)


## import raw data ------------------------------------------------------------

dd.raw <- fread(paste(data.path,"GDA Sacramento Data 01.23.2017.csv", sep=""), data.table=F)
names(dd.raw) <- gsub("_", ".", tolower(names(dd.raw)))

# Exclude date and sample/any rows that aren't the ua columns
dd <-  dd.raw
names(dd)

##-----------------------------------------------------------------------------

macro <- list()
macro$dsin <- "dd"
macro$varstolookat <- c("b15r1", "b15r2", "b15r3", "b1r1",  "b1r2",  "b1r3")
macro$contains <- c("etanercept", "enb")
macro$starts <- c("e")
macro$ends <- c(NA)
macro$brand <- c("enbrel", "otezla", "humira", "stelara", "cosentyx", "remicade")
macro$pull.num <- 1
macro$prev.pull <- F
macro$exportpath <- #data.path #
    paste(path,"lookup.ua.", macro$pull.num, ".csv", sep="")
macro$dict <- paste(data.path, "dictionary", ".xlsx", sep="")

##--------------------------------------------------------------------------------

uar.find()
uar.dictionary()
uar.replace()
