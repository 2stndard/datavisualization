install.packages('ggplot2')
install.packages('tidyverse')
install.packages('gt')

library(showtext)
showtext_auto()


start <- as.Date('2021-12-11')
today <- as.Date('2021-12-17')
all_days <- seq(start, today, by = 'day')
year <- as.POSIXlt(all_days)$year + 1900
urls <- paste0('http://cran-logs.rstudio.com/', year, '/', all_days, '.csv.gz')
# only download the files you don't have:
missing_days <- setdiff(as.character(all_days), tools::file_path_sans_ext(dir("CRANlogs"), TRUE))
dir.create("CRANlogs")
for (i in 1:length(missing_days)) {
  print(paste0(i, "/", length(missing_days)))
  download.file(urls[i], paste0('CRANlogs/', missing_days[i], '.csv.gz'))
}


file_list <- list.files("CRANlogs", full.names=TRUE)
logs <- list()
for (file in file_list) {
  print(paste("Reading", file, "..."))
  logs[[file]] <- read.table(file, header = TRUE, sep = ",", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "", as.is=TRUE)
}

# rbind together all files
library(data.table)
dat <- rbindlist(logs)
# add some keys and define variable types
dat[, date:=as.Date(date)]
dat[, package:=factor(package)]
dat[, country:=factor(country)]
dat[, weekday:=weekdays(date)]
dat[, day:=strftime(as.POSIXlt(date),format="%M-%D")]
setkey(dat, package, date, day, country)
save(dat, file="CRANlogs/CRANlogs.RData")
# for later analyses: load the saved data.table
# load("CRANlogs/CRANlogs.RData")

## ======================================================================
library(ggplot2)

library(plyr)
str(dat)
# Overall downloads of packages
d1 <- dat[, length(day), by=package]
d1 <- d1[order(V1), ]
d1[package=="readxl", ]
d1[package=="openxlsx", ]
# plot 1: Compare downloads of selected packages on a weekly basis
agg1 <- dat[J(c("readxl", "openxlsx")), length(unique(ip_id)), by=c("day", "package")]
ggplot(agg1, aes(x=day, y=V1, color=package, group=package)) + geom_line() + ylab("Downloads") + theme_bw() + theme(axis.text.x  = element_text(angle=0, size=8, vjust=0.5)) + 
  scale_x_discrete(labels = c('12월11일', '12월12일', '12월13일', '12월14일', '12월15일', '12월16일'))
agg1 <- dat[J(c("psych", "TripleR", "RSA")), length(unique(ip_id)), by=c("week", "package")]
ggplot(agg1, aes(x=week, y=V1, color=package, group=package)) + geom_line() + ylab("Downloads") + theme_bw() + theme(axis.text.x  = element_text(angle=90, size=8, vjust=0.5))


################################################################################################
library(openxlsx)
library(readxl)

options(scipen=999)  # no scientific number format

nn <- c(1, 10, 100, 1000, 5000, 10000, 20000, 30000)

pp <- c(1, 5, 10, 20, 30, 40, 50)

# create some excel files
l <- list()  # save results
tmp_dir <- tempdir()

for (n in nn) {
  for (p in pp) {
    name <-  
      cat("\n\tn:", n, "p:", p)
    flush.console()
    m <- matrix(rnorm(n*p), n, p)
    file <- paste0(tmp_dir, "/n", n, "_p", p, ".xlsx")
    
    # write
    write.xlsx(m, file)
    # read
    elapsed <- system.time( x <- openxlsx::read.xlsx(file) )["elapsed"]
    df <- data.frame(fun = "openxlsx::read.xlsx", n = n, p = p, 
                     elapsed = elapsed, stringsAsFactors = F, row.names = NULL)
    l <- append(l, list(df))
    
    elapsed <- system.time( x <- readxl::read_xlsx(file) )["elapsed"]
    df <- data.frame(fun = "readxl::read_xlsx", n = n, p = p, 
                     elapsed = elapsed, stringsAsFactors = F, row.names = NULL)
    l <- append(l, list(df))
    
  }
}


# results 
d <- do.call(rbind, l)

library(ggplot2)

ggplot(d, aes(n, elapsed, color= fun)) + 
  geom_line() + geom_point() +  
  facet_wrap( ~ paste("columns:", p)) +
  xlab("Number of rows") +
  ylab("Seconds")



library(openxlsx)

## `read.xlsx()`을 사용하여 엑셀 파일의 데이터를 불러옴
getwd()
df <- read.xlsx(xlsxFile = './chap2/21년 고등 학과별 입학정원 입학 지원 재적 재학 휴학 외국인유학생 졸업 교원_211119.xlsx', 
                sheet = '학과별 주요 현황',  ## 불러오는 데이터가 저장된 sheet는 '학과별 주요현황'
                startRow = 13,   ##시작하는 열은 13번쨰열
                na.string = '-', ## NA값은 '-'로 표기
                colNames = T)    ## 맨 첫줄은 열 이름

class(df.1)


library(tibble)
as_tibble(df)

library(readxl)

## `read_excel()`을 사용하여 엑셀파일을 읽어옴
df <- read_excel('./chap2/주요-06 (유초)지역규모별 개황(1999-2021)_211214y.xlsx', 
                 ## '학과별 주요 현황' 시트의 데이터를 불러오는데,
                 sheet = 'data', 
                 skip = 10, 
                 ## 첫번째 행은 열 이름을 설정
                 col_names = FALSE, 
                 ## 열의 타입을 설정, 처음 8개는 문자형으로 다음 56개는 수치형으로 설정
                 col_types = c(rep('text', 3), rep('numeric', 45)))
View(df.1)
head(df)
getwd()
library(tidyverse)
df <- read.xlsx(xlsxFile = './chap2/주요-06 (유초)지역규모별 개황(1999-2021)_211214y.xlsx', 
                sheet = 'data',  ## 불러오는 데이터가 저장된 sheet는 '학과별 주요현황'
                startRow = 11,   ##시작하는 열은 13번쨰열
                na.string = '-', ## NA값은 '-'로 표기
                colNames = F) ## 맨 첫줄은 열 이름 


glimpse(df)
glimpse(df.1)
