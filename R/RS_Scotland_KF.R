######################################################
# Revenue Outturn RSX Data per LAD 2010-2018
#####################################################


# Libraries
library(readxl) 
# library(readODS)
library(data.table)
library(stringr)
library(dplyr)

# setwd("c:/ILRR/RS_Indicators")

# Function to convert to alphanumeric, mainly for df column names
convert2alphanumeric <- function(x){
  # remove parenthesis, text and leading spaces such as (e), (d) etc. 
  x <- gsub("\\s*\\([^\\)]+\\)","",as.character(x)) 
  # Remove commas for numeric thousands
  x <- gsub(",", "", x)
  # Remove punctuation
  x <- gsub("[[:punct:]]", " ", x)
  # Remove multiple spaces, trailing spaces {stringr}
  x <- str_squish(x)
  # Replace spaces with underscore
  x <- gsub(" ", "_", x)
  
  return(x)
}

# Function to convert to alphanumeric, mainly for df column names
# alt for including parenthesis for 2011, 2012, 2013, 2014, 2015, 2016, 2017
convert2alphanumeric_alt <- function(x){
  # remove parenthesis, text and leading spaces such as (e), (d) etc. 
  # x <- gsub("\\s*\\([^\\)]+\\)","",as.character(x)) 
  # Remove commas for numeric thousands
  x <- gsub(",", "", x)
  # Remove punctuation
  x <- gsub("[[:punct:]]", " ", x)
  # Remove multiple spaces, trailing spaces {stringr}
  x <- str_squish(x)
  # Replace spaces with underscore
  x <- gsub(" ", "_", x)
  
  return(x)
}


#########################
# READ DATA
#########################

# Keep Scotland for tests if possible

# 2018/19 ###################################################

# Starting df
i <- 2
la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2018-19.xlsx")[i]
rs_scot_gross <- read_excel("data/scotland_spend_ind/RS_SC_2018-19.xlsx", sheet = i, skip = 0, col_names = T, trim_ws = T)
rs_scot_gross <- rs_scot_gross[c(4,29), 2:13]
colnames(rs_scot_gross) <- rs_scot_gross[1,]
rs_scot_gross <- rs_scot_gross[2,]
rs_scot_gross$LA_NAME <- la_name
rs_scot_gross <- rs_scot_gross[, c(ncol(rs_scot_gross), 1:(ncol(rs_scot_gross)-1))]

# Rest
for(i in 3:35) {
  la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2018-19.xlsx")[i]
  print(la_name)
  rs_scot <- read_excel("data/scotland_spend_ind/RS_SC_2018-19.xlsx", sheet = i, skip = 0, col_names = T, trim_ws = T)
  rs_scot <- rs_scot[c(4,29), 2:13]
  colnames(rs_scot) <- rs_scot[1,]
  rs_scot <- rs_scot[2,]
  rs_scot$LA_NAME <- la_name
  rs_scot <- rs_scot[, c(ncol(rs_scot), 1:(ncol(rs_scot)-1))]
  
  # Append
  rs_scot_gross <- rbind(rs_scot_gross, rs_scot)
}

colnames(rs_scot_gross) <- convert2alphanumeric(colnames(rs_scot_gross))
rs_scot_gross$Year <- 2018
# Safekeeping
rs_scot_2018 <- rs_scot_gross


# 2017/18 ###################################################

# Starting df
i <- 1
la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2017-18b.xlsx")[i]
rs_scot_gross <- read_excel("data/scotland_spend_ind/RS_SC_2017-18b.xlsx", sheet = i, skip = 0, col_names = T, trim_ws = T)
rs_scot_varnames <- rs_scot_gross[3, 2:13]

rs_scot_gross <- rs_scot_gross[32, ]
colnames(rs_scot_gross) <- c("LA_NAME", rs_scot_varnames)
rs_scot_gross[1, 1] <- la_name

# Rest
for(i in 2:33) {
  la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2017-18b.xlsx")[i]
  print(la_name)
  rs_scot <- read_excel("data/scotland_spend_ind/RS_SC_2017-18b.xlsx", sheet = i, skip = 0, col_names = T, trim_ws = T)
  rs_scot_varnames <- rs_scot[3, 2:13]
  
  rs_scot <- rs_scot[32, ]
  colnames(rs_scot) <- c("LA_NAME", rs_scot_varnames)
  rs_scot[1, 1] <- la_name

  # Append
  rs_scot_gross <- rbind(rs_scot_gross, rs_scot)
}

colnames(rs_scot_gross) <- convert2alphanumeric_alt(colnames(rs_scot_gross))
rs_scot_gross$Year <- 2017
# Safekeeping
rs_scot_2017 <- rs_scot_gross


# 2016/17 ###################################################

# Starting df
i <- 1
la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2016-17b.xlsx")[i]
rs_scot_gross <- read_excel("data/scotland_spend_ind/RS_SC_2016-17b.xlsx", sheet = i, col_names = T, trim_ws = T)
rs_scot_varnames <- rs_scot_gross[3, 2:13]

rs_scot_gross <- rs_scot_gross[32, ]
colnames(rs_scot_gross) <- c("LA_NAME", rs_scot_varnames)
rs_scot_gross[1, 1] <- la_name

# Rest
for(i in 2:33) {
  la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2016-17b.xlsx")[i]
  print(la_name)
  rs_scot <- read_excel("data/scotland_spend_ind/RS_SC_2016-17b.xlsx", sheet = i, col_names = T, trim_ws = T)
  rs_scot_varnames <- rs_scot[3, 2:13]
  
  rs_scot <- rs_scot[32, ]
  colnames(rs_scot) <- c("LA_NAME", rs_scot_varnames)
  rs_scot[1, 1] <- la_name

  # Append
  rs_scot_gross <- rbind(rs_scot_gross, rs_scot)
}

colnames(rs_scot_gross) <- convert2alphanumeric_alt(colnames(rs_scot_gross))
rs_scot_gross$Year <- 2016
# Safekeeping
rs_scot_2016 <- rs_scot_gross


# 2015/16 ###################################################

# Starting df
i <- 1
la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2015-16b.xlsx")[i]
rs_scot_gross <- read_excel("data/scotland_spend_ind/RS_SC_2015-16b.xlsx", sheet = i, col_names = T, trim_ws = T)
rs_scot_varnames <- as.data.frame(rs_scot_gross[5:17, 1])
rs_scot_varnames <- unlist(rs_scot_varnames)

rs_scot_gross <- data.frame(t(rs_scot_gross[5:17, 2]), row.names = NULL)
rs_scot_gross$LA_NAME <- la_name
rs_scot_gross <- rs_scot_gross[, c(ncol(rs_scot_gross), 1:(ncol(rs_scot_gross)-1))]
colnames(rs_scot_gross) <- c("LA_NAME", rs_scot_varnames)

# Rest
for(i in 2:33) {
  la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2015-16b.xlsx")[i]
  print(la_name)
  rs_scot <- read_excel("data/scotland_spend_ind/RS_SC_2015-16b.xlsx", sheet = i, col_names = T, trim_ws = T)
  rs_scot_varnames <- as.data.frame(rs_scot[5:17, 1])
  rs_scot_varnames <- unlist(rs_scot_varnames)
  
  rs_scot <- data.frame(t(rs_scot[5:17, 2]), row.names = NULL)
  rs_scot$LA_NAME <- la_name
  rs_scot <- rs_scot[, c(ncol(rs_scot), 1:(ncol(rs_scot)-1))]
  colnames(rs_scot) <- c("LA_NAME", rs_scot_varnames)
  
  # Append
  rs_scot_gross <- rbind(rs_scot_gross, rs_scot)
}

colnames(rs_scot_gross) <- convert2alphanumeric_alt(colnames(rs_scot_gross))
rs_scot_gross$Year <- 2015
# Safekeeping
rs_scot_2015 <- rs_scot_gross


# 2014/15 ###################################################

# Starting df
i <- 1
la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2014-15b.xlsx")[i]
rs_scot_gross <- read_excel("data/scotland_spend_ind/RS_SC_2014-15b.xlsx", sheet = i, col_names = T, trim_ws = T)
rs_scot_varnames <- rs_scot_gross[2, 3:14]

rs_scot_gross <- rs_scot_gross[37, 2:14]
colnames(rs_scot_gross) <- c("LA_NAME", rs_scot_varnames)
rs_scot_gross[1, 1] <- la_name

# Rest
for(i in 2:33) {
  la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2014-15b.xlsx")[i]
  print(la_name)
  rs_scot <- read_excel("data/scotland_spend_ind/RS_SC_2014-15b.xlsx", sheet = i, col_names = T, trim_ws = T)
  rs_scot_varnames <- rs_scot[2, 3:14]
  
  rs_scot <- rs_scot[37, 2:14]
  colnames(rs_scot) <- c("LA_NAME", rs_scot_varnames)
  rs_scot[1, 1] <- la_name
  
  # Append
  rs_scot_gross <- rbind(rs_scot_gross, rs_scot)
}

colnames(rs_scot_gross) <- convert2alphanumeric_alt(colnames(rs_scot_gross))
rs_scot_gross$Year <- 2014
# Safekeeping
rs_scot_2014 <- rs_scot_gross


# 2013/14 ###################################################

# Starting df
i <- 1
la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2013-14b.xlsx")[i]
rs_scot_gross <- read_excel("data/scotland_spend_ind/RS_SC_2013-14b.xlsx", sheet = i, col_names = T, trim_ws = T)
rs_scot_varnames <- rs_scot_gross[2, 3:14]

rs_scot_gross <- rs_scot_gross[39, 2:14]
colnames(rs_scot_gross) <- c("LA_NAME", rs_scot_varnames)
rs_scot_gross[1, 1] <- la_name

# Rest
for(i in 2:33) {
  la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2013-14b.xlsx")[i]
  print(la_name)
  rs_scot <- read_excel("data/scotland_spend_ind/RS_SC_2013-14b.xlsx", sheet = i, col_names = T, trim_ws = T)
  rs_scot_varnames <- rs_scot[2, 3:14]
  
  rs_scot <- rs_scot[39, 2:14]
  colnames(rs_scot) <- c("LA_NAME", rs_scot_varnames)
  rs_scot[1, 1] <- la_name
  
  # Append
  rs_scot_gross <- rbind(rs_scot_gross, rs_scot)
}

colnames(rs_scot_gross) <- convert2alphanumeric_alt(colnames(rs_scot_gross))
rs_scot_gross$Year <- 2013
# Safekeeping
rs_scot_2013 <- rs_scot_gross


# 2012/13 ###################################################

# Starting df
i <- 1
la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2012-13b.xlsx")[i]
rs_scot_gross <- read_excel("data/scotland_spend_ind/RS_SC_2012-13b.xlsx", sheet = i, col_names = T, trim_ws = T)
rs_scot_varnames <- rs_scot_gross[2, 3:14]

rs_scot_gross <- rs_scot_gross[39, 2:14]
colnames(rs_scot_gross) <- c("LA_NAME", rs_scot_varnames)
rs_scot_gross[1, 1] <- la_name

# Rest
for(i in 2:33) {
  la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2012-13b.xlsx")[i]
  print(la_name)
  rs_scot <- read_excel("data/scotland_spend_ind/RS_SC_2012-13b.xlsx", sheet = i, col_names = T, trim_ws = T)
  rs_scot_varnames <- rs_scot[2, 3:14]
  
  rs_scot <- rs_scot[39, 2:14]
  colnames(rs_scot) <- c("LA_NAME", rs_scot_varnames)
  rs_scot[1, 1] <- la_name
  
  # Append
  rs_scot_gross <- rbind(rs_scot_gross, rs_scot)
}

colnames(rs_scot_gross) <- convert2alphanumeric_alt(colnames(rs_scot_gross))
rs_scot_gross$Year <- 2012
# Safekeeping
rs_scot_2012 <- rs_scot_gross


# 2011/12 ###################################################

# Starting df
i <- 1
la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2011-12b.xlsx")[i]
rs_scot_gross <- read_excel("data/scotland_spend_ind/RS_SC_2011-12b.xlsx", sheet = i, col_names = T, trim_ws = T)
rs_scot_varnames <- rs_scot_gross[2, 3:14]

rs_scot_gross <- rs_scot_gross[40, 2:14]
colnames(rs_scot_gross) <- c("LA_NAME", rs_scot_varnames)
rs_scot_gross[1, 1] <- la_name

# Rest
for(i in 2:33) {
  la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2011-12b.xlsx")[i]
  print(la_name)
  rs_scot <- read_excel("data/scotland_spend_ind/RS_SC_2011-12b.xlsx", sheet = i, col_names = T, trim_ws = T)
  rs_scot_varnames <- rs_scot[2, 3:14]
  
  rs_scot <- rs_scot[40, 2:14]
  colnames(rs_scot) <- c("LA_NAME", rs_scot_varnames)
  rs_scot[1, 1] <- la_name
  
  # Append
  rs_scot_gross <- rbind(rs_scot_gross, rs_scot)
}

colnames(rs_scot_gross) <- convert2alphanumeric_alt(colnames(rs_scot_gross))
rs_scot_gross$Year <- 2011
# Safekeeping
rs_scot_2011 <- rs_scot_gross


# 2010/11 ###################################################

# Starting df
i <- 1
la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2010-11b.xls")[i]
rs_scot_gross <- read_excel("data/scotland_spend_ind/RS_SC_2010-11b.xls", sheet = i, col_names = T, trim_ws = T)
varnames <- c(4,12,22,35,44,50,68,78,85,95,114,116,118,120,122,124)
rs_scot_varnames <- rs_scot_gross[varnames,1]
rs_scot_varnames <- unlist(rs_scot_varnames)

rs_scot_gross <- data.frame(t(rs_scot_gross[varnames, 9]), row.names = NULL)
rs_scot_gross$LA_NAME <- la_name
rs_scot_gross <- rs_scot_gross[, c(ncol(rs_scot_gross), 1:(ncol(rs_scot_gross)-1))]
colnames(rs_scot_gross) <- c("LA_NAME", rs_scot_varnames)

# Rest
for(i in 2:33) {
  la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2010-11b.xls")[i]
  print(la_name)
  rs_scot <- read_excel("data/scotland_spend_ind/RS_SC_2010-11b.xls", sheet = i, col_names = T, trim_ws = T)
  varnames <- c(4,12,22,35,44,50,68,78,85,95,114,116,118,120,122,124)
  rs_scot_varnames <- rs_scot[varnames,1]
  rs_scot_varnames <- unlist(rs_scot_varnames)
  
  rs_scot <- data.frame(t(rs_scot[varnames, 9]), row.names = NULL)
  rs_scot$LA_NAME <- la_name
  rs_scot <- rs_scot[, c(ncol(rs_scot), 1:(ncol(rs_scot)-1))]
  colnames(rs_scot) <- c("LA_NAME", rs_scot_varnames)
  
  # Append
  rs_scot_gross <- rbind(rs_scot_gross, rs_scot)
}

colnames(rs_scot_gross) <- convert2alphanumeric(colnames(rs_scot_gross))
colnames(rs_scot_gross) <- gsub("[0-9]+", "", colnames(rs_scot_gross))
rs_scot_gross$Year <- 2010
# Safekeeping
rs_scot_2010 <- rs_scot_gross

# 2009/10 ###################################################

# Starting df
i <- 1
la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2009-10.xls")[i]
rs_scot_gross <- read_excel("data/scotland_spend_ind/RS_SC_2009-10.xls", sheet = i, col_names = T, trim_ws = T)
varnames <- c(4,12,22,35,44,50,68,78,85,95,114,116,118,120,122,124)
rs_scot_varnames <- rs_scot_gross[varnames,1]
rs_scot_varnames <- unlist(rs_scot_varnames)

rs_scot_gross <- data.frame(t(rs_scot_gross[varnames, 9]), row.names = NULL) # at this point can change col number to get income
rs_scot_gross$LA_NAME <- la_name
rs_scot_gross <- rs_scot_gross[, c(ncol(rs_scot_gross), 1:(ncol(rs_scot_gross)-1))]
colnames(rs_scot_gross) <- c("LA_NAME", rs_scot_varnames)

# Rest
for(i in 2:33) {
  la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2009-10.xls")[i]
  print(la_name)
  rs_scot <- read_excel("data/scotland_spend_ind/RS_SC_2009-10.xls", sheet = i, col_names = T, trim_ws = T)
  varnames <- c(4,12,22,35,44,50,68,78,85,95,114,116,118,120,122,124)
  rs_scot_varnames <- rs_scot[varnames,1]
  rs_scot_varnames <- unlist(rs_scot_varnames)
  rs_scot_varnames[rs_scot_varnames == "Fire and emergency planning1"] <- "Fire1"
  
  rs_scot <- data.frame(t(rs_scot[varnames, 9]), row.names = NULL)
  rs_scot$LA_NAME <- la_name
  rs_scot <- rs_scot[, c(ncol(rs_scot), 1:(ncol(rs_scot)-1))]
  colnames(rs_scot) <- c("LA_NAME", rs_scot_varnames)
  
  # Append
  rs_scot_gross <- rbind(rs_scot_gross, rs_scot)
}

colnames(rs_scot_gross) <- convert2alphanumeric(colnames(rs_scot_gross))
colnames(rs_scot_gross) <- gsub("[0-9]+", "", colnames(rs_scot_gross))
rs_scot_gross$Year <- 2009
# Safekeeping
rs_scot_2009 <- rs_scot_gross

# 2008/09 ###################################################

# Starting df
i <- 1
la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2008-09.xls")[i]
rs_scot_gross <- read_excel("data/scotland_spend_ind/RS_SC_2008-09.xls", sheet = i, col_names = T, trim_ws = T)
varnames <- c(5,13,23,36,49,68,78,85,95,115,117,119,121,123,125)
rs_scot_varnames <- rs_scot_gross[varnames,1]
rs_scot_varnames <- unlist(rs_scot_varnames)

rs_scot_gross <- data.frame(t(rs_scot_gross[varnames, 9]), row.names = NULL) # at this point can change col number to get income
rs_scot_gross$LA_NAME <- la_name
rs_scot_gross <- rs_scot_gross[, c(ncol(rs_scot_gross), 1:(ncol(rs_scot_gross)-1))]
colnames(rs_scot_gross) <- c("LA_NAME", rs_scot_varnames)

# Rest
for(i in 2:33) {
  la_name <- excel_sheets("data/scotland_spend_ind/RS_SC_2008-09.xls")[i]
  print(la_name)
  rs_scot <- read_excel("data/scotland_spend_ind/RS_SC_2008-09.xls", sheet = i, col_names = T, trim_ws = T)
  varnames <- c(5,13,23,36,49,68,78,85,95,115,117,119,121,123,125)
  rs_scot_varnames <- rs_scot[varnames,1]
  rs_scot_varnames <- unlist(rs_scot_varnames)
  
  rs_scot <- data.frame(t(rs_scot[varnames, 9]), row.names = NULL)
  rs_scot$LA_NAME <- la_name
  
  rs_scot <- rs_scot[, c(ncol(rs_scot), 1:(ncol(rs_scot)-1))]

  z <- as.integer(sapply(rs_scot_varnames, function(y) nchar(y)-2))
  x <- NA
  for(j in 1:15){x[j] <- (substr(rs_scot_varnames[j], 1, z[j]) ==
                            substr(colnames(rs_scot_gross)[j+1], 1, z[j]))}
  if(all(x==T)) colnames(rs_scot) <- colnames(rs_scot_gross)
  
  # Append
  rs_scot_gross <- rbind(rs_scot_gross, rs_scot, make.row.names = TRUE)
}

colnames(rs_scot_gross) <- convert2alphanumeric(colnames(rs_scot_gross))
colnames(rs_scot_gross) <- gsub("[0-9]+", "", colnames(rs_scot_gross))
rs_scot_gross$Year <- 2008
# Safekeeping
rs_scot_2008 <- rs_scot_gross


##################################
# Convert to numeric & other fixes
##################################


for(y in 2007:2018) {
  
  temp <- get(paste0("rs_scot_", y))
  LA_NAME <- temp[, 1]
  temp <- temp[, -1]
  temp <- apply(temp, 2, function(x) as.numeric(as.character(x)))
  temp <- as.data.frame(temp)
  # temp[is.na(temp)] <- 0 - why setting NAs as zero?
  temp <- round(temp, 3)
  
  temp <- cbind(LA_NAME, temp)
  
  assign(paste0("rs_scot_", y), temp)
}

# Baseline
varnames <- colnames(rs_scot_2017)


# 2008
rs_scot_2008$Total_General_Fund_Services <- NA
rs_scot_2008 <- rs_scot_2008[, c(1:4, 6:8, 10, 9, 11, 18, 12, 16, 17)]
colnames(rs_scot_2008) <- varnames

# 2009
rs_scot_2009 <- rs_scot_2009[, -c(5,6,14,15,16)]
rs_scot_2009 <- rs_scot_2009[, c(1:7, 9, 8, 10, 12, 11, 13)]
rs_scot_2009$Total_General_Fund_Services <- NA
rs_scot_2009 <- rs_scot_2009[, c(1:10, 14, 12, 11, 13)]
colnames(rs_scot_2009) <- varnames

# Fix 2010-2012 by removing police & fire (later, responsibility of Scot. Goverment)
rs_scot_2010$Total_General_Fund_Services <- rowSums(rs_scot_2010[, c(2:4, 7:12)])
rs_scot_2010$Total_General_Fund_Services_inc_HRA <- rowSums(rs_scot_2010[, c(13,19)])
rs_scot_2010 <- rs_scot_2010[, c(1:4, 7:9, 11,10,12,19,13,20,18)]
colnames(rs_scot_2010) <- varnames

# 2011
rs_scot_2011$Total_General_Fund_Services <- rowSums(rs_scot_2011[, c(2:4, 7:12)])
rs_scot_2011$Police_Service <- NULL
rs_scot_2011$Fire_Service <- NULL
# no HRA data
rs_scot_2011$HRA_Housing_Services <- NA
rs_scot_2011$Total_General_Fund_Services_inc_HRA <- NA
rs_scot_2011 <- rs_scot_2011[, c(1:11,13:14,12)]
colnames(rs_scot_2011) <- varnames

# 2012
rs_scot_2012$Total_General_Fund_Services <- rowSums(rs_scot_2012[, c(2:4, 7:12)])
rs_scot_2012$Police_Service <- NULL
rs_scot_2012$Fire_Service <- NULL
# no HRA data
rs_scot_2012$HRA_Housing_Services <- NA
rs_scot_2012$Total_General_Fund_Services_inc_HRA <- NA
rs_scot_2012 <- rs_scot_2012[, c(1:11,13:14,12)]
colnames(rs_scot_2012) <- varnames

# 2013
colnames(rs_scot_2013) <- varnames

# 2014
colnames(rs_scot_2014) <- varnames

# 2015
# Road_Bridges is 0 except scotland? remove it from totals
rs_scot_2015$General_Fund[1] <- rs_scot_2015$General_Fund[1] - 1427
rs_scot_2015$Net_Cost_of_Services[1] <- rs_scot_2015$Net_Cost_of_Services[1] - 1427
rs_scot_2015$Road_Bridges <- NULL
colnames(rs_scot_2015) <- varnames

# 2016
colnames(rs_scot_2016) <- varnames

# 2017
colnames(rs_scot_2017) <- varnames

# 2018
colnames(rs_scot_2018) <- varnames


#####################
# MERGE
#####################

rs_scot_2008_2018 <- bind_rows(rs_scot_2008,
                               rs_scot_2009,
                               rs_scot_2010,
                               rs_scot_2011,
                               rs_scot_2012,
                               rs_scot_2013,
                               rs_scot_2014,
                               rs_scot_2015,
                               rs_scot_2016,
                               rs_scot_2017,
                               rs_scot_2018)

rs_scot_2007_2018 <- rs_scot_2007_2018[, c(ncol(rs_scot_2007_2018),
                                           1:(ncol(rs_scot_2007_2018)-1))]
rs_scot_2007_2018 <- rs_scot_2007_2018[order(rs_scot_2007_2018$LA_NAME,
                                             rs_scot_2007_2018$Year), ]

rs_scot_2007_2018$Total_General_Fund_Services <-
  rowSums(rs_scot_2007_2018[, c(3:11)])
rs_scot_2007_2018$Total_General_Fund_Services_inc_HRA <-
  rs_scot_2007_2018$Total_General_Fund_Services +
  rs_scot_2007_2018$HRA_Housing_Services

##############
# Attach codes
##############

# form ons geoportal : Local Authority Districts (December 2018) Names and Codes in the United Kingdom
la_codes <- fread("data/Local_Authority_Districts_December_2018_Names_and_Codes_in_the_United_Kingdom.csv")

rs_scot_2007_2018$LA_NAME <- gsub("&", "and", rs_scot_2007_2018$LA_NAME)

rs_scot_2007_2018 <- merge(rs_scot_2007_2018, la_codes[, 2:3], by.x = "LA_NAME", by.y = "LAD18NM", all.x = T)

# Manually
rs_scot_2007_2018$LAD18CD[rs_scot_2007_2018$LA_NAME == "Edinburgh, City of"] <- "S12000036"
rs_scot_2007_2018$LAD18CD[rs_scot_2007_2018$LA_NAME == "Eilean Siar"] <- "S12000013"
rs_scot_2007_2018$LAD18CD[rs_scot_2007_2018$LA_NAME == "Scotland"] <- "S92000003"

# Corrections
rs_scot_2007_2018$LAD18CD[rs_scot_2007_2018$LA_NAME == "Fife"] <- "S12000015"
rs_scot_2007_2018$LAD18CD[rs_scot_2007_2018$LA_NAME == "Perth and Kinross"] <- "S12000024"


######################
# Pop estimates
######################

# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/population-estimates-time-series-data

y <- 2007 
pop_est <- fread(paste0("Scotland_pop_estimates/mid-year-pop-est-18-time-series-1_", y, ".csv"))
pop_est <- pop_est[5:37, ]
pop_est[, 3:ncol(pop_est)] <- as.data.frame(apply(pop_est[, 3:ncol(pop_est)], 2, function(x) gsub(",", "", x)))
pop_est[, 3:ncol(pop_est)] <- as.data.frame(apply(pop_est[, 3:ncol(pop_est)], 2, function(x) as.numeric(as.character(x))))

pop_est$Pop_Est_Total <- pop_est$V3
pop_est$Pop_Est_Child <- rowSums(pop_est[, c(4:21)])
pop_est$Pop_Est_Adult <- pop_est$Pop_Est_Total - pop_est$Pop_Est_Child

pop_est <- pop_est[, c(1:2, 95:97)]
colnames(pop_est)[1:2] <- c("LAD18CD", "LAD18NM")
pop_est$Year <- y


for(y in 2008:2018) {
  pop <- fread(paste0("Scotland_pop_estimates/mid-year-pop-est-18-time-series-1_", y, ".csv"))
  pop <- pop[5:37, ]
  pop[, 3:ncol(pop)] <- as.data.frame(apply(pop[, 3:ncol(pop)], 2, function(x) gsub(",", "", x)))
  pop[, 3:ncol(pop)] <- as.data.frame(apply(pop[, 3:ncol(pop)], 2, function(x) as.numeric(as.character(x))))
  
  pop$Pop_Est_Total <- pop$V3
  pop$Pop_Est_Child <- rowSums(pop[, c(4:21)])
  pop$Pop_Est_Adult <- pop$Pop_Est_Total - pop$Pop_Est_Child
  
  pop <- pop[, c(1:2, 95:97)]
  colnames(pop)[1:2] <- c("LAD18CD", "LAD18NM")
  pop$Year <- y
  
  pop_est <- rbind(pop_est, pop)
}

pop_est$LAD18CD[pop_est$LAD18NM == "Fife" & pop_est$Year == 2018] <- "S12000015"
pop_est$LAD18CD[pop_est$LAD18NM == "Perth and Kinross" & pop_est$Year == 2018] <- "S12000024"


#################################
# Attach Pop Estimates and CPI
#################################

rs_scot_2007_2018 <- merge(rs_scot_2007_2018, pop_est, by = c("Year", "LAD18CD"), all.x = T)

cpi <- fread("C:/Users/katie/The University of Liverpool/PLDR - Katie_Fahy/raw_data/LA_Lower_to_Upper_to_ONS_Code_Lookup_by_Year_with_Pop_and_CPI.csv")
cpi <- cpi[, c(2,19)]
cpi <- unique(cpi)
rs_scot_2007_2018 <- merge(rs_scot_2007_2018, cpi, by = "Year", all.x = T)
# need cpi for 2018

rs_scot_2007_2018$CPI2017 <- NA

###################
# Export
###################

# Prepare folders
dir.create(file.path(getwd(), "FIN_SCO_2007-2018"), showWarnings = F)

# Wil not export HRA Housing and Total + HRA in this case due to not being complete

# remove 'Councils' (equivalent to 'Scotland' nd only present in 2018)
rs_scot_2007_2018 <- rs_scot_2007_2018[ !(rs_scot_2007_2018$LA_NAME=="Councils"), ]
rs_scot_2007_2018$LA_NAME <- NULL

for(v in 3:12) {
  
  var_name <- colnames(rs_scot_2007_2018)[v]
  
  FIN_SCO <- rs_scot_2007_2018[, c("Year", "LAD18CD", "LAD18NM", 
                             "Pop_Est_Total", "Pop_Est_Child", "Pop_Est_Adult", "CPI2017")]
  FIN_SCO[, 8] <- rs_scot_2007_2018[, v]
  colnames(FIN_SCO)[8] <- var_name
  
  # Per capita values
  FIN_SCO[, 9] <- FIN_SCO[, 8] / FIN_SCO$Pop_Est_Total
  colnames(FIN_SCO)[9] <- paste0(var_name, "_PerCap")
  
  # Sort rows by name and year
  FIN_SCO <- FIN_SCO[order(FIN_SCO$LAD18NM, FIN_SCO$Year), ]
  
  # Export
  write.csv(FIN_SCO, paste0("FIN_SCO_2007-2018/FIN_SCO_", var_name, ".csv"), row.names = F)
  print(paste0(var_name, "....Done"))
}

write.csv(rs_scot_2007_2018, "FIN_SCO_2007-2018/scotland_2007_2018.csv", row.names = F)

# END
