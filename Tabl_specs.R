# Tabl_specs.R
# Contains the specifications or metadata for the SEPH tables
# July 30, 2021.

# NOTE: The data for tables are stored and must be updated 
# once per month.

tcol <- "#E3ECF6"
endDate0 <- as.Date("2021-05-01")

t01 <- list(
  Num = 1,
  Bnum = 6,
  Strt = as.Date("2001-01-01"),
  Endt = endDate0,
  STCno = "14-10-0201-01",
  Units = "Persons",
  Titl = "Employment by industry and province or territory",
  Ftnt = "Source: Statistics Canada table 14-10-0201-01.",
  Seas = "Not seasonally adjusted",
  Size = 0
)
t02 <- list(
  Num = 2,
  Bnum = 7,
  Strt = as.Date("2001-01-01"),
  Endt = endDate0,
  STCno = "14-10-0203-01",
  Units = "Dollars",
  Titl = "Average weekly earnings by industry and province or territory",
  Ftnt = "Source: Statistics Canada table 14-10-0203-01.",
  Seas = "Not seasonally adjusted",
  Size = 0
)
t03 <- list(
  Num = 3,
  Bnum = 8,
  Strt = as.Date("2001-01-01"),
  Endt = endDate0,
  STCno = "14-10-0205-01",
  Units = "Dollars",
  Titl = "Average hourly earnings by industry and province or territory",
  Ftnt = "Source: Statistics Canada table 14-10-0205-01.",
  Seas = "Not seasonally adjusted",
  Size = 0
)
t04 <- list(
  Num = 4,
  Bnum = 9,
  Strt = as.Date("2001-01-01"),
  Endt = endDate0,
  STCno = "14-10-0209-01",
  Units = "Dollars",
  Titl = "Average hourly earnings for salaried employees by industry and province or territory",
  Ftnt = "Source: Statistics Canada table 14-10-0209-01.",
  Seas = "Not seasonally adjusted",
  Size = 0
)
t05 <- list(
  Num = 5,
  Bnum = 10,
  Strt = as.Date("2001-01-01"),
  Endt = endDate0,
  STCno = "14-10-0211-01",
  Units = "Hours",
  Titl = "Standard work week for salaried employees by industry and province or territory",
  Ftnt = "Source: Statistics Canada table 14-10-0211-01.",
  Seas = "Not seasonally adjusted",
  Size = 0
)
t06 <- list(
  Num = 6,
  Bnum = 5,
  Strt = as.Date("2001-01-01"),
  Endt = endDate0,
  STCno = "14-10-0213-01",
  Units = "Index, 2002 = 100",
  Titl = "Fixed weighted index of average hourly earnings, by industry and province or territory",
  Ftnt = "Source: Statistics Canada table 14-10-0213-01.",
  Seas = "Not seasonally adjusted",
  Size = 0
)
t07 <- list(
  Num = 7,
  Bnum = 3,
  Strt = as.Date("2001-01-01"),
  Endt = endDate0,
  STCno = "14-10-0220-01",
  Units = "Persons or dollars",
  Titl = "Employment and average weekly earnings (including overtime) by industry",
  Ftnt = "Source: Statistics Canada table 14-10-0220-01.",
  Seas = "Seasonally adjusted",
  Size = 0
)
t08 <- list(
  Num = 8,
  Bnum = 2,
  Strt = as.Date("2001-01-01"),
  Endt = endDate0,
  STCno = "14-10-0221-01",
  Units = "Persons or dollars or hours",
  Titl = "Employment, AHE, AWE and AWH by industry",
  Ftnt = "Source: Statistics Canada table 14-10-0221-01.",
  Seas = "Seasonally adjusted",
  Size = 0
)
t09 <- list(
  Num = 9,
  Bnum = 1,
  Strt = as.Date("2001-01-01"),
  Endt = endDate0,
  STCno = "14-10-0222-01",
  Units = "Persons or dollars or hours",
  Titl = "Employment, AWE, AHE and AWH by province and territory",
  Ftnt = "Source: Statistics Canada table 14-10-0222-01.",
  Seas = "Seasonally adjusted",
  Size = 0
)
t10 <- list(
  Num = 10,
  Bnum = 4,
  Strt = as.Date("2001-01-01"),
  Endt = endDate0,
  STCno = "14-10-0223-01",
  Units = "Persons or dollars",
  Titl = "Employment and average weekly earnings by province and territory",
  Ftnt = "Source: Statistics Canada table 14-10-0223-01.",
  Seas = "Seasonally adjusted",
  Size = 0
)
t11 <- list(
  Num = 11,
  Bnum = 11,
  Strt = as.Date("2001-01-01"),
  Endt = endDate0,
  STCno = "14-10-0255-01",
  Units = "Hours",
  Titl = "Average weekly hours by industry by province and territory",
  Ftnt = "Source: Statistics Canada table 14-10-0255-01.",
  Seas = "Not seasonally adjusted",
  Size = 0
)

TS <- list(t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11)

tord <- c(9,8,7,10,6,1,2,3,4,5,11) # the order of the tables
numTabs <- length(tord) # Number of tables available in the app
tordR <- rep(NA,numTabs)
for (i in 1:numTabs) {
  for (j in 1:numTabs) {
    if (tord[i]==j) tordR[j] <- i
  }
}
tbl <- character()
tabn <- numeric()
for (i in 1:length(tord)) {
  tbl[i] <- TS[[tord[i]]]$Titl # a vector of table names
  tabn[i] <- TS[[tord[i]]]$Num # a vector of table numbers
}
tn <- setNames(tabn,tbl) # a vector of named table numbers

