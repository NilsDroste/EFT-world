#################################################################################
# analysis of IUCN category PA in 2016 
setwd("/home/droste/Dropbox/Dokumente/doctorate/fiscal transfers/EFT-world/data")
#################################################################################

# 00 - preps -------------------------------------------------------

# load socio-economic control data from World Development Indicators (World Bank)
require(WDI)
require(dplyr)
require(tidyr)


# 01 - read in  data ----------------------------------------------------------

PA_DF <- read.csv("raw/countries2.csv", encoding = "utf-8") #country2 is the more precise arcpy calculation

PA_DF <- PA_DF[,c(6,4,20,27,29:30,36,70:76)] # get rid of (for now) unnecessary columns

names(PA_DF)[names(PA_DF) == 'NAME_ISO'] <- 'Country.Name'
PA_DF$Country.Name <- as.character(PA_DF$Country.Name)
PA_DF[PA_DF$Country.Name=="LIBYAN ARAB JAMAHIRIYA",1] <- "LIBYA"
PA_DF[PA_DF$Country.Name=="PALESTINIAN TERRITORY, OCCUPIED",1] <- "PALESTINE, STATE OF"

PA_DF$ID <- tolower(PA_DF$Country.Name)

CBD <- read.csv("CBD.csv")
# Note : rtf = Ratification, acs = Accession, acp = Acceptance, apv = Approval, scs = Succession

# fixing name problems for merge
CBD$Country.Name <- gsub("  ", "", CBD$Country.Name, fixed = TRUE)

# adding greenland
CBD <- rbind(CBD,c(No.="",Country.Name="Greenland",as.vector(CBD[CBD$Country.Name=="Denmark",c(3:6)])))

CBD[CBD$Country.Name=="Bolivia (Plurinational State of)",2] <- "Bolivia"
CBD[CBD$Country.Name=="Cabo Verde",2] <- "Cape Verde"
CBD[CBD$Country.Name=="Côte d'Ivoire",2] <- "Cote d'ivoire"
CBD[CBD$Country.Name=="Democratic People's Republic of Korea",2] <- "Korea, Democratic People's Republic of"
CBD[CBD$Country.Name=="Democratic Republic of the Congo",2] <- "Congo, The Democratic Republic of the"
CBD[CBD$Country.Name=="Gambia (the)",2] <- "Gambia"
CBD[CBD$Country.Name=="Iran (Islamic Republic of)",2] <- "Iran, Islamic Republic of"
CBD[CBD$Country.Name=="Micronesia (Federated States of)",2] <- "Micronesia, Federated States of"
CBD[CBD$Country.Name=="Republic of Moldova",2] <- "Moldova, Republic of"
CBD[CBD$Country.Name=="The former Yugoslav Republic of Macedonia",2] <- "Macedonia, The former Yugoslav Republic of"
CBD[CBD$Country.Name=="State of Palestine",2] <- "Palestine, State of"
CBD[CBD$Country.Name=="Republic of Korea",2] <- "Korea, Republic of"
CBD[CBD$Country.Name=="United Kingdom of Great Britain and Northern Ireland",2] <- "United Kingdom"
CBD[CBD$Country.Name=="United Republic of Tanzania",2] <- "Tanzania, United Republic of"
CBD[CBD$Country.Name=="Venezuela (Bolivarian Republic of)",2] <- "Venezuela"

CBD$ID <- tolower(CBD$Country.Name)

#HDI
all_content_HDI = readLines("HDI.csv", 190) #omitting incomplete last line problem
skip_first = all_content_HDI[-1] #reading in data except first line
HDI = read.csv(textConnection(skip_first), header = TRUE, stringsAsFactors = FALSE) #create data set

#fixing name problems for merge
HDI[HDI$Country== " C\xf4te d'Ivoire",2] <- " Cote d'ivoire"
HDI$Country <- substring(HDI$Country, 2)

# adding greenland
HDI <- rbind(HDI,as.vector(unlist(c("","Greenland",HDI[HDI$Country=="Denmark",c(3:28)]))))
#adding Somalia (not on UNDP page but here: https://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index)
HDI <- rbind(HDI,as.vector(unlist(c("","Somalia",rep("NA",25),0.285)))) #2012 estimate

# fixing rest of name probs
HDI[HDI$Country=="Bolivia (Plurinational State of)",2] <- "Bolivia"
HDI[HDI$Country=="Cabo Verde",2] <- "Cape Verde"
HDI[HDI$Country=="Côte d'Ivoire",2] <- "Cote d'ivoire"
HDI[HDI$Country=="Democratic People's Republic of Korea",2] <- "Korea, Democratic People's Republic of"
HDI[HDI$Country=="Congo (Democratic Republic of the)",2] <- "Congo, The Democratic Republic of the"
HDI[HDI$Country=="Gambia (the)",2] <- "Gambia"
HDI[HDI$Country=="Hong Kong, China (SAR)",2] <- "Hong Kong"
HDI[HDI$Country=="Iran (Islamic Republic of)",2] <- "Iran, Islamic Republic of"
HDI[HDI$Country=="Micronesia (Federated States of)",2] <- "Micronesia, Federated States of"
HDI[HDI$Country=="Moldova (Republic of)",2] <- "Moldova, Republic of"
HDI[HDI$Country=="The former Yugoslav Republic of Macedonia",2] <- "Macedonia, The former Yugoslav Republic of"
HDI[HDI$Country=="Korea (Republic of)",2] <- "Korea, Republic of"
HDI[HDI$Country=="Tanzania (United Republic of)",2] <- "Tanzania, United Republic of"
HDI[HDI$Country=="United Kingdom of Great Britain and Northern Ireland",2] <- "United Kingdom"
HDI[HDI$Country=="United Republic of Tanzania",2] <- "Tanzania, United Republic of"
HDI[HDI$Country=="Venezuela (Bolivarian Republic of)",2] <- "Venezuela"

HDI$ID <- tolower(HDI$Country)

#population
pop <- WDI(country = "all", indicator = "SP.POP.TOTL", start = 2015, end = 2015, extra = FALSE, cache = NULL)
names(pop)[names(pop) == 'iso2c'] <- 'ISO2' #renaming ISO column
pop <- pop[c(48:nrow(pop)),]

#GDP
GDP <- WDI(country = "all", indicator = "NY.GDP.MKTP.PP.KD", start = 2015, end = 2015, extra = FALSE, cache = NULL)
names(GDP)[names(GDP) == 'iso2c'] <- 'ISO2' #renaming ISO column
GDP <- GDP[c(48:nrow(GDP)),]

#tax revenue
TR <- WDI(country = "all", indicator = "GC.TAX.TOTL.GD.ZS", start = 1960, end = 2015, extra = FALSE, cache = NULL)
TR_groups <- TR[c(1:2632),]
TR <- TR[c(2633:nrow(TR)),]
# detach(package:MASS) # detach to avoid confusion with select
TR <- TR %>% 
  select(iso2c,GC.TAX.TOTL.GD.ZS,year) %>% 
  spread(year,GC.TAX.TOTL.GD.ZS) 
TR <- TR %>% 
  mutate(avgTR=rowMeans(TR[,c(2:57)],na.rm = TRUE)) %>% 
  select(iso2c,avgTR)
names(TR)[names(TR) == 'iso2c'] <- 'ISO2'
TR_groups <- TR_groups %>%  # needed for later imputation of unkown values through averages per income group
  select(country,GC.TAX.TOTL.GD.ZS,year) %>% 
  spread(year,GC.TAX.TOTL.GD.ZS) 
TR_groups <- TR_groups %>% 
  mutate(avgTR=rowMeans(TR_groups[,c(2:57)],na.rm = TRUE)) %>% 
  select(country,avgTR)

#government effectivness by WB governance indicators

GE <- read.csv("GE.csv")
GE$GE <- as.numeric(as.character(GE$GE))
GE <- na.omit(GE)
GE$GE <- ((GE$GE-min(GE$GE))/(max(GE$GE)-min(GE$GE))) #normalizing / feature scaling to [0,1]
GE$ISO <- as.character(GE$ISO)
GE[GE$ISO=="ADO",2] <- "AND"
GE[GE$ISO=="ZAR",2] <- "COD"
GE[GE$ISO=="ROM",2] <- "ROU"
GE[GE$ISO=="TMP",2] <- "TLS"

# Nagoya <- read.csv("Nagoya.csv")
# Note : rtf = Ratification, acs = Accession, acp = Acceptance, apv = Approval, scs = Succession

## cites
CITES <- read.csv("raw/cites_listings_2017-06.csv")

CITESspeciescount <- as.data.frame(table(unlist(strsplit(as.character(CITES$All_DistributionISOCodes),","))))
names(CITESspeciescount) <- c("ISO2", "CITES")
CITESspeciescount$ISO2 <- as.character(CITESspeciescount$ISO2)


# 02 - join data ----------------------------------------------------------

# PA and CBD
df <-inner_join(PA_DF,CBD[,c(5,7)], by = 'ID') #subsetting to those parties who have ratified CBD

# edits
names(df)[names(df) == 'Party'] <- 'CBD' #renaming CBD column

# plus HDI
df <- inner_join(df,HDI[,c(28,29)], by = 'ID') #subsetting to those countries who also have a HDI

# edits
names(df)[names(df) == 'X2015'] <- 'HDI' #renaming HDI colum
df$ISO2 <- as.character(df$ISO2)
df[df$Country.Name=="NAMIBIA","ISO2"] <- "NA" # since it is not NA but "NA"

# plus pop
df <- left_join(df, pop, by = 'ISO2')

# edits
df <- df[,-c(which(names(df)=="country"), which(names(df)=="year"))]
names(df)[names(df) == 'SP.POP.TOTL'] <- 'POP' #renaming HDI colum
df$HDI <- as.numeric(df$HDI)

#plus GDP
df <- left_join(df, GDP, by = 'ISO2')
names(df)[names(df) == 'NY.GDP.MKTP.PP.KD'] <- "GDP"

# edits
df <- df[,-c(which(names(df)=="ID"),which(names(df)=="country"),which(names(df)=="year"))]

#missing variables area, pop, GDP (ppp)

df$ISO2[which(df$Country.Name=="SOUTH SUDAN")] <- "SS" 
df$UNREGION1[which(df$Country.Name=="SOUTH SUDAN")] <- "Eastern Africa" 

#TODO: why doesn't (SOUTH) SUDAN have an area?
df$SQKM[which(df$Country.Name=="SUDAN")] <- 1886068 #wikipedia.org
df$SQKM[which(df$Country.Name=="SOUTH SUDAN")] <- 619745 #wikipedia.org

#TODO: why doesn't ERITREA have an population?
df$POP[which(df$Country.Name=="ERITREA")] <- 5869869 #wikipedia
df$POP[which(df$Country.Name=="SOUTH SUDAN")] <- 12340000 #wikipedia

#TODO: add GDP for following countries from CIA World Factbook (https://www.cia.gov/library/publications/the-world-factbook/rankorder/2001rank.html)
# df[which(is.na(df$GDP)),1]
df$GDP[which(df$Country.Name=="ANDORRA")] <- 3327000000 #2015 est
df$GDP[which(df$Country.Name=="CUBA")] <-134200000000 #2015 est
df$GDP[which(df$Country.Name=="ERITREA")] <- 9169000000 #2016 est
df$GDP[which(df$Country.Name=="GREENLAND")] <- 2173000000 #2015 est
df$GDP[which(df$Country.Name=="IRAN, ISLAMIC REPUBLIC OF")] <- 1459000000000 #2016 est
df$GDP[which(df$Country.Name=="LIBYA")] <- 90890000000 #2016 est
df$GDP[which(df$Country.Name=="LIECHTENSTEIN")] <- 4978000000 #2014 est
df$GDP[which(df$Country.Name=="MAURITANIA")] <- 16710000000 #2016 est
df$GDP[which(df$Country.Name=="PAPUA NEW GUINEA")] <- 28020000000 #2016 est
df$GDP[which(df$Country.Name=="SOUTH SUDAN")] <- 20880000000 #2016 est
df$GDP[which(df$Country.Name=="SYRIAN ARAB REPUBLIC")] <- 50280000000 #2015 est
df$GDP[which(df$Country.Name=="VENEZUELA")] <- 468600000000 #2016 est
df$GDP[which(df$Country.Name=="SOMALIA")] <- 4719000000 #2016 est
df$GDP[which(df$Country.Name=="PALESTINE, STATE OF")] <- 12766*10e+6 #2014 est from http://data.un.org/CountryProfile.aspx?crName=State%20of%20Palestine

# plus tax revenue
df <- left_join(df, TR, by = 'ISO2')

# add the missing ones through imputation of an average value of the country's WB income group
# df[which(is.na(df$avgTR)),c(1,7)]

df$WBINCOME <- as.character(df$WBINCOME)

df[df$Country.Name=="ANDORRA","avgTR"] <- TR_groups[TR_groups$country=="High income",2] 
df[df$Country.Name=="BRUNEI DARUSSALAM","avgTR"] <- TR_groups[TR_groups$country=="High income",2]
df[df$Country.Name=="CAMEROON","avgTR"] <- TR_groups[TR_groups$country=="Low income",2]
df[df$Country.Name=="CHAD","avgTR"] <- TR_groups[TR_groups$country=="Low income",2]
df[df$Country.Name=="COMOROS","avgTR"] <- TR_groups[TR_groups$country=="Low income",2]
df[df$Country.Name=="CUBA","avgTR"] <- TR_groups[TR_groups$country=="Lower middle income",2]
df[df$Country.Name=="DJIBOUTI","avgTR"] <- TR_groups[TR_groups$country=="Lower middle income",2]
df[df$Country.Name=="ECUADOR","avgTR"] <- TR_groups[TR_groups$country=="Lower middle income",2]
df[df$Country.Name=="ERITREA","avgTR"] <- TR_groups[TR_groups$country=="Low income",2]
df[df$Country.Name=="GABON","avgTR"] <- TR_groups[TR_groups$country=="Upper middle income",2]
df[df$Country.Name=="GREENLAND","avgTR"] <- TR_groups[TR_groups$country=="High income",2] 
df[df$Country.Name=="GUINEA-BISSAU","avgTR"] <- TR_groups[TR_groups$country=="Low income",2]
df[df$Country.Name=="GUINEA","avgTR"] <- TR_groups[TR_groups$country=="Low income",2]
df[df$Country.Name=="GUYANA","avgTR"] <- TR_groups[TR_groups$country=="Lower middle income",2]
df[df$Country.Name=="HAITI","avgTR"] <- TR_groups[TR_groups$country=="Low income",2]
df[df$Country.Name=="LIBYA","avgTR"] <- TR_groups[TR_groups$country=="Upper middle income",2]
df[df$Country.Name=="LIECHTENSTEIN","avgTR"] <- TR_groups[TR_groups$country=="High income",2] 
df[df$Country.Name=="MAURITANIA","avgTR"] <- TR_groups[TR_groups$country=="Low income",2]
df[df$Country.Name=="MONTENEGRO","avgTR"] <- TR_groups[TR_groups$country=="Lower middle income",2]
df[df$Country.Name=="MYANMAR","avgTR"] <- TR_groups[TR_groups$country=="Low income",2]
df[df$Country.Name=="NIGER","avgTR"] <- TR_groups[TR_groups$country=="Low income",2]
df[df$Country.Name=="PALAU","avgTR"] <- TR_groups[TR_groups$country=="Upper middle income",2]
df[df$Country.Name=="PANAMA","avgTR"] <- TR_groups[TR_groups$country=="Upper middle income",2]
df[df$Country.Name=="SAUDI ARABIA","avgTR"] <- TR_groups[TR_groups$country=="High income",2] 
df[df$Country.Name=="SOUTH SUDAN","avgTR"] <- TR_groups[TR_groups$country=="Low income",2]
df[df$Country.Name=="SOMALIA","avgTR"] <- TR_groups[TR_groups$country=="Low income",2]
df[df$Country.Name=="SUDAN","avgTR"] <- TR_groups[TR_groups$country=="Low income",2]
df[df$Country.Name=="TAJIKISTAN","avgTR"] <- TR_groups[TR_groups$country=="Low income",2]
df[df$Country.Name=="TONGA","avgTR"] <- TR_groups[TR_groups$country=="Lower middle income",2]
df[df$Country.Name=="TURKMENISTAN","avgTR"] <- TR_groups[TR_groups$country=="Lower middle income",2]
df[df$Country.Name=="UZBEKISTAN","avgTR"] <- TR_groups[TR_groups$country=="Low income",2]
df[df$Country.Name=="VENEZUELA","avgTR"] <- TR_groups[TR_groups$country=="Upper middle income",2]

#government effectivness
df <- left_join(df, GE[,c("ISO","GE")], by = 'ISO')

#impute Palastines GE as mean of its WB income group
df$GE[df$ISO=="PSE"] <- mean(df$GE[df$WBINCOME=="Lower middle income"], na.rm = T)

#cites
df <- inner_join(df,CITESspeciescount, "ISO2")

#free up space
rm("GDP","GE","HDI","pop","CITES","CITESspeciescount","PA_DF","CBD","TR","TR_groups","skip_first", "all_content_HDI")

# 03 - indicators and transfers ----------------------

iucn_weights <- c(1,.9,.8,.7,.5,.3,.1) # category weights for IUCN PA categories.

# ecocentric -- only PA indicators:
df$PA_ind <-  rowSums(mapply('*',(df[,c("PAcatIa","PAcatIb","PAcatII","PAcatIII","PAcatIV","PAcatV","PAcatVI")]/100)*df$SQKM,iucn_weights))

df$EFT_eco <- df$PA_ind*(1e+9/sum(df$PA_ind)) # 1 B fund size share for each countr

hist(log(df$EFT_eco))
head(df[with(df,order(-EFT_eco)),c(1,23)],10) #top EFT

# socio-ecological -- PA / HDI
df$SE_ind <-  ((df$PAcatIa/100)*iucn_weights[1]+(df$PAcatIb/100)*iucn_weights[2]+(df$PAcatII/100)*iucn_weights[3]+(df$PAcatIII/100)*iucn_weights[4]+(df$PAcatIV/100)*iucn_weights[5]+(df$PAcatV/100)*iucn_weights[6]+(df$PAcatVI/100)*iucn_weights[7])/df$HDI
# rowSums(mapply('*',df[,c("PAcatIa","PAcatIb","PAcatII","PAcatIII","PAcatIV","PAcatV","PAcatVI")]/100,iucn_weights))*(1-df$HDI) # more efficient not as easy to read, maybe?

df$EFT_soceco <- df$SE_ind*(1e+9/sum(df$SE_ind)) # 1 B fund size share for each country

# hist(log(df$EFT_soceco))
# head(df[with(df,order(-EFT_soceco)),c(1,25)],10) #top EFT

# anthropocentric (PA/HDI)
df$ANTHR_ind <-  (((df$PAcatIa/100)*iucn_weights[1]+(df$PAcatIb/100)*iucn_weights[2]+(df$PAcatII/100)*iucn_weights[3]+(df$PAcatIII/100)*iucn_weights[4]+(df$PAcatIV/100)*iucn_weights[5]+(df$PAcatV/100)*iucn_weights[6]+(df$PAcatVI/100)*iucn_weights[7])/df$HDI)*(df$POP/df$SQKM) #non-normalized

# df$EFT_anthr <-  df$ANTHR_ind*(1e+9/sum(df$ANTHR_ind))
df$EFT_anthr <-  df$ANTHR_ind*(1e+9/sum(df$ANTHR_ind))

hist(log(df$EFT_anthr))
head(df[with(df,order(-EFT_anthr)),c(1,27)],10) #top EFT


# 04 - analyze the incentive -------------------------------------------------

# calculate mean probability to designate PA of different categories

SumOmeanPA <- sum(colMeans(df[,c("PAcatIa","PAcatIb","PAcatII","PAcatIII","PAcatIV","PAcatV","PAcatVI")]))
PAprob <- colMeans(df[,c("PAcatIa","PAcatIb","PAcatII","PAcatIII","PAcatIV","PAcatV","PAcatVI")])/SumOmeanPA

# calculate the increase in EFT as a share of GDP for a 1% increase in PA given mean PA designation probabilities for each country, ceteris paribus

# ecocentric
EFT_inc_gdp <- vector(mode="numeric", length=nrow(df))
EFT_inc_sqkm <- vector(mode="numeric", length=nrow(df))

for (i in 1:nrow(df)){
  oldPA_i <- df[i,c("PAcatIa","PAcatIb","PAcatII","PAcatIII","PAcatIV","PAcatV","PAcatVI")]
  newPA_i <- oldPA_i + (df$SQKM[i]*PAprob)/df$SQKM[i]
  newIND_i <- rowSums(newPA_i/100*df$SQKM[i]*iucn_weights)
  newIND <- df$PA_ind
  newIND[i] <- newIND_i
  newEFT <- newIND*(1e+9/sum(newIND)) 
  newEFT_i <- newEFT[i]
  EFT_inc_gdp[i] <- (newEFT_i-df$EFT_eco[i])/df$GDP[i]*100
  EFT_inc_sqkm[i] <- (newEFT_i-df$EFT_eco[i])/df$SQKM[i]
  #print(paste("oldEFT_",i, " < newEFT_",i,": ", df$EFT_eco[i]<newEFT_i, sep = ""))
}

df$EFT_eco_incent_gdp <- EFT_inc_gdp
df$EFT_eco_incent_sqkm <- EFT_inc_sqkm

# socio-ecological
EFT_inc_gdp <- vector(mode="numeric", length=nrow(df))
EFT_inc_sqkm <- vector(mode="numeric", length=nrow(df))

for (i in 1:nrow(df)){
  oldPA_i <- df[i,c("PAcatIa","PAcatIb","PAcatII","PAcatIII","PAcatIV","PAcatV","PAcatVI")]
  newPA_i <- oldPA_i + (df$SQKM[i]*PAprob)/df$SQKM[i]
  newIND_i <- rowSums(newPA_i/100*iucn_weights)/df$HDI[i]
  newIND <- df$SE_ind
  newIND[i] <- newIND_i
  newEFT <- newIND*(1e+9/sum(newIND)) 
  newEFT_i <- newEFT[i]
  EFT_inc_gdp[i] <- (newEFT_i-df$EFT_soceco[i])/df$GDP[i]*100
  EFT_inc_sqkm[i] <- (newEFT_i-df$EFT_soceco[i])/df$SQKM[i]
  #print(paste("oldEFT_",i, " < newEFT_",i,": ", df$EFT_soceco[i]<newEFT_i, sep = ""))
}

df$EFT_soceco_incent_gdp <- EFT_inc_gdp
df$EFT_soceco_incent_sqkm <- EFT_inc_sqkm

#anthropocentric
EFT_inc_gdp <- vector(mode="numeric", length=nrow(df))
EFT_inc_sqkm <- vector(mode="numeric", length=nrow(df))

for (i in 1:nrow(df)){
  oldPA_i <- df[i,c("PAcatIa","PAcatIb","PAcatII","PAcatIII","PAcatIV","PAcatV","PAcatVI")]
  newPA_i <- oldPA_i + (df$SQKM[i]*PAprob)/df$SQKM[i]
  newIND_i <- (rowSums(newPA_i/100*iucn_weights)/df$HDI[i])*(df$POP[i]/df$SQKM[i])
  newIND <- df$ANTHR_ind
  newIND[i] <- newIND_i
  newEFT <- newIND*(1e+9/sum(newIND)) 
  newEFT_i <- newEFT[i]
  EFT_inc_gdp[i] <- (newEFT_i-df$EFT_anthr[i])/df$GDP[i]*100
  EFT_inc_sqkm[i] <- (newEFT_i-df$EFT_anthr[i])/df$SQKM[i]
  #print(paste("oldEFT_",i, " < newEFT_",i,": ", df$EFT_anthr[i]<newEFT_i, sep = ""))
}

df$EFT_anthr_incent_gdp <- EFT_inc_gdp
df$EFT_anthr_incent_sqkm <- EFT_inc_sqkm

# Caculating the marginal incentive
df$EFT_eco_incent_abs <- df$EFT_eco_incent_gdp*df$GDP
df$EFT_soceco_incent_abs <- df$EFT_soceco_incent_gdp*df$GDP
df$EFT_anthr_incent_abs <- df$EFT_anthr_incent_gdp*df$GDP

# #top incentives ECO_abs
# head(df[with(df,order(-EFT_eco_incent_abs)),c("Country.Name","EFT_eco_incent_abs")],20)
# #top incentives SOCECO_abs
# head(df[with(df,order(-EFT_soceco_incent_abs)),c("Country.Name","EFT_soceco_incent_abs")],20)
# #top incentives ANTHR_abs
# head(df[with(df,order(-EFT_anthr_incent_abs)),c("Country.Name","EFT_anthr_incent_abs")],20)
# 
# #top incentives ECO_gdp
# head(df[with(df,order(-EFT_eco_incent_gdp)),c("Country.Name","EFT_eco_incent_gdp")],20)
# #top incentives SOCECO_gdp
# head(df[with(df,order(-EFT_soceco_incent_gdp)),c("Country.Name","EFT_soceco_incent_gdp")],20)
# #top incentives ANTHR_gdp
# head(df[with(df,order(-EFT_anthr_incent_gdp)),c("Country.Name","EFT_anthr_incent_gdp")],20)


# 05 - plotting -------------------

require(rworldmap)
require(sp)
sPDF <- joinCountryData2Map( df, joinCode = "ISO3", nameJoinColumn = "ISO")

# reproject to Robinson
sPDF <- spTransform(sPDF, CRS=CRS("+proj=robin +ellps=WGS84"))

## TODO: NOTE THAT THE SCALE OF THE LEGEND IS IN QUANTILES; BUT QUANTILES ARE NOT DISPLAYED ACCORDING TO DATA BUT EQUAL ON PAGE. ## 


# per GDP

op <- par(mfrow = c(3,3),
          mar = c(1,0.5,1,0.5) + 0.1)

#ecocentric
map1 <- mapCountryData( sPDF, nameColumnToPlot="EFT_eco", addLegend='FALSE', numCats =10, catMethod = "quantiles", colourPalette="terrain", mapTitle = "EFT flows in $")
do.call(addMapLegend, c(map1, legendMar = 4, labelFontSize=.75, legendIntervals="page"))
text(0,"ecocentric design", adj=c(.5,-20), srt=90, cex = 1.25, font=2)
map2 <- mapCountryData( sPDF, nameColumnToPlot="EFT_eco_incent_abs", addLegend = F, numCats =10, catMethod = "quantiles", colourPalette="terrain", mapTitle = "marginal incentives in $")
do.call(addMapLegend, c(map2, legendMar = 4, labelFontSize=.75, legendIntervals="page"))
map3 <- mapCountryData( sPDF, nameColumnToPlot="EFT_eco_incent_gdp", addLegend = F, numCats =10, catMethod = "quantiles", colourPalette="terrain", mapTitle = " incentives in % of GDP")
do.call(addMapLegend, c(map3, legendMar = 4, labelFontSize=.75, legendIntervals="page"))

# #socio-ecol
map1 <- mapCountryData( sPDF, nameColumnToPlot="EFT_soceco", addLegend = F, numCats =10, catMethod = "quantiles", colourPalette="terrain",  mapTitle = "")
do.call(addMapLegend, c(map1, legendMar = 4, labelFontSize=.75, legendIntervals="page"))
text(0,"socio-ecological design", adj=c(.5,-20), srt=90, cex = 1.25, font=2)
map2 <- mapCountryData( sPDF, nameColumnToPlot="EFT_soceco_incent_abs", addLegend = F, numCats =10, catMethod = "quantiles", colourPalette="terrain", mapTitle = "")
do.call(addMapLegend, c(map2, legendMar = 4, labelFontSize=.75, legendIntervals="page"))
map3 <- mapCountryData( sPDF, nameColumnToPlot="EFT_soceco_incent_gdp", addLegend = F, numCats =10, catMethod = "quantiles", colourPalette="terrain", mapTitle = "")
do.call(addMapLegend, c(map3, legendMar = 4, labelFontSize=.75, legendIntervals="page"))

#anthroprocentric
map1 <- mapCountryData( sPDF, nameColumnToPlot="EFT_anthr", addLegend = F, numCats =10, catMethod = "quantiles", colourPalette="terrain",  mapTitle = "")
do.call(addMapLegend, c(map1, legendMar = 4, labelFontSize=.75, legendIntervals="page"))
text(0,"anthropocentric design", adj=c(.5,-20), srt=90, cex = 1.25, font=2)
map2 <- mapCountryData( sPDF, nameColumnToPlot="EFT_anthr_incent_abs", addLegend = F, numCats =10, catMethod = "quantiles", colourPalette="terrain", mapTitle = "")
do.call(addMapLegend, c(map2, legendMar = 4, labelFontSize=.75, legendIntervals="page"))
map3 <- mapCountryData( sPDF, nameColumnToPlot="EFT_anthr_incent_gdp", addLegend = F, numCats =10, catMethod = "quantiles", colourPalette="terrain", mapTitle = "")
do.call(addMapLegend, c(map3, legendMar = 4, labelFontSize=.75, legendIntervals="page"))

dev.print(file= "All_EFT_new.png", device=png, width=4200, height=3000, res=300)
dev.off()


# 06 - Aichi Target 11 - 17% per cent PA coverage -----------------------

df$PAgap <- 17 - rowSums(df[,c("PAcatIa","PAcatIb","PAcatII","PAcatIII","PAcatIV","PAcatV","PAcatVI")])

# head(df[with(df,order(-PAgap)),c(1,37)],10)

# hist(df$PAgap,100)

qnt <- quantile(df$PAgap,seq(0,1,.25))
df$PAgap_groups <- cut(df$PAgap,unique(qnt),include.lowest=TRUE)
library(plyr)
df$PAgap_groups <- mapvalues(df$PAgap_groups, from = c("[-34.8,1.14]", "(1.14,8.91]",  "(8.91,15.1]",  "(15.1,17]"), to = c("no gap", "low", "med", "high"))

sPDF <- joinCountryData2Map( df, joinCode = "ISO3", nameJoinColumn = "ISO")

# reproject to Robinson
sPDF <- spTransform(sPDF, CRS=CRS("+proj=robin +ellps=WGS84"))
colourPalette <- RColorBrewer::brewer.pal(4,"YlOrRd")
par(mar=c(8,0,3,0),cex.main=0.65)
map1 <- mapCountryData( sPDF, nameColumnToPlot="PAgap_groups",  numCats =4, addLegend = F, catMethod='categorical', colourPalette = colourPalette, mapTitle = "Aichi target 11 gap")
do.call(addMapLegendBoxes, c(map1, title="Quartiles", x="bottom", horiz = T, cex=.5, pt.cex = 1))
dev.print(file= "PAgap_cuartiles.png", device=png, width=2200, height=1700, res=300)
dev.off()
#TODO: This map needs to be cut with image processing tools in order to crop white spaces, it is a HACK to place the legend well

# 07 - evaluation of design options -------------------------------------------------------

library(car)
library(MASS)
library(sandwich)
library(lmtest)
library(RColorBrewer)
library(ggplot2)
library(cowplot)

#absolute marginal incentive
g_eco_abs <- ggplot(df, aes(x=PAgap_groups, y=EFT_eco_incent_abs, fill=PAgap_groups)) + geom_violin() + geom_boxplot(width=.25) + scale_y_continuous(trans="log10", name="marginal incentive in $", limits = c(1e+03, 1e+10)) + scale_fill_brewer(palette="YlOrRd") + labs(title="ecocentric") + guides(fill=FALSE) + theme(axis.title.x = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) 

g_soceco_abs <-ggplot(df, aes(x=PAgap_groups, y=EFT_soceco_incent_abs, fill=PAgap_groups)) + geom_violin() + geom_boxplot(width=.25)+ scale_y_continuous(trans="log10", limits = c(1e+03, 1e+10)) + scale_fill_brewer(palette="YlOrRd") + labs(title="socio-ecological") + guides(fill=FALSE) + theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank(), axis.ticks = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) 

g_anthr_abs <-ggplot(df, aes(x=PAgap_groups, y=EFT_anthr_incent_abs, fill=PAgap_groups)) + geom_violin() + geom_boxplot(width=.25) + scale_y_continuous(trans="log10", limits = c(1e+03, 1e+10)) + scale_fill_brewer(palette="YlOrRd") + labs(title="anthropocentric") + guides(fill=guide_legend(title=NULL)) + theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank(), axis.ticks = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) 
 
# multiplot(g_eco_abs, g_soceco_abs, g_anthr_abs, cols = 3)

# incentives per gdp
g_eco_gdp <- ggplot(df, aes(x=PAgap_groups, y=EFT_eco_incent_gdp, fill=PAgap_groups)) + geom_violin() + geom_boxplot(width=.25) + scale_y_continuous(trans="log10", name="incentive in % of GDP", limits = c(1e-08, 1e-00)) + scale_fill_brewer(palette="YlOrRd") + guides(fill=FALSE) + theme(axis.title.x = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) #+ theme(plot.margin = unit(c(1,2.5,1,0), "cm")) 

g_soceco_gdp <- ggplot(df, aes(x=PAgap_groups, y=EFT_soceco_incent_gdp, fill=PAgap_groups)) + geom_violin() + geom_boxplot(width=.25) + scale_y_continuous(trans="log10",limits =c(1e-08, 1e-00)) + scale_fill_brewer(palette="YlOrRd") + guides(fill=FALSE) + theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank(), axis.ticks = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) 

g_anthr_gdp <- ggplot(df, aes(x=PAgap_groups, y=EFT_anthr_incent_gdp, fill=PAgap_groups)) + geom_violin() + geom_boxplot(width=.25) + scale_y_continuous(trans="log10",limits = c(1e-08, 1e-00)) + scale_fill_brewer(palette="YlOrRd") +  guides(fill=guide_legend(title=NULL)) + theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank()) + theme(plot.title = element_text(hjust = 0.5)) 

theme_set(theme_grey())
legend_abs <- get_legend(g_anthr_abs)
legend_gdp <- get_legend(g_anthr_gdp)

ggdraw() + 
  draw_plot(plot_grid(g_eco_abs, g_soceco_abs, g_anthr_abs + theme(legend.position = 'none'), g_eco_gdp, g_soceco_gdp, g_anthr_gdp + theme(legend.position = 'none'), ncol = 3, nrow=2, align = 'hv'), width = 0.9) +
  draw_plot(legend_abs, x = 0.9, y=.01, width = 0.1)

#multiplot absolut and per GDP incentives
# multiplot(g_eco_abs, g_eco_sqkm, g_soceco_abs, g_soceco_sqkm, g_anthr_abs, g_anthr_sqkm, cols = 3)
dev.print(file= "DesignEvaluation_new.png", device=png, width=2400, height=1700, res=300)
dev.off()

# 08 Data export ---------------------------------------------------------

# write out data 
library(xlsx)
write.xlsx(df, "EFT_world_data.xlsx", sheetName="dataframe")
codebook <- as.data.frame(cbind(names(df), c(
  "Name of the country",
  "ISO code for the country",
  "ISO2 code for the country",
  "Country area in square kilometer",
  "United Nations Regional Groups level 1",
  "United Nations Regional Groups level 2",
  "World Bank income group",
  "IUCN Protected Area category Ia in per cent of area",
  "IUCN Protected Area category Ib in per cent of area",
  "IUCN Protected Area category II in per cent of area",
  "IUCN Protected Area category III in per cent of area",
  "IUCN Protected Area category IV in per cent of area",
  "IUCN Protected Area category V in per cent of area",
  "IUCN Protected Area category VI in per cent of area",
  "CBD status",
  "Human Development Index",
  "Population",
  "GDP in PPP (constant 2005 international $)",
  "average tax rate",
  "government effectivness by World Bank governance indicators",
  "CITES convention species count",
  "Indicator for ecocentric design w_k*PA_i",
  "EFT flows from ecocentric design of a 1 B fund",
  "Indicator for socio-ecological design w_k*PA_i/HDI_i",
  "EFT flows from socio-ecological design of a 1 B fund",
  "Indicator for anthropocentric design w_k*(PA_i/HDI_i)*pop.dens",
  "EFT flows from anthroprocentric design of a 1 B fund",
  "EFT ecocentric design incentives per GDP (in percent)",
  "EFT ecocentric design incentives per square kilometer",
  "EFT socio-ecological design incentives per GDP (in percent)",
  "EFT socio-ecological design incentives per square kilometer",
  "EFT anthropocentric design incentives per GDP (in percent)",
  "EFT anthropocentric design incentives per square kilometer",
  "absolute marginal EFT ecocentric design incentives (in $ of a 1 B fund)",
  "absolute marginal EFT socio-ecological design incentives (in $ of a 1 B fund)",
  "absolute marginal EFT anthropocentric design incentives (in $ of a 1 B fund)",
  "PA gap to Aichi target 11",
  "PA gap to Aichi target 11 in 4 groups"
)))
names(codebook) <- c("Variable Name", "Description")
write.xlsx(codebook, file="EFT-world_data.xlsx", sheetName="codebook", append=TRUE, row.names=FALSE, col.names = T)

