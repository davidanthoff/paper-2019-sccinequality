#aggregate SSP data


library(openxlsx)
library(stringr)
require(utils)
library(data.table)
library(reshape2)
library(plyr)


Population = read.xlsx("../data/SSP_v9_clean.xlsx", sheet = "Population", startRow = 1, colNames = TRUE, skipEmptyRows = TRUE, rowNames = FALSE)

GDP = read.xlsx("../data/SSP_v9_clean.xlsx", sheet = "GDP PPP", startRow = 1, colNames = TRUE, skipEmptyRows = TRUE, rowNames = FALSE)

PPP2MER = read.xlsx("../data/SSP_v9_clean.xlsx", sheet = "PPP2MER", startRow = 1, colNames = TRUE, skipEmptyRows = TRUE, rowNames = FALSE)[1:2]

#choose SSP
Population <- subset(Population, SCENARIO=="SSP2_v9_130325")
GDP <- subset(GDP, SCENARIO=="SSP2_v9_130325")

###SSP is GDP not CONSUMPTION!! ==> take 20% savings rate!!
GDP[, 9:26]  <- GDP[, 9:26]*0.8

#create SSP_pop
SSP_pop <- as.data.frame(t(Population[, 9:26]))
SSP_pop <- SSP_pop[rep(1:nrow(SSP_pop),each=5),] 
SSP_pop[90:285, ] <- SSP_pop[90, ]
write.table(SSP_pop, file = "../output/SSP_pop.csv", row.names=FALSE, col.names=FALSE, sep=",")


#create SSP_cpc
#compute per capita GDP (here: original SSP that is PPP)
SSP_cpc <- 1000*as.data.frame(t(GDP[, 9:26]))/as.data.frame(t(Population[, 9:26]))
SSP_cpc <- SSP_cpc[rep(1:nrow(SSP_cpc),each=5),] 
SSP_cpc[90:285, ] <- SSP_cpc[90, ]
write.table(SSP_cpc, file = "../output/SSP_cpc.csv", row.names=FALSE, col.names=FALSE, sep=",")

############# ALTERNATIVE: CPC for SSPs based on MER
GDP_MER <- merge(GDP, PPP2MER, by="REGION")
GDP_MER[, 9:26] <- GDP_MER[, 9:26]*GDP_MER$PPP2MER
SSP_cpc_MER <- 1000*as.data.frame(t(GDP_MER[, 9:26]))/as.data.frame(t(Population[, 9:26]))
SSP_cpc_MER <- SSP_cpc_MER[rep(1:nrow(SSP_cpc_MER),each=5),] 
SSP_cpc_MER[90:285, ] <- SSP_cpc_MER[90, ]
write.table(SSP_cpc_MER, file = "../output/SSP_cpc_MER.csv", row.names=FALSE, col.names=FALSE, sep=",")

#SSP_sigmasquared just set all zero!
SSP_sigmasquared <- SSP_cpc*0
write.table(SSP_sigmasquared, file = "../output/SSP_sigmasquared.csv", row.names=FALSE, col.names=FALSE, sep=",")

#now aggregate across model regions
GDP_long <- melt(GDP[, 3:26], value.name="GDP", id.var=c("REGION", "FUND_region", "RICE_region"), variable.name="year")
Population_long <- melt(Population[, 3:26], value.name="Population", id.var=c("REGION", "FUND_region", "RICE_region"), variable.name="year")
#Full_Data <- merge(GDP_long, Population_long, by=c("year", "REGION", "FUND_Region", "RICE_region"))
Full_Data <- GDP_long
Full_Data$Population <- Population_long$Population
Full_Data$CPC = 1000*Full_Data$GDP/Full_Data$Population
#add MER values
Full_Data <- merge(Full_Data, PPP2MER, by="REGION")
Full_Data$GDPMER = Full_Data$GDP * Full_Data$PPP2MER
Full_Data$CPCMER = 1000*Full_Data$GDPMER/Full_Data$Population



#create fund_sigmasquared.csv => MER  
fund_sigmasquared <- ddply(Full_Data, .(year, FUND_region), summarise, s2=weighted.mean(log(CPCMER)^2, Population, na.rm = TRUE)-weighted.mean(log(CPCMER), Population, na.rm = TRUE)^2)
fund_sigmasquared <- dcast(fund_sigmasquared, FUND_region ~ year, value.var="s2")
write.table(fund_sigmasquared[,4:22], file = "../output/fund_sigmasquared_2010_2100.csv", row.names=FALSE, col.names=FALSE, sep=",")

#create rice_sigmasquared.csv => PPP
rice_sigmasquared <- ddply(Full_Data, .(year, RICE_region), summarise, s2=weighted.mean(log(CPC)^2, Population, na.rm = TRUE)-weighted.mean(log(CPC), Population, na.rm = TRUE)^2)
rice_sigmasquared <- dcast(rice_sigmasquared, RICE_region ~ year, value.var="s2")
write.table(rice_sigmasquared[,4:22], file = "../output/rice_sigmasquared_2010_2100.csv", row.names=FALSE, col.names=FALSE, sep=",")


#now full 300 year yearly data
rice_sigmasquared <- as.data.frame(t(rice_sigmasquared[,5:22]))
rice_sigmasquared <- rice_sigmasquared[rep(1:nrow(rice_sigmasquared),each=5),] 
rice_sigmasquared[90:285, ] <- rice_sigmasquared[90, ]
write.table(rice_sigmasquared, file = "../output/RICE_sigmasquared.csv", row.names=FALSE, col.names=FALSE, sep=",")
fund_sigmasquared <- as.data.frame(t(fund_sigmasquared[,5:22]))
fund_sigmasquared <- fund_sigmasquared[rep(1:nrow(fund_sigmasquared),each=5),] 
fund_sigmasquared[90:285, ] <- fund_sigmasquared[90, ]
write.table(fund_sigmasquared, file = "../output/FUND_sigmasquared.csv", row.names=FALSE, col.names=FALSE, sep=",")
