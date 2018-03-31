#set work directory to rouce file pane

suppressPackageStartupMessages
rm(list = ls())

require("reshape2")
require("plyr")
require("ggplot2")
library("ggrepel")
library("scales")
library("data.table")
library("stringr")

saveplot <- function(plotname){ggsave(filename=paste("../output/", as.character(gsub(" ", "_", plotname)),".eps", sep=""), plot = last_plot(), width=7, height=5)}

Gini <- function (x, weights = rep(1, length = length(x))) 
{
  ox <- order(x)
  x <- x[ox]
  weights <- weights[ox]/sum(weights)
  p <- cumsum(weights)
  nu <- cumsum(weights * x)
  n <- length(nu)
  nu <- nu/nu[n]
  round(sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1]), 4)
}
Atkinson <- function (x, weights = rep(1, length = length(x)), gamma=0.7)
{
  pop_share <- weights / sum(weights)
  if(gamma==1){
    round((1 - (exp(sum(pop_share * log(x))) / sum(pop_share * x))), 4)
  }else{
    round((1 - (sum(pop_share * x^(1-gamma))^(1/(1-gamma)) / sum(pop_share * x))), 4)  
  }
}



#historical WDI data: GDP per capita, PPP (constant 2011 international $)
pop = read.xlsx("../data/SSP_v9_clean.xlsx", sheet = "POP_WDI_2015", startRow = 1, colNames = TRUE, skipEmptyRows = TRUE, rowNames = FALSE)
cpc = read.xlsx("../data/SSP_v9_clean.xlsx", sheet = "GDP_pc_WDI_2015", startRow = 1, colNames = TRUE, skipEmptyRows = TRUE, rowNames = FALSE)
year = seq(1990,2014,1)
pop <- pop[complete.cases(cpc),(c(-1))]
cpc <- cpc[complete.cases(cpc),(c(-1))]
gamma = 0.7
I_global_hist = data.frame(year)
I_global_hist$I_global=0
for (i in 1:25) {
  I_global_hist$I_global[i] <- Atkinson(cpc[,i],pop[,i])
  I_global_hist$I_global_one[i] <- Atkinson(cpc[,i],pop[,i], 1)
  I_global_hist$Gini[i] <- Gini(cpc[,i],pop[,i])
}
colnames(I_global_hist) <- c("year", "I(0.7)", "I(1.0)", "Gini")
I_global_hist <- melt(I_global_hist, id="year")




# Model run data
#years of both models: 2015-2300
year = as.data.frame(seq(2015,2299,1))
colnames(year) <- "year"

gamma = 0.7

models = c("RICE", "FUND", "SSP")


for(m in models){
s2 <- read.csv(paste("../output/", m, "_sigmasquared.csv", sep=""), fill = TRUE, header=FALSE)
cpc <- data.frame(read.csv(paste("../output/", m, "_cpc.csv", sep=""), fill = TRUE, header=FALSE))
pop <- data.frame(read.csv(paste("../output/", m, "_pop.csv", sep=""), fill = TRUE, header=FALSE))

I_within_regions = round(1 - exp(-0.5*gamma*s2), 4)

pop_share = pop / rowSums(pop)
I_between_regions = as.data.frame(round((1 - (rowSums(pop_share * cpc^(1-gamma))^(1/(1-gamma)) / rowSums(pop_share * cpc))), 4))
colnames(I_between_regions) <- "I_between_regions"
I_global = as.data.frame(round((1 - (rowSums(pop_share * (cpc*(1-I_within_regions))^(1-gamma))^(1/(1-gamma)) / rowSums(pop_share * cpc))), 4))
colnames(I_global) <- "I_global"
I_within_weighted = as.data.frame(round(rowSums(pop_share * I_within_regions), 4))
I_global_minus_between = I_global - I_between_regions


#full data set
Atkinson <- year
Atkinson$I_global <- I_global$I_global
Atkinson$I_between_regions <- I_between_regions$I_between_regions
Atkinson$model <- m

if(m==models[1]){Atkinson_all_models=Atkinson}else{Atkinson_all_models <-rbind(Atkinson_all_models,Atkinson)}

#CPC
CPC_global_computed = as.data.frame(round(rowSums(pop_share * cpc), 4))
#full data set
CPC_global <- year
CPC_global$CPC_global <- CPC_global_computed[,1]
CPC_global$model <- m

if(m==models[1]){CPC_global_all_models=CPC_global}else{CPC_global_all_models <-rbind(CPC_global_all_models,CPC_global)}

}
#add CPC and EDE
Atkinson_all_models <- merge(Atkinson_all_models, CPC_global_all_models, by=c("year", "model"))
Atkinson_all_models$EDE = Atkinson_all_models$CPC_global * (1-Atkinson_all_models$I_global)



Atkinson_all_models <- melt(Atkinson_all_models,id=c("year", "model"))
#only each five years
Atkinson_all_models <- subset(Atkinson_all_models, (Atkinson_all_models$year %% 10) == 0)
#remove SSP after 2100
Atkinson_all_models <- subset(Atkinson_all_models, (!(Atkinson_all_models$model=="SSP" & Atkinson_all_models$year > 2100)))

#rename some variable names #####change MER to PPP if PPP is used
Atkinson_all_models$model = gsub("SSP", "SSP2", Atkinson_all_models$model)
Atkinson_all_models$variable = gsub("I_global", "country inequality", Atkinson_all_models$variable)
Atkinson_all_models$variable = gsub("I_between_regions", "regional inequality", Atkinson_all_models$variable)
Atkinson_all_models$variable = gsub("CPC_global", "Per-capita Consumption", Atkinson_all_models$variable)
#Until when to plot the graphs?
Atkinson_all_models <- subset(Atkinson_all_models, (Atkinson_all_models$year <= 2100))




###################### PLOTS ######################

#Plot of inequality indices comparison [Figure 1]
ggplot(I_global_hist) + geom_line(aes(year,value, colour=variable), stat="identity") + ylab(NULL) + xlab(NULL) + ylim(0, 0.7) + ggtitle("Inequality indices") + guides(colour=guide_legend(title="")) + theme_bw() + theme(legend.position = "none", legend.box.just='left') + geom_text_repel(data=I_global_hist[c(6,39,69),], aes(x=year,y=value,label=variable, color=variable))
saveplot("figure-1-Atkinson_historical")

#Plot of Atkinson indices [Figure 2]
ggplot(subset(Atkinson_all_models, variable=="country inequality" | variable=="regional inequality"),aes(year,value,colour=model, linetype=variable, shape=model)) + geom_line(stat="identity") + geom_point(stat="identity") + ylab(expression(paste("I(", gamma, ")"))) + ylim(0, 0.6) + ggtitle(expression(paste("Atkinson index (", gamma, "=0.7)"))) + theme_bw() + theme(legend.position = c(0.25, 0.15), legend.direction = "vertical", legend.box = "horizontal",  legend.box.just='left', legend.background = element_rect(fill=alpha('white', 0.0))) + 
scale_linetype_manual("", values=c("solid", "dashed")) + 
scale_colour_discrete("") +
scale_shape_manual("", values=c(1,2,3,4,5)) 
saveplot("figure-2-Atkinson_indices")

#Plot per-capita consumption and c_ede [Figure 3]
ggplot(subset(Atkinson_all_models, variable=="Per-capita Consumption" | variable=="EDE"),aes(year,value,colour=model, linetype=variable, shape=model)) + geom_line(stat="identity") + geom_point(stat="identity") + ylab("US-$(2014)") + ggtitle(expression(paste("Consumption per-capita and EDE (", gamma, "=0.7)"))) + theme_bw() + theme(legend.position = c(0.2, 0.7), legend.box.just='left', legend.background = element_rect(fill=alpha('white', 0.0))) +
scale_colour_discrete("") +
scale_shape_manual("", values=c(1,2,3,4,5)) + 
scale_linetype_manual("", values=c("dashed", "solid"))
saveplot("figure-3-CPC_EDE")
