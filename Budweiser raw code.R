library(tidyverse)
library(maps)
library(mapproj)
library(usmap)
library(class)
library(e1071)
library(caret)
library(ggplot2)
library(plotly)


## Data Cleaning ##

#load brewery data
breweries <- read.csv(file.choose())

#Trim space in front of abbreviation
breweries$abbr = str_trim(breweries$State)
colnames(breweries)[1] = "Brewery_id"

#Generate brewery count by state
brewery_count_by_state = count(breweries,abbr)

#Clean up column name to be more descriptive
colnames(brewery_count_by_state)[2] = "Breweries"

#Merge/select brewery count with statepop (usmap data set) to get fips for plot.
plotbrew = merge(statepop,brewery_count_by_state,by = "abbr") %>% select(fips, Breweries)

# Merge beer data with brewries data
beers <- read.csv(file.choose())

comb_data = full_join(beers,breweries, by="Brewery_id")

tail(comb_data, 6)
head(comb_data, 6)

#write csv for full data table
write.csv(x = comb_data, file = "full_data_raw.csv")

# Address Missing Values in each column
comb_data <-comb_data %>% mutate_all(na_if,"")
beerclean = comb_data %>% filter (!is.na(ABV) & !is.na(IBU) & !is.na(Style))

#Binning data
ibubreaks <- c(0,50,60,100,160)
ibutags <- c("Low", "Mild", "Strong", "Extreme")
beertrain <- beerclean %>% mutate(ibuclass = cut(IBU, breaks = ibubreaks, labels = ibutags))
write.csv(x=beertrain, file = "Beertrain.csv")

#KNN data filter
IPA <- beerclean %>% filter(grepl("IPA",Style))
ALE <- beerclean %>% filter(grepl("Ale", Style))
IPAC <- IPA %>% mutate(Beerclass = "IPA")
ALEC <- ALE %>% mutate(Beerclass = "ALE")
beertrain

#KNN train
set.seed(6)
splitPerc = .75
trainIndices = sample(1:dim(beertrain)[1],round(splitPerc * dim(beertrain)[1]))
train = beertrain[trainIndices,]
test = beertrain[-trainIndices,]
summary(beertrain)

#finding most accurate K value
accs = data.frame(accuracy = numeric(40), k = numeric(40))
for(i in 1:40)
{
  classifications = knn(train[,c(3,4)],test[,c(3,4)],train$Beerclass, prob = TRUE, k = i)
  table(test$Beerclass,classifications)
  CM = confusionMatrix(table(test$Beerclass,classifications))
  accs$accuracy[i] = CM$overall[1]
  accs$k[i] = i
}
p = plot(accs$k,accs$accuracy, type = "l", xlab = "k")
ggplotly(p)



#knn
table(classifications,test$Beerclass)
confusionMatrix(table(classifications,test$Beerclass))



## PLOTS ##

#Merge/select brewery count with statepop (usmap data set) to get fips for plot.
plotbrew = merge(statepop,brewery_count_by_state,by = "abbr") %>% select(fips, Breweries)

#Plot brewery count on the state map.  Includes Alaska, Hawaii and DC
plot_usmap(data=plotbrew, values="Breweries", color = "black") + scale_fill_continuous(name = " Brewery Count", label = scales::comma) + theme(legend.position = "right")

plot_usmap(data=plotbrew, values="Breweries", color = "black") + scale_fill_gradient(name = " Brewery Count", low = "yellow", high = "red") + theme(legend.position = "right")


# Plot median ABV and IBU by state

#The two plots below display the median ABV and IBU 
plotABV <- beerclean %>% group_by(abbr) %>% summarise(medianABV= median(ABV)) %>% arrange(desc(medianABV)) 
plotABV$abbr <- factor(plotABV$abbr, levels = plotABV$abbr)

plotABV %>% ggplot(aes(abbr,medianABV, fill=medianABV))+geom_bar(stat="identity", width=.5)+ coord_cartesian(ylim=c(.03,.07)) + scale_fill_gradient(low = "yellow", high= "red")+ labs(title="Ordered Bar Chart", subtitle="Median ABV by State") + theme(axis.text.x = element_text(angle=65, vjust=.3)) 

plotIBU <- beerclean %>% group_by(abbr) %>% summarise(medianIBU= median(IBU)) %>% arrange(desc(medianIBU)) 
plotIBU$abbr <- factor(plotIBU$abbr, levels = plotIBU$abbr)

plotIBU %>% ggplot(aes(abbr,medianIBU, fill=medianIBU))+geom_bar(stat="identity", width=.5) + scale_fill_gradient(low = "yellow", high= "red")+ labs(title="Ordered Bar Chart", subtitle="Median IBU by State") + theme(axis.text.x = element_text(angle=65, vjust=.3)) 

# The two plots below present the states alphabetically.
beerclean %>% group_by(abbr) %>% summarise(medianABV= median(ABV))%>% ggplot(aes(abbr,medianABV, fill=medianABV))+geom_bar(stat="identity", width=.5)+ coord_cartesian(ylim=c(.03,.07)) + scale_fill_gradient(low = "yellow", high= "red")+ labs(title="Ordered Bar Chart", subtitle="Median ABV by State") + theme(axis.text.x = element_text(angle=65, vjust=.3)) 

beerclean %>% group_by(abbr) %>% summarise(medianIBU= median(IBU)) %>% ggplot(aes(abbr,medianIBU, fill=medianIBU))+geom_bar(stat="identity", width=.5) + scale_fill_gradient(low = "yellow", high= "red")+ labs(title="Ordered Bar Chart", subtitle="Median IBU by State") + theme(axis.text.x = element_text(angle=65, vjust=.3)) 


# Display the state with highest ABV and IBU
beerclean[grep(max(beerclean$ABV),beerclean$ABV),]
beerclean[grep(max(beerclean$IBU),beerclean$IBU),]

#plot of relationship of IBU to ABV
beertrain %>% ggplot(aes(x = ABV, y = IBU, color = Beerclass)) + geom_point()
cor.test(beerclean$ABV, beerclean$IBU)





