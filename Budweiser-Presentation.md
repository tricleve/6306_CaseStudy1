---
title: "Budweiser Presentation by FoamFingerMakers"
output: 
  html_document: 
    keep_md: yes
---
##  Introduction
Good morning.  
Today we will showcase the answers to the following questions based on the provided data:  
* Where are breweries located in the United States?  
* Where did we land post data clean up?  
* What is the median ABV and IBU in each state?  
* Who has the most alcoholic and most bitter beer?  
* What does the ABV distribution look like in the United States?  
* Is there a relationship between ABV and IBU?  
We will also look into our ability to successfully identify an IPA.  
Finally, we will showcase promising test markets for a new IPA.




## Where are breweries located in the United States?
The graphic below provides a heat map of the breweries in all 50 states and Washington DC.  


```r
#load brewery data

breweries <- read.csv(curl("https://raw.githubusercontent.com/tricleve/6306_CaseStudy1/master/Breweries.csv"))
#Trim space in front of abbreviation
breweries$abbr = str_trim(breweries$State)

#Rename Brewery id column to enable merge with beers.csv
colnames(breweries)[1] = "Brewery_id"

#Generate brewery count by state
brewery_count_by_state = count(breweries,abbr)

#Clean up column name to be more descriptive
colnames(brewery_count_by_state)[2] = "Breweries"

#Merge/select brewery count with statepop (usmap data set) to get fips for plot.
plotbrew = merge(statepop,brewery_count_by_state,by = "abbr") %>% select(fips, Breweries)

#Plot brewery count on the state map.  Includes Alaska, Hawaii and DC
plot_usmap(data=plotbrew, values="Breweries", color = "black") + scale_fill_gradient(name = " Brewery Count", low = "yellow", high = "red") + labs(title="Brewery count by state", subtitle="All 50 states and DC") + theme(legend.position = "right")
```

![](Budweiser-Presentation_files/figure-html/Map Breweries in Each State-1.png)<!-- -->
    
The code above cleans the brewery data, merges the brewery data with the "statepop" data set from the usmap library and finally plots the brewery counts by state as a heat map using the plot_usmap function.  

## Merging beer data with brewery data

```r
beers <- read.csv(curl("https://raw.githubusercontent.com/tricleve/6306_CaseStudy1/master/Beers.csv"))

#Full join to ensure we bring in all data from both sets
comb_data = full_join(beers,breweries, by="Brewery_id")

tail(comb_data, 6)
```

```
##                           Name.x Beer_ID   ABV IBU Brewery_id
## 2405 Rocky Mountain Oyster Stout    1035 0.075  NA        425
## 2406                   Belgorado     928 0.067  45        425
## 2407               Rail Yard Ale     807 0.052  NA        425
## 2408             B3K Black Lager     620 0.055  NA        425
## 2409         Silverback Pale Ale     145 0.055  40        425
## 2410        Rail Yard Ale (2009)      84 0.052  NA        425
##                         Style Ounces                  Name.y   City State abbr
## 2405           American Stout     12 Wynkoop Brewing Company Denver    CO   CO
## 2406              Belgian IPA     12 Wynkoop Brewing Company Denver    CO   CO
## 2407 American Amber / Red Ale     12 Wynkoop Brewing Company Denver    CO   CO
## 2408              Schwarzbier     12 Wynkoop Brewing Company Denver    CO   CO
## 2409  American Pale Ale (APA)     12 Wynkoop Brewing Company Denver    CO   CO
## 2410 American Amber / Red Ale     12 Wynkoop Brewing Company Denver    CO   CO
```

```r
head(comb_data, 6)
```

```
##                Name.x Beer_ID   ABV IBU Brewery_id
## 1            Pub Beer    1436 0.050  NA        409
## 2         Devil's Cup    2265 0.066  NA        178
## 3 Rise of the Phoenix    2264 0.071  NA        178
## 4            Sinister    2263 0.090  NA        178
## 5       Sex and Candy    2262 0.075  NA        178
## 6        Black Exodus    2261 0.077  NA        178
##                            Style Ounces                    Name.y City State
## 1            American Pale Lager     12 10 Barrel Brewing Company Bend    OR
## 2        American Pale Ale (APA)     12       18th Street Brewery Gary    IN
## 3                   American IPA     12       18th Street Brewery Gary    IN
## 4 American Double / Imperial IPA     12       18th Street Brewery Gary    IN
## 5                   American IPA     12       18th Street Brewery Gary    IN
## 6                  Oatmeal Stout     12       18th Street Brewery Gary    IN
##   abbr
## 1   OR
## 2   IN
## 3   IN
## 4   IN
## 5   IN
## 6   IN
```
  
The code above reads the beers.csv from github and completes a full join of beers.csv and breweries.csv.

## Where did we land post data clean up?
1,007 observations were removed from the data due to missing value in ABV, IBU and Style.  Remaining data is adequate for analysis with 1,403 observations remaining.

```r
sum(is.na(comb_data$ABV))
```

```
## [1] 62
```

```r
sum(is.na(comb_data$IBU))
```

```
## [1] 1005
```

```r
sum(is.na(comb_data$Style))
```

```
## [1] 0
```

```r
beerclean = comb_data %>% filter (!is.na(ABV) & !is.na(IBU) & !is.na(Style))

dim(comb_data)
```

```
## [1] 2410   11
```

```r
dim(beerclean)
```

```
## [1] 1405   11
```
  
The code above identifies the missing values in three fields(ABV, IBU and Style) and then removes them from the combined data set.

## What is the median ABV and IBU in each state?
The graphs below are a bar chart of the median ABV and median IBU for each state. The states are ordered alphabetically. The y axis for ABV has been shifted to make it easier to see differences in ABV between the states.  

```r
## The two plots below present the states alphabetically.
beerclean %>% group_by(abbr) %>% summarise(medianABV= median(ABV))%>% ggplot(aes(abbr,medianABV, fill=medianABV))+geom_bar(stat="identity", width=.5)+ coord_cartesian(ylim=c(.03,.07)) + scale_fill_gradient(low = "yellow", high= "red")+ labs(title="Median ABV by State", x="State", y="Median ABV") + theme(axis.text.x = element_text(angle=65, vjust=.3)) 
```

![](Budweiser-Presentation_files/figure-html/Plot median ABV and IBU by state-1.png)<!-- -->

```r
beerclean %>% group_by(abbr) %>% summarise(medianIBU= median(IBU)) %>% ggplot(aes(abbr,medianIBU, fill=medianIBU))+geom_bar(stat="identity", width=.5) + scale_fill_gradient(low = "yellow", high= "red")+ labs(title="Median IBU by State", x="State", y="Median IBU") + theme(axis.text.x = element_text(angle=65, vjust=.3)) 
```

![](Budweiser-Presentation_files/figure-html/Plot median ABV and IBU by state-2.png)<!-- -->
  
The code above plots the median ABV and IBU by state alphabetically.  The view of the y axis was shifted from 0.0 to 0.03 to provide an easier way to see the differences between states.  The color scheme was chosen to be consistent with all graphs.

## Which state has the most alcoholic and most bitter beer?
Louisville, KY has the most alcoholic beer.
Astoria, OR has the most bitter beer.

```r
beerclean[grep(max(beerclean$ABV),beerclean$ABV),] %>% select(City, abbr, Name.y, Name.x, ABV)
```

```
##          City abbr                    Name.y         Name.x   ABV
## 40 Louisville   KY Against the Grain Brewery London Balling 0.125
```

```r
beerclean[grep(max(beerclean$IBU),beerclean$IBU),] %>% select(City, abbr, Name.y, Name.x, IBU)
```

```
##       City abbr                  Name.y                    Name.x IBU
## 89 Astoria   OR Astoria Brewing Company Bitter Bitch Imperial IPA 138
```
  
The code above finds the row of the highest ABV/IBU and then selects pertinent columns for a clean display.

## What does the ABV distribution look like in the United States?
The distribution of ABV shows a normal distribution with a right skew.

```r
beerclean %>% ggplot(aes(ABV)) + geom_histogram(fill ='red', binwidth = .005) + labs(title="Distribution of ABV")
```

![](Budweiser-Presentation_files/figure-html/Summary and distribution of ABV-1.png)<!-- -->

```r
summarise(beerclean, max=max(ABV), min=min(ABV), mean = mean(ABV), median = median(ABV), sd=sd(ABV))
```

```
##     max   min       mean median         sd
## 1 0.125 0.027 0.05991388  0.057 0.01357633
```
  
The code above plots a histogram of ABV with a specific binwidth.  A dataframe is also printed to show the max, min, mean, median and standard deviation.

## Is there a relationship between ABV and IBU?
There appears to be a positive linear relationship between Bitterness and Alcohol Content.  As ABV increases, IBU increases.

```r
IPA <- beerclean %>% filter(grepl("IPA",Style))
ALE <- beerclean %>% filter(grepl("Ale", Style))
LITE <-beerclean %>% filter(grepl("Pilsner", Style) | grepl("Lager", Style))
IPAC <- IPA %>% mutate(Beerclass = "IPA")
ALEC <- ALE %>% mutate(Beerclass = "ALE")
LITEC <- LITE %>% mutate(Beerclass = "Pilsner/Lager")
beertrain <- rbind(IPAC,ALEC,LITEC)


beertrain %>% ggplot(aes(ABV, IBU)) + geom_jitter(color='red', width=.01) + labs(title="Scatterplot Bitterness vs Alcohol", x="Alcohol Content (ABV)", y="Bitterness (IBU)")
```

![](Budweiser-Presentation_files/figure-html/Scatterplot IBU vs ABV-1.png)<!-- -->
  
The code above performs the initial categorization of styles for our KNN model, but plots all points in a uniform color.  Jitter was chosen over point because it gave a better visual of the observations.

## Can we create a model to identify a beer as an IPA?
We were able to create a model to identfy an IPA with 74% accuracy.  The sensitivity of the model is 76% and the specificity is 88%.

```r
beertrain %>% ggplot(aes(ABV, IBU, color=Beerclass)) + geom_jitter(width=.01) + labs(title="Scatterplot Depicting Beer Style", x="Alcohol Content (ABV)", y="Bitterness (IBU)")
```

![](Budweiser-Presentation_files/figure-html/KNN to classify IPAs, Ales, Pilsner/Lager-1.png)<!-- -->

```r
#KNN train
set.seed(6)
splitPerc = .75
trainIndices = sample(1:dim(beertrain)[1],round(splitPerc * dim(beertrain)[1]))
train = beertrain[trainIndices,]
test = beertrain[-trainIndices,]

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
```

![](Budweiser-Presentation_files/figure-html/KNN to classify IPAs, Ales, Pilsner/Lager-2.png)<!-- -->

```r
#knn
classifications = knn(train[,c(3,4)], test[,c(3,4)], train$Beerclass, prob = TRUE, k = 3)
table(classifications,test$Beerclass)
```

```
##                
## classifications ALE IPA Pilsner/Lager
##   ALE           112  24            15
##   IPA            16  75             3
##   Pilsner/Lager  11   0             8
```

```r
confusionMatrix(table(classifications,test$Beerclass))
```

```
## Confusion Matrix and Statistics
## 
##                
## classifications ALE IPA Pilsner/Lager
##   ALE           112  24            15
##   IPA            16  75             3
##   Pilsner/Lager  11   0             8
## 
## Overall Statistics
##                                           
##                Accuracy : 0.7386          
##                  95% CI : (0.6812, 0.7906)
##     No Information Rate : 0.5265          
##     P-Value [Acc > NIR] : 1.249e-12       
##                                           
##                   Kappa : 0.5318          
##                                           
##  Mcnemar's Test P-Value : 0.1567          
## 
## Statistics by Class:
## 
##                      Class: ALE Class: IPA Class: Pilsner/Lager
## Sensitivity              0.8058     0.7576              0.30769
## Specificity              0.6880     0.8848              0.95378
## Pos Pred Value           0.7417     0.7979              0.42105
## Neg Pred Value           0.7611     0.8588              0.92653
## Prevalence               0.5265     0.3750              0.09848
## Detection Rate           0.4242     0.2841              0.03030
## Detection Prevalence     0.5720     0.3561              0.07197
## Balanced Accuracy        0.7469     0.8212              0.63074
```
  
The code above replots the data set showing the the categories of style( IPA, Ale, Pilsner/Lager).  The code then separates a training and test set, finds the most accurate K value, performs the kNN and prints the confusion matrix.

## What markets are good test areas for a new IPA?
In looking at the per captia consumption of beer by state, we found the top 10 states. These states would be a good test market for a new IPA.  The graphic and table below show these states.

```r
#Source for per capita consumption: Kaplan, Jacob. Apparent Per Capita Alcohol Consumption: National, State, and Regional Trends 1977-2018. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor], 2020-06-29. https://doi.org/10.3886/E105583V4

beerrate <- read.csv(curl("https://raw.githubusercontent.com/tricleve/6306_CaseStudy1/master/apparent_per_capita_alcohol_consumption_1977_2018.csv"))

#filter to look at 2018
BR <- beerrate %>% filter(year == 2018)

#Select State, year and beer
BRR <- BR %>% select(state, year, number_of_beers)


#Column Rename
BRR <-BRR %>% rename(State = state, "Beers_per_person_2018" = "number_of_beers")


#pull state data for mapping
state.abb
```

```
##  [1] "AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "FL" "GA" "HI" "ID" "IL" "IN" "IA"
## [16] "KS" "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" "NJ"
## [31] "NM" "NY" "NC" "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" "VT"
## [46] "VA" "WA" "WV" "WI" "WY"
```

```r
state.name
```

```
##  [1] "Alabama"        "Alaska"         "Arizona"        "Arkansas"      
##  [5] "California"     "Colorado"       "Connecticut"    "Delaware"      
##  [9] "Florida"        "Georgia"        "Hawaii"         "Idaho"         
## [13] "Illinois"       "Indiana"        "Iowa"           "Kansas"        
## [17] "Kentucky"       "Louisiana"      "Maine"          "Maryland"      
## [21] "Massachusetts"  "Michigan"       "Minnesota"      "Mississippi"   
## [25] "Missouri"       "Montana"        "Nebraska"       "Nevada"        
## [29] "New Hampshire"  "New Jersey"     "New Mexico"     "New York"      
## [33] "North Carolina" "North Dakota"   "Ohio"           "Oklahoma"      
## [37] "Oregon"         "Pennsylvania"   "Rhode Island"   "South Carolina"
## [41] "South Dakota"   "Tennessee"      "Texas"          "Utah"          
## [45] "Vermont"        "Virginia"       "Washington"     "West Virginia" 
## [49] "Wisconsin"      "Wyoming"
```

```r
df <- data.frame(state.abb,state.name)
df <- df %>% rename(State = state.name)

#merging state data
df2 <- regex_left_join(df,BRR, by = "State",ignore_case = TRUE)
df3 <- df2[-c(49,5),]
df4 <- df3[,-c(3,4)]
df4 <- df4 %>% rename(state = state.abb, State_Name = State.x)
df4 <- df4[order(-df4$Beers_per_person_2018),]
df5 <- df4[-c(11:50),]

#Top 10 states for per capita beer consumption
df5
```

```
##    state    State_Name Beers_per_person_2018
## 30    NH New Hampshire              424.2963
## 27    MT       Montana              379.2593
## 46    VT       Vermont              362.6667
## 35    ND  North Dakota              355.5556
## 42    SD  South Dakota              324.7407
## 29    NV        Nevada              320.0000
## 20    ME         Maine              317.6296
## 39    PA  Pennsylvania              305.7778
## 51    WI     Wisconsin              305.7778
## 12    HI        Hawaii              303.4074
```

```r
#Plots
plot_usmap(data=df5, values="Beers_per_person_2018", color = "black") + labs(title = "Top Ten Beer Drinking States", legend.position = "center", caption = "Consumption source: https://doi.org/10.3886/E105583V4") + theme(legend.position = "right") + scale_fill_gradient(name = "Beers/person 2018", low = "yellow", high= "red")
```

![](Budweiser-Presentation_files/figure-html/Knock your socks off-1.png)<!-- -->
  
This code grabs per capita consumption by state for 2018, merges it with state data and prints the top 10 states using plot_usmap.  The top 10 states is printed out with the consumption information as well.

## Conclusion
In Summary:  
* High competition states are Colorado, California, Oregon, and Michigan  
* Average of all beers show ABV at 5.5% and IBU > 30  
* Highest ABV was London Balling out of KY  
* Highest Bitterness was Bitter Bitch Imperial IPA out of OR  
* ABV is a normal curve with a right skew centered around 5.5%.  
* Proved Positive correlation between ABV and IBU  
* KNN can accurately classify beers by type for new releases  
* New England region is prime market for new IPA beer with low competition from other breweries  

## Thank You
