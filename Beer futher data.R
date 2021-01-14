install.packages("fuzzyjoin")
library(fuzzyjoin)

beerrate <- read.csv(file.choose())
beerrate
#filter to look at last two years
BR <- beerrate %>% filter(year == 2018)
BR
#pulling beer and liquor data
BRR <- BR %>% select(state, year, number_of_beers)


#Column Rename
BRR <-BRR %>% rename(State = state, "Beers_per_person_2018" = "number_of_beers")


#pull state data for mapping
state.abb
state.name
df <- data.frame(state.abb,state.name)
df <- df %>% rename(State = state.name)

#merging state data
df2 <- regex_left_join(df,BRR, by = "State",ignore_case = TRUE)
df3 <- df2[-c(49,5),]
df4 <- df3[,-c(3,4)]
df4 <- df4 %>% rename(state = state.abb, State_Name = State.x)
df4 <- df4[order(-df4$Beers_per_person_2018),]
df5 <- df4[-c(11:50),]

#Plots
plot_usmap(data=df5, values="Beers_per_person_2018", color = "black") + labs(title = "Top Ten Beer Drinking States", legend.position = "center") + theme(legend.position = "right") + scale_fill_gradient(name = "Beers/person 2018", low = "yellow", high= "red")
