#### loading and installing packages
library(arules)
library(arulesViz)

data_movies<- read.csv(file.choose())

### making rules

rules <- apriori(as.matrix(data_movies[, 6:15]),parameter=list(support=0.02, confidence = 0.6,minlen=5))
inspect(rules[1:5])

###sort rules to decreasing
rules<- sort(rules,by="lift", decreasing = TRUE )
inspect(rules[1:5])   #### lift should be above 1 to be a good rule                

###making rules with differnt support amd confidence 
rules_1 <- apriori(as.matrix(data_movies[, 6:15]),parameter=list(support=0.05, confidence = 0.7,minlen=5))
inspect(rules_1[1:5])

### sort rules Ascending
rules_1<- sort(rules_1,by="support", decreasing = FALSE )
inspect(rules_1[1:5])   #### lift should be above 1 to be a good rule                

##making rules with differnt support amd confidence

rules_2 <- apriori(as.matrix(data_movies[, 6:15]),parameter=list(support=0.04, confidence = 0.4,minlen=5))
inspect(rules_1[1:5])

### sort rules decreasing
rules_2<- sort(rules_2,by="support", decreasing = TRUE )
inspect(rules_1[1:5])   #### lift should be above 1 to be a good rule                


### getting reduntant rules
redundant_rules<-is.redundant(rules_1)
summary(redundant_rules)

### removing redundant rules
rules_2<- rules_2[!redundant_rules]
rules_2

####plotting the rules
plot(rules_2, method = "graph")

###plotting the rules scatter plot
plot(rules_2, method = "scatter")

###plotting the rules grouping
plot(rules_2, method ="grouped")

###plotting the rules in interative plot
plot(rules_2, method= "graph", interactive= TRUE)

