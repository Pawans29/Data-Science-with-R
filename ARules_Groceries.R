### install packages
library("arules")
library("arulesViz")

#### loading data 
data("Groceries")
summary(Groceries)
str(Groceries)
inspect(Groceries)


##get rules
gr_rules<- apriori(Groceries,parameter = list(supp= 0.05, conf= 0.9))  ### found zero rules

## changing the support and confidence to get rules
gr_rules_1<- apriori(Groceries,parameter = list(supp= 0.10, conf= 0.9)) #### zero rules found

## changing the support and confidence to get rules

gr_rules_2<- apriori(Groceries,parameter = list(supp= 0.005, conf= 0.5))  ### 120 rules found

### changing the support and confidence to get more rules

gr_rules_3<- apriori(Groceries,parameter = list(supp= 0.006, conf= 0.6)) #### 8 rules found


### inspecting the first 10 rules
inspect(gr_rules_2[1:10])

##sorting rules by lift
gr_rules_2<- sort(gr_rules_2,by="lift", decreasing = TRUE )
inspect(gr_rules_2[1:10])   #### lift should be above 1 to be a good rule                

### getting reduntant(duplicate) rules
redundant_rules<-is.redundant(gr_rules_2)
summary(redundant_rules)

### removing redundant rules 1 is redundant
gr_rules_2<- gr_rules_2[!redundant_rules]
gr_rules_2

####plotting the rules
plot(gr_rules_3, method = "graph")

### plotting an scatter plot
plot(gr_rules_3, method = "scatterplot")

## plotting a grouped matrix

plot(gr_rules_3, method = "grouped")

## plotting the rules

plot(gr_rules_3, method = "graph", interactive= TRUE)
