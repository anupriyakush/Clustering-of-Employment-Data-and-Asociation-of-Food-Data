install.packages('arules')
library(readxl)

data.food <- read_excel("C:/Users/anupr/Documents/Flex 4/Data Mining 2/case 3/food_4_association.xls",col_names = TRUE)
head(data.food)
df = as.data.frame(data.food)
typeof(df)
library(arules)


df$TransID <- as.factor(df$TransID)

df$`Add CheeseFood`<- as.logical(df$`Add CheeseFood`)
class(df$BeerFood)

df_2 <- data.frame(lapply(df[,-c(1)], function(x) if(is.numeric(x)) { 
  return(as.logical(x))
} else {  
  return(x) 
}
), stringsAsFactors=FALSE)

df_2['TransID'] = df['TransID']
head(df_2)
class(df_2$TransID)
df_2[(is.na(df_2))] <- FALSE
basket <- as(df_2, "transactions")
head(basket)
View(basket)

summary(basket)
itemFrequencyPlot(basket, support = 0.1, cex.names=0.8, col="sky blue")
basket_rules <- apriori(basket,parameter = list(sup = 0.003, conf = 0.9,target="rules"))
summary(basket_rules)
library(gridExtra)
library(grid)

grid.table(inspect((basket_rules)))

grid.table(inspect(subset(basket_rules, size(basket_rules)>2)))
install.packages('arulesViz')
library('arulesViz')
plot(basket_rules)
plot(basket_rules, interactive=TRUE)

plot(head(sort(basket_rules, by="lift"), 10), method = "graph")
plot(basket_rules, method="grouped")


set.seed(12825704)
wss <- sapply(1:14, 
              function(k)
              {kmeans(Europe_emp_sample[2:10], k,iter.max = 15 )$tot.withinss})
plot(1:14, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
