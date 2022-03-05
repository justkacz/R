#library(igraph)
library(networkD3)
library(data.tree)
library(igraph)

dataset<-read.csv("https://raw.githubusercontent.com/justkacz/csvfiles/main/Dane%20NW2.csv", sep=";")
dataset <- dataset %>%
  group_by(from, Category.Name) %>%
  summarise(sum = sum(sales), n = n())

dataset$Category.Name <- paste(dataset$Category.Name, dataset$from)
dataset<-data.frame(dataset)
#dataset

kraj<-unique(dataset$from)
source <- rep("Countries",lengths(list(kraj)))
target<-kraj
source<-append(source, dataset$from)
target<-append(target, dataset$Category.Name)
source<-append(source, dataset$Category.Name)
target<-append(target, dataset$sum)

df <- data.frame(source, target)

nd3 <- ToListExplicit(FromDataFrameNetwork(df), unname = T)


diagonalNetwork(List =nd3, 
                fontSize = 10, 
                #opacity = 0.9, 
                margin = NULL,
                height = 800, width=600,
                nodeStroke = "#3192ed",
                linkColour = "#3192ed",
                textColour = "#3192ed",
                opacity = 
                  JS('function(){d3.select("body").style("background-color", "#222A35"); return 50;}'))
