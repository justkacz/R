#install.packages(c("igraph", "networkD3"))
library(igraph)
library(networkD3)

# create a dataset:
data <- data.frame(
  from = c(
    "Dam","Dam","Dam", "Dam",
    "River","River","River", "River","River",
    "Canal", "Canal", 
    "Diversion","Diversion", 
    "Reservoir", "Reservoir","Reservoir",
    "Lake","Lake","Lake", "Lake", 
    "Road","Road","Road",
    "Culvert", "Culvert",
    "Fish", "Fish","Fish",
    "Frog","Frog","Frog",
    "MacroInvertebrates","MacroInvertebrates"
  ),
  to = c(
    "River","Reservoir","Canal","Diversion",
    "Lake","Reservoir","Frog","Fish","MacroInvertebrates",
    "Diversion", "Reservoir",
    "Dam", "River",
    "River","Dam","Fish",
    "Fish","Dam","Frog","MacroInvertebrates",
    "Fish","Dam", "Canal",
    "Road", "Dam",
    "Frog", "River","MacroInvertebrates",
    "Fish", "River", "Lake",
    "River", "Lake"
  )
)

# Plot
(p <- simpleNetwork(data, height = "600px", width = "600px", 
                    fontSize = 12, fontFamily = "cambria",
                    nodeColour = "3192ed", linkColour = "3192ed",
                    opacity = 0.9, zoom = T, charge = -500, 
                    linkDistance = 
                      JS('function(){d3.select("body").style("background-color", "#222A35"); return 50;}')))
p


