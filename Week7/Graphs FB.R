setwd("~/Dropbox/MIT Analytics/Week7")


install.packages("igraph")
install.packages("rgl")

library(igraph)
library(rgl)
(install.packages("rgl"))
library(ggplot2)
library(ggmap)

edges = read.csv("edges.csv")
users = read.csv("users.csv")

str(edges)
str(users)

(146*2)/59

summary(users)
summary(edges)

table(users$locale)
table(users$school, users$gender)

g = graph.data.frame(edges, FALSE, users)

plot(g, vertex.size=5, vertex.label=NA)
?graph.data.frame
sum(degree(g) >=10) 
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
summary(V(g)$size)

V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label=NA)

V(g)$color = "black"

V(g)$color[V(g)$school == "A"] = "red"

V(g)$color[V(g)$school == "AB"] = "green"

plot(g, vertex.label=NA)

table(users$locale)

V(g)$color = "black"

V(g)$color[V(g)$locale == "A"] = "red"

V(g)$color[V(g)$locale  == "B"] = "green"

plot(g, vertex.label=NA, vertex.shape="sphere")
rglplot(g, vertex.label=NA)
table(users$locale)

?igraph.plotting
