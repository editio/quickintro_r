### ------------------------------------------- ###
###         Code for a quick intro in r         ###
###                   networks                  ###
### ------------------------------------------- ###

# -- José Luis Losada Palenzuela -- #
# -- 2023 -- #
# -- 2_scripts_en.R -- #

library(tidyverse)
tags_sample = read.csv("data/tags_sample.csv", encoding = "UTF-8", stringsAsFactors=F)

# Separate and reshape the data----

 edgelist <- tags_sample %>%
  separate_rows(Manual.Tags, sep = ";") %>%
  separate_rows(Author, sep = ";") %>%
  separate(Author, into = c("surname", "name"), sep = ",") %>%
  select(surname, Manual.Tags)

## Trim leading and trailing spaces
  
  edgelist <- edgelist %>% mutate_all(str_trim)

# Create a network object----

library(igraph)
graph <- graph.data.frame(edgelist, directed = F)

# Visualize with plot() 1----

# plot(graph)

plot(graph,
     vertex.label = NA)

# Visualize with plot() 2----

## Specify node colors
node_colors <- ifelse(V(graph)$name %in% edgelist$surname, "green", "red")

plot(graph, 
     vertex.color = node_colors)

# Visualize with plot() 3----

deg <- degree(graph)

plot(graph,
  # vertex.label = NA,
  vertex.color = node_colors,
  vertex.size = deg*2)
