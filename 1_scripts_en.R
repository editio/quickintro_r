### ------------------------------------------- ###
###         Code for a quick intro in r         ###
### ------------------------------------------- ###

# -- José Luis Losada Palenzuela -- #
# -- 2023 -- #
# -- 1_scripts_en.R -- #

## Basic instructions in R ----

x = 4         # Assign a value with "="

x             # Check the value

y <- 2        # Assign a value with "<-"

y = y + 5     # Add 5 to it.

y

y == x        # Compare x with y "==" (equality) "!=" (inequality)

myVariable <- "Somewhere in La Mancha, in a place whose name I do not care to remember"

myVariable

# Function that counts characters
nchar(myVariable)

# Dataframes

myBiblio = data.frame(
  author = c("Góngora", "Cervantes", "Calderón", "Cervantes"),
  title = c("Polifemo", "Quijote", "Príncipe constante", "Persiles"), 
  year = c(1612, 1605, 1647, 1617),
  label = c("poetry", "novel", "theater", "novel"),
  available = c(TRUE, TRUE, TRUE, FALSE)
)

myBiblio$author = NULL
myBiblio$year
myBiblio$label
myBiblio$available

## Installation and loading of packages ----
# You only need to install them once. 

# install.packages("tidyverse") # Contains ggplot2
# install.packages("tokenizers")
# install.packages("stopwords")

# Once installed, they should be loaded in each session, using library().

library(tidyverse) # for the view() function: shows a table in the source panel (non-editable)

view(myBiblio)

## Create and access the working directory ----

# Create a folder manually (outside of R) or create directly from R with the function: dir.create("~/Documents/example")

# Atention! Absolut paths in Windows. Use forward slash C:/.../.../ instead of backslash C:\...\...\ 

# Set the working directory. 

setwd("~/Documents/quickintro_r/")

# Other options are available in RStudio menus.
# Session > Set Working Directory > to Source File Location

## Encoding ----

# Pay attention to the encoding. It is important that it is UTF-8 (check with Sys.getlocale() or l10n_info()).

# For Mac and Linux OS, setting it may be sufficient: Sys.setlocale(locale="UTF-8"). For Windows, it should be specified each time data is loaded in the corresponding function with: encoding = "UTF-8".

library(tidyverse)
bibliostylo = read.csv("data/stylometry_sample.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

# bibliostylo = read.csv("data/zotero_estilometria.csv", encoding = "UTF-8")

# Informative functions about variables

dim(bibliostylo)
bibliostylo$Item.Type
class(bibliostylo)
colnames(bibliostylo)

## Initial visualizations with ggplot2 ----

# geom_bar(). Visualize the count.

ggplot(bibliostylo) +
  aes(Item.Type) + 
  geom_bar()

# count(). Count it ourselves.

pub_type = dplyr::count(bibliostylo, Item.Type) # Count the observations

# geom_col(), reorder()

ggplot(pub_type) +
  aes(x = reorder(Item.Type, n), y = n, fill = n) +
  geom_col() +
  labs(x = "count", y = "number of pl", title = "Publication Type")

# Years: Publication.Year ----

years = count(bibliostylo, Publication.Year) # Count the observations of the variable

ggplot(years) +
  aes(x = reorder(Publication.Year, n), y = n, fill = n) +
  geom_col() +
  labs(x = "", y = "", title = "Publications") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate the years 45° and adjust them horizontally.

# Authors: Author ----
# Understand the data and its structure: name separator with ;

bibliostylo$Author[21]

authors = separate_rows(bibliostylo, sep = ";", Author)
authors = count(authors, Author)

dim(authors) # How many authors?

# authors[1:6,]  # a_table_made_of[rows, columns]

authors = top_n(authors, 10, n) # Select the top 10 by frequency.

ggplot(authors) +
  aes(x = reorder(Author, n), y = n, fill = n) +
  geom_col() +
  labs(x = "", y = "", title = "Authors") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Labels: Manual.Tags ----
bibliostylo$Manual.Tags

tags = separate_rows(bibliostylo, sep = ";", Manual.Tags) # Separate the observations
tags = data.frame(lapply(tags, str_trim)) # Clean the spaces
tags = count(tags, Manual.Tags, sort = TRUE) # Count the unique observations

tags

# Select a frequency of two or more.
# subset

tags = subset(tags, tags$n > 2)  # Subset of more than 2 tags

ggplot(tags) +
  aes(x = reorder(Manual.Tags, n), y = n, fill = n) +
  geom_col() +
  labs(x = "", y = "", title = "Tags") +
  coord_flip()

## Textual analysis of titles ----

# “Titles are still the best way to go beyond the 1 percent of novels that make up the canon” (Moretti 2009, 1).

#  Moretti, Franco (2009): “Style, Inc. Reflections on Seven Thousand Titles (British Novels, 1740–1850)”, Critical Inquiry, 36, 1, pp. 134-158, <https://doi.org/10.1086/606125>.

bibliostylo$Title

titles = paste(bibliostylo$Title)  # Join all Title observations (character string)

# extract lexical components (tokens) and remove stopwords.

library(stopwords)

stopwords_en = stopwords("en")  # English
stopwords_de = stopwords("de")  # German
stopwords_es = stopwords("es")  # Spanish
stopwords_nl = stopwords("nl")  # Dutch
stopwords_fr = stopwords("fr")  # French
stopwords_pl = stopwords("pl", source = "stopwords-iso")  # Polish
stopwords_la = stopwords("latin", source = "stopwords-iso") # Latin

stopwords_la 

# append(stopwords_es, c("ay", "aun", "si", "asi", "tan", "ser", "oh"))  # manually added

stopwords_la[2:8]  # View from the second to the eighth

library(tokenizers)

list_tokens = tokenize_words(titles, 
                             lowercase = TRUE, 
                             strip_punct = TRUE, 
                             strip_numeric = TRUE, 
                             stopwords = c(stopwords_en, stopwords_de, stopwords_es, stopwords_la, stopwords_nl, stopwords_fr, stopwords_pl))

title_words = data.frame(words = unlist(list_tokens)) # Convert to a table.

title_words = count(title_words, words)  # Count the observations
title_words = top_n(title_words, 20, n)  

# updated_myData <- subset(title_words, words != "em")

ggplot(title_words) +
  aes(x = reorder(words, n), y = n, fill = n) +
  geom_col() +
  labs(x = "", y = "", title = "Words in Titles") +
  coord_flip()

## Textual analysis of abstracts ----

subset_bibliostylo <- bibliostylo[nchar(bibliostylo$Abstract.Note) > 0, ]

abstracts_content <- bibliostylo$Abstract.Note[nchar(bibliostylo$Abstract.Note) > 0]

list_tokens = tokenize_words(abstracts_content, 
                             lowercase = TRUE, 
                             strip_punct = TRUE, 
                             strip_numeric = TRUE, 
                             stopwords = c(stopwords_en, stopwords_de, stopwords_es, stopwords_la, stopwords_nl, stopwords_fr, stopwords_pl))

abstract_words = data.frame(words = unlist(list_tokens)) # Convert to a table.

abstract_words = count(abstract_words, words)  # Count the observations
abstract_words = top_n(abstract_words, 20, n)  

ggplot(abstract_words) +
  aes(x = reorder(words, n), y = n, fill = n) +
  geom_col() +
  labs(x = "", y = "", title = "Words in Abstracts") +
  coord_flip()


