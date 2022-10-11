# TUTORIAL - EFFICIENT DATA MANIPULATION
# Daniel Collier
# 11/10/2022


# LIBRARIES
library(dplyr)     # for data manipulation
library(ggplot2)   # for making graphs; make sure you have it installed, or install it now


# 1. INTRODUCTION TO PIPES %>% ----

# LOAD DATA
trees <- read.csv(file = "trees.csv", header = TRUE)

head(trees)  # make sure the data imported OK, familiarise yourself with the variables



# If we want to know how many trees of each species are found in the dataset:

# Count the number of trees for each species

trees.grouped <- group_by(trees, CommonName)    # create an internal grouping structure, so that the next function acts on groups (here, species) separately.

trees.summary <- summarise(trees.grouped, count = length(CommonName))   # here we use length to count the number of rows (trees) for each group (species). We could have used any row name.

# Alternatively, dplyr has a tally function that does the counts for you!
trees.summary2 <- tally(trees.grouped)


# For the above to work we had to create a new object to summarise from.
# With pipes you don't need to do this:



# Count the number of trees for each species, with a pipe!

trees.summary3 <- trees %>%                   # the data frame object that will be passed in the pipe
  group_by(CommonName) %>%    # see how we don't need to name the object, just the grouping variable?
  tally()                     # and we don't need anything at all here, it has been passed through the pipe!


# See how we go from trees to trees.summary while running one single chunk of code?


# Shortcut!!  Cmd + Shift + M on a Mac to create the %>% operator



trees.subset <- trees %>%
  filter(CommonName %in% c('Common Ash', 'Rowan', 'Scots Pine')) %>%
  group_by(CommonName, AgeGroup) %>%
  tally()

# Here we are first subsetting the data frame to only three species, 
# and counting the number of trees for each species, but also breaking them down by age group


# 2. MORE FUNTIONS OF dplyr ----

# summarise_all() ----

# will run a summary function of your choice over ALL the columns


summ.all <- summarise_all(trees, mean)


# case_when() - for re-classifying values or factors ----

# simpler function upon which it builds, ifelse()

# conditional statement which it will evaluate, 
# and the values it should return when this statement is true or false



vector <- c(4, 13, 15, 6)      # create a vector to evaluate

ifelse(vector < 10, "A", "B")  # give the conditions: if inferior to 10, return A, if not, return B

# Congrats, you're a dancing queen! (Or king!)



# case_when() lets you assign more than two outcomes. 
# All logical operators are available, and you assign the new value with a tilde ~


vector2 <- c("What am I?", "A", "B", "C", "D")

case_when(vector2 == "What am I?" ~ "I am the walrus",
          vector2 %in% c("A", "B") ~ "goo",
          vector2 == "C" ~ "ga",
          vector2 == "D" ~ "joob")


# "I am the walrus" "goo"             "goo"             "ga"          "joob"       



# 3. CHANGING FACTOR LEVELS / CREATE CATEGORICAL VARIABLES ----


# We may want to create a Genus column using mutate() that will hold that information.


# We will do this using a character string search with the grepl function, 
# which looks for patterns in the data, and specify what to return for each genus. 
# Before we do that, we may want the full list of species occuring in the data!



unique(trees$LatinName)  # Shows all the species names

# Create a new column with the tree genera

trees.genus <- trees %>%
  mutate(Genus = case_when(               # creates the genus column and specifies conditions
    grepl("Acer", LatinName) ~ "Acer",
    grepl("Fraxinus", LatinName) ~ "Fraxinus",
    grepl("Sorbus", LatinName) ~ "Sorbus",
    grepl("Betula", LatinName) ~ "Betula",
    grepl("Populus", LatinName) ~ "Populus",
    grepl("Laburnum", LatinName) ~ "Laburnum",
    grepl("Aesculus", LatinName) ~ "Aesculus",
    grepl("Fagus", LatinName) ~ "Fagus",
    grepl("Prunus", LatinName) ~ "Prunus",
    grepl("Pinus", LatinName) ~ "Pinus",
    grepl("Sambucus", LatinName) ~ "Sambucus",
    grepl("Crataegus", LatinName) ~ "Crataegus",
    grepl("Ilex", LatinName) ~ "Ilex",
    grepl("Quercus", LatinName) ~ "Quercus",
    grepl("Larix", LatinName) ~ "Larix",
    grepl("Salix", LatinName) ~ "Salix",
    grepl("Alnus", LatinName) ~ "Alnus")
  )



# To do much quicker we could use the separate() function from the tidyr package 
# to split the column into several new columns filled with the words making up the species names, 
# and keep only the first one



library(tidyr)

trees.genus.2 <- trees %>%
  tidyr::separate(LatinName, c("Genus", "Species"), sep = " ", remove = FALSE) %>%  
  dplyr::select(-Species)

# Above we're creating two new columns in a vector (genus name and species name), 
# "sep" refers to the separator, here space between the words, 
# and remove = FALSE means that we want to keep the original column LatinName in the data frame


# How we can reclassify a factor ----

trees.genus <- trees.genus %>%   # overwriting our data frame
  mutate(Height.cat =   # creating our new column
           case_when(Height %in% c("Up to 5 meters", "5 to 10 meters") ~ "Short",
                     Height %in% c("10 to 15 meters", "15 to 20 meters") ~ "Medium",
                     Height == "20 to 25 meters" ~ "Tall")
  )



# Reordering factor levels for display ----


# If we plot the number of trees in each of our new height categories, 
# we may want the bars to read, from left to right: ‘Short’, ‘Medium’, ‘Tall’. 
# However, by default, R will order them ‘Medium’, ‘Short’, ‘Tall’

# To fix this:


## Reordering a factor's levels

levels(trees.genus$Height.cat)  # shows the different factor levels in their default order

trees.genus$Height.cat <- factor(trees.genus$Height.cat,
                                 levels = c('Short', 'Medium', 'Tall'),   # whichever order you choose will be reflected in plots etc
                                 labels = c('SHORT', 'MEDIUM', 'TALL')    # Make sure you match the new names to the original levels!
)   

levels(trees.genus$Height.cat)  # a new order and new names for the levels



# 4. ADVANCED PIPING ----


# Mapping trees using eastings + northings, also showing sp. + height ----


# Subset data frame to fewer genera

trees.five <- trees.genus %>%
  filter(Genus %in% c("Acer", "Fraxinus", "Salix", "Aesculus", "Pinus"))

# Map all the trees

(map.all <- ggplot(trees.five) +
    geom_point(aes(x = Easting, y = Northing, size = Height.cat, colour = Genus), alpha = 0.5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12))
)



# If we want to save a separate map for each genus (so 5 maps in total)


# The do() function allows us to use pretty much any R function within a pipe chain, 
# provided that we supply the data as data = . where the function requires it.



# Plotting a map for each genus

tree.plots <-  
  trees.five  %>%      # the data frame
  group_by(Genus) %>%  # grouping by genus
  do(plots =           # the plotting call within the do function
       ggplot(data = .) +
       geom_point(aes(x = Easting, y = Northing, size = Height.cat), alpha = 0.5) +
       labs(title = paste("Map of", .$Genus, "at Craigmillar Castle", sep = " ")) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.text = element_text(size = 14),
             legend.text = element_text(size = 12),
             plot.title = element_text(hjust = 0.5),
             legend.position = "bottom")
  )

# You can view the graphs before saving them
tree.plots$plots

# Saving the plots to file

tree.plots %>%              # the saving call within the do function
  do(.,
     ggsave(.$plots, filename = paste(getwd(), "/", "map-", .$Genus, ".png", sep = ""), 
            device = "png", height = 12, width = 16, units = "cm"))



# Sticking things together with paste() ----

# Lets you combine text strings as well as outputs from functions or object names in the environment

# We used the paste() function to define the filename= argument of the last piece of code. 
# (We did the same to define the titles that appear on the graphs.)


paste(getwd(), '/', 'map-', .$Genus, '.png', sep = '')


# Let's take that apart

# getwd(): You are familiar with this call: try it in the console now! It writes the path to your working directory, 
# i.e. the root folder where we want to save the plots.
# ’/’: we want to add a slash after the directory folder and before writing the name of the plot
# ‘map-‘: a custom text bit that will be shared by all the plots. We’re drawing maps after all!
#  ’.$Genus’: accesses the Genus name of the tree.plots object, so each plot will bear a different name according to the tree genus.
# ‘.png’: the file extension; we could also have chosen a pdf, jpg, etc.
# ‘sep = ‘’’: we want all the previous bits to be pasted together with nothing separating them


# So, in the end, the whole string could read something like: ‘C:/Coding_Club/map-Acer.png’.