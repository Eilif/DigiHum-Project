#install packages if necessary and load
#install.packages("tidyverse", "psych", "scales", "beanplot", "gridExtra")
library(tidyverse)
library(psych)
library(scales)
library(beanplot)
library(gridExtra)

#set working directory
setwd("C:/Users/EAO/Desktop/DigHum")

#read in csv
RID <- read.csv("DigiHum.csv")

#creating a dataset of the Altered texts
altered <- RID[grep("Altered", RID$X),]

#creating a dataset of the Fiction texts
fiction <- RID[grep("Fiction", RID$X),]

#stripcharts of primary and secondary variables
stripchart(RID$Primary, method = "stack", pch = 19, col = "skyblue3",
           xlab = "Primary")

stripchart(RID$Secondary, method = "stack", pch = 19, col = "skyblue3",
           xlab = "Secondary")

#creating an object of sturges function
sturges <- function(x){
    R <- diff(range(x)) # get range of the data
    n <- length(x) # get number of observations
    ci <- R / (1 + log2(n)) # class interval with Sturges formula
    nclass_sturges <- ceiling(R/ci) # number of classes
    b <- pretty(x, n = nclass_sturges)
    b
}

#getting breakpoints
brx_primary <- sturges(RID$Primary)
brx_primary

brx_alt_primary <- sturges(altered$Primary)
brx_alt_primary

brx_alt_secondary <- sturges(altered$Secondary)
brx_alt_secondary

brx_fic_primary <- sturges(fiction$Primary)
brx_fic_primary

brx_fic_secondary <- sturges(fiction$Secondary)
brx_fic_secondary

#histograms with breakpoints for binwidth
ggplot(RID, aes(Primary)) +
    geom_histogram(color = "black", fill = "skyblue3", breaks = brx_primary)

ggplot(altered, aes(Primary)) +
    geom_histogram(color = "black", fill = "skyblue3", breaks = brx_alt_primary)

ggplot(fiction, aes(Primary)) +
    geom_histogram(color = "black", fill = "skyblue3", breaks = brx_fic_primary)

ggplot(altered, aes(Secondary)) +
    geom_histogram(color = "black", fill = "skyblue3", breaks = brx_alt_secondary)

ggplot(fiction, aes(Secondary)) +
    geom_histogram(color = "black", fill = "skyblue3", breaks = brx_fic_secondary)

#combined histograms with kernel density for easier comparison
alt_prim <- ggplot(altered, aes(Primary)) +
    geom_histogram(color = "black", fill = "skyblue3", breaks = brx_alt_primary,
                   aes(y = ..density..)) +
    geom_density(fill = NA, colour = "black", size = 1) +
    labs(x = "Primary", y = "Word Count") +
    ggtitle("Altered")
fic_prim <- ggplot(fiction, aes(Primary)) +
    geom_histogram(color = "black", fill = "skyblue3", breaks = brx_fic_primary,
                   aes(y = ..density..)) +
    geom_density(fill = NA, colour = "black", size = 1) +
    labs(x = "Primary", y = "Word Count") +
    ggtitle("Fiction") +
    geom_density(fill = NA, colour = "black", size = 1)
grid.arrange(alt_prim, fic_prim, ncol = 2)

alt_sec <- ggplot(altered, aes(Secondary)) +
    geom_histogram(color = "black", fill = "skyblue3", breaks = brx_alt_secondary,
                   aes(y = ..density..)) +
    geom_density(fill = NA, colour = "black", size = 1) +
    labs(x = "Secondary", y = "Word Count") +
    ggtitle("Altered")
fic_sec <- ggplot(fiction, aes(Secondary)) +
    geom_histogram(color = "black", fill = "skyblue3", breaks = brx_fic_secondary,
                   aes(y = ..density..)) +
    geom_density(fill = NA, colour = "black", size = 1) +
    labs(x = "Secondary", y = "Word Count") +
    ggtitle("Fiction")
grid.arrange(alt_sec, fic_sec, ncol = 2)

#kernel density estimator
ggplot(RID, aes(Primary, fill = Secondary)) +
    geom_density(alpha = .5)

#kernel density estimator + histogram
RID %>%
    ggplot(aes(Primary)) +
    geom_histogram(breaks = brx_primary, colour = "black", fill = "skyblue3",
                   aes(y = ..density..)) +
    geom_density(fill = NA, colour = "black", size = 1)

altered %>%
    ggplot(aes(Primary)) +
    geom_histogram(breaks = brx_alt_primary, colour = "black", fill = "skyblue3",
                   aes(y = ..density..)) +
    geom_density(fill = NA, colour = "black", size = 1)

altered %>%
    ggplot(aes(Secondary)) +
    geom_histogram(breaks = brx_alt_secondary, colour = "black", fill = "skyblue3",
                   aes(y = ..density..)) +
    geom_density(fill = NA, colour = "black", size = 1)

fiction %>%
    ggplot(aes(Primary)) +
    geom_histogram(breaks = brx_fic_primary, colour = "black", fill = "skyblue3",
                   aes(y = ..density..)) +
    geom_density(fill = NA, colour = "black", size = 1)

fiction %>%
    ggplot(aes(Secondary)) +
    geom_histogram(breaks = brx_fic_secondary, colour = "black", fill = "skyblue3",
                   aes(y = ..density..)) +
    geom_density(fill = NA, colour = "black", size = 1)

#adding a normal density layer
RID %>%
    ggplot(aes(Primary)) +
    geom_histogram(breaks = brx_primary, colour = "black", fill = "skyblue3",
                   aes(y = ..density..)) +
    stat_function(fun = dnorm,
                  args = c(mean = mean(RID$Primary),
                           sd = sd(RID$Primary)),
                  size = 1, color = "red", lty = 2) +
    geom_density(fill = NA, colour = "black", size = 1)

#horizontally oriented box plot
ggplot(RID, aes(Primary, Secondary)) +
    geom_boxplot() +
    coord_flip() +
    geom_jitter()
    
#violin plot
density <- ggplot(RID, aes(Primary)) +
    geom_density() +
    ggtitle("Kernel Density Plot")
# x = factor(0) is a hack to get a single violin plot
violin <- ggplot(RID, aes(x = factor(0), y = Primary)) +
    geom_violin() +
    coord_flip() +
    xlab(NULL) +
    ggtitle("Violin Plot")
grid.arrange(density, violin, ncol = 2)

#layered plot
ggplot(RID, aes(Primary, Secondary)) +
    geom_violin(scale = "count") +
    geom_jitter()

ggplot(altered, aes(Primary, Secondary)) +
    geom_violin(scale = "count") +
    geom_jitter()

ggplot(fiction, aes(Primary, Secondary)) +
    geom_violin(scale = "count") +
    geom_jitter()
