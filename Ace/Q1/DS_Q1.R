setwd("E:\\General\\Personal\\Ace")

inp_path <- "E:\\General\\Personal\\Ace"

#Run the code below this line, everytime you restart Rstudio
library(tidyverse)
library(beepr)
library(ggplot2)
library(data.table)
library(caret)
library(lubridate)
library(MLmetrics)
library(openxlsx)
library(imputeTS)
library(dplyr)
library(randomForest)
library(sparseSVM)
library(corrplot)
library(rpart.plot)
library(rpart.utils)
library(dtree)
library(xlsx)
library(foreign)
library(rpart)
library(pROC)
library(reshape2)
library(eeptools)
library(MASS)
library(glmnet)
library(car)
library(Hmisc)
library(aod)
library(Rcpp)
library(grid)
library(gridExtra)
library(cluster)
library(factoextra)
library(GA)
library(genalg)
library(rgenoud)
library(leaps)
library(readxl)
library(robfilter)
library(ROSIsoft)
library(rDotNet)
library(devtools)
library(rjson)
library(dplyr)
library(ggplot2)
library(iBreakDown)
library(ggraptR)
library(devtools)
library(rattle)
library(esquisse)
library(RWeka)
library(caTools)
library(ggplot2movies)
#End of packages loading

#Loading the Data
ingredient <- read.csv("E:/General/Personal/Ace/ingredient.csv")
#View(ingredient)

#rattle()

#=======================================================================

# Rattle is Copyright (c) 2006-2020 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2022-05-25 00:19:07 x86_64-w64-mingw32 

# Rattle version 5.4.0 user '117324'

# This log captures interactions with Rattle as an R script. 

# For repeatability, export this activity log to a 
# file, like 'model.R' using the Export button or 
# through the Tools menu. Th script can then serve as a 
# starting point for developing your own scripts. 
# After xporting to a file called 'model.R', for exmample, 
# you can type into a new R Console the command 
# "source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 

# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access the weather dataset and utilities.
library(magrittr) # Utilise %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#=======================================================================
# Rattle timestamp: 2022-05-25 00:19:13 x86_64-w64-mingw32 

# Load an R data frame.

crs$dataset <- ingredient

# Display a simple summary (structure) of the dataset.

str(crs$dataset)

#=======================================================================
# Rattle timestamp: 2022-05-25 00:19:13 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=214 train=150 validate=32 test=32

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
  crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
  crs$test

# The following variable selections have been noted.

crs$input     <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")

crs$numeric   <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2022-05-25 00:19:15 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")

crs$numeric   <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2022-05-25 00:19:29 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[, c(crs$input, crs$risk, crs$target)])

# The 'basicStats' package provides the 'fBasics' function.

library(fBasics, quietly=TRUE)

# Generate a description of the numeric data.

lapply(crs$dataset[, c(crs$input, crs$risk, crs$target)][,c(1:9)], basicStats)

# The 'kurtosis' package provides the 'fBasics' function.

library(fBasics, quietly=TRUE)

# Summarise the kurtosis of the numeric data.

kurtosis(crs$dataset[, c(crs$input, crs$risk, crs$target)][,c(1:9)], na.rm=TRUE)

# The 'skewness' package provides the 'fBasics' function.

library(fBasics, quietly=TRUE)

# Summarise the skewness of the numeric data.

skewness(crs$dataset[, c(crs$input, crs$risk, crs$target)][,c(1:9)], na.rm=TRUE)

#=======================================================================
# Rattle timestamp: 2022-05-25 00:20:11 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation ingredient using Pearson",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2022-05-25 00:20:15 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[, crs$numeric], use="pairwise", method="spearman")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation ingredient using Spearman",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2022-05-25 00:23:01 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for a

# Generate a box plot.

p01 <- crs %>%
  with(dataset[,]) %>%
  ggplot2::ggplot(ggplot2::aes(y=a)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Rattle 2022-May-25 00:23:01 117324") +
  ggplot2::ggtitle("Distribution of a") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for b

# Generate a box plot.

p02 <- crs %>%
  with(dataset[,]) %>%
  ggplot2::ggplot(ggplot2::aes(y=b)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Rattle 2022-May-25 00:23:01 117324") +
  ggplot2::ggtitle("Distribution of b") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for c

# Generate a box plot.

p03 <- crs %>%
  with(dataset[,]) %>%
  ggplot2::ggplot(ggplot2::aes(y=c)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Rattle 2022-May-25 00:23:01 117324") +
  ggplot2::ggtitle("Distribution of c") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for d

# Generate a box plot.

p04 <- crs %>%
  with(dataset[,]) %>%
  ggplot2::ggplot(ggplot2::aes(y=d)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Rattle 2022-May-25 00:23:01 117324") +
  ggplot2::ggtitle("Distribution of d") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for e

# Generate a box plot.

p05 <- crs %>%
  with(dataset[,]) %>%
  ggplot2::ggplot(ggplot2::aes(y=e)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Rattle 2022-May-25 00:23:01 117324") +
  ggplot2::ggtitle("Distribution of e") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for f

# Generate a box plot.

p06 <- crs %>%
  with(dataset[,]) %>%
  ggplot2::ggplot(ggplot2::aes(y=f)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Rattle 2022-May-25 00:23:01 117324") +
  ggplot2::ggtitle("Distribution of f") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for g

# Generate a box plot.

p07 <- crs %>%
  with(dataset[,]) %>%
  ggplot2::ggplot(ggplot2::aes(y=g)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Rattle 2022-May-25 00:23:01 117324") +
  ggplot2::ggtitle("Distribution of g") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for h

# Generate a box plot.

p08 <- crs %>%
  with(dataset[,]) %>%
  ggplot2::ggplot(ggplot2::aes(y=h)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Rattle 2022-May-25 00:23:01 117324") +
  ggplot2::ggtitle("Distribution of h") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for i

# Generate a box plot.

p09 <- crs %>%
  with(dataset[,]) %>%
  ggplot2::ggplot(ggplot2::aes(y=i)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Rattle 2022-May-25 00:23:01 117324") +
  ggplot2::ggtitle("Distribution of i") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01, p02, p03, p04, p05, p06, p07, p08, p09)

#=======================================================================
# Rattle timestamp: 2022-05-25 00:23:02 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for a

# Generate the plot.

p01 <- crs %>%
  with(dataset[,]) %>%
  dplyr::select(a) %>%
  ggplot2::ggplot(ggplot2::aes(x=a)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("a\n\nRattle 2022-May-25 00:23:02 117324") +
  ggplot2::ggtitle("Distribution of a") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for b

# Generate the plot.

p02 <- crs %>%
  with(dataset[,]) %>%
  dplyr::select(b) %>%
  ggplot2::ggplot(ggplot2::aes(x=b)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("b\n\nRattle 2022-May-25 00:23:02 117324") +
  ggplot2::ggtitle("Distribution of b") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for c

# Generate the plot.

p03 <- crs %>%
  with(dataset[,]) %>%
  dplyr::select(c) %>%
  ggplot2::ggplot(ggplot2::aes(x=c)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("c\n\nRattle 2022-May-25 00:23:02 117324") +
  ggplot2::ggtitle("Distribution of c") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for d

# Generate the plot.

p04 <- crs %>%
  with(dataset[,]) %>%
  dplyr::select(d) %>%
  ggplot2::ggplot(ggplot2::aes(x=d)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("d\n\nRattle 2022-May-25 00:23:02 117324") +
  ggplot2::ggtitle("Distribution of d") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for e

# Generate the plot.

p05 <- crs %>%
  with(dataset[,]) %>%
  dplyr::select(e) %>%
  ggplot2::ggplot(ggplot2::aes(x=e)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("e\n\nRattle 2022-May-25 00:23:02 117324") +
  ggplot2::ggtitle("Distribution of e") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for f

# Generate the plot.

p06 <- crs %>%
  with(dataset[,]) %>%
  dplyr::select(f) %>%
  ggplot2::ggplot(ggplot2::aes(x=f)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("f\n\nRattle 2022-May-25 00:23:02 117324") +
  ggplot2::ggtitle("Distribution of f") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for g

# Generate the plot.

p07 <- crs %>%
  with(dataset[,]) %>%
  dplyr::select(g) %>%
  ggplot2::ggplot(ggplot2::aes(x=g)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("g\n\nRattle 2022-May-25 00:23:04 117324") +
  ggplot2::ggtitle("Distribution of g") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for h

# Generate the plot.

p08 <- crs %>%
  with(dataset[,]) %>%
  dplyr::select(h) %>%
  ggplot2::ggplot(ggplot2::aes(x=h)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("h\n\nRattle 2022-May-25 00:23:04 117324") +
  ggplot2::ggtitle("Distribution of h") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for i

# Generate the plot.

p09 <- crs %>%
  with(dataset[,]) %>%
  dplyr::select(i) %>%
  ggplot2::ggplot(ggplot2::aes(x=i)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("i\n\nRattle 2022-May-25 00:23:04 117324") +
  ggplot2::ggtitle("Distribution of i") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02, p03, p04, p05, p06, p07, p08, p09)

#=======================================================================
# Rattle timestamp: 2022-05-25 00:23:04 x86_64-w64-mingw32 

# Generate just the data for an Ecdf plot of the variable 'a'.

ds <- rbind(data.frame(dat=crs$dataset[,][,"a"], grp="All"))

# The 'Hmisc' package provides the 'Ecdf' function.

library(Hmisc, quietly=TRUE)

# Plot the data.

Ecdf(ds[ds$grp=="All",1], col="#E495A5", xlab="a", lwd=2, ylab=expression(Proportion <= x), subtitles=FALSE)


# Add a title to the plot.

title(main="Distribution of a",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2022-05-25 00:23:05 x86_64-w64-mingw32 

# Generate just the data for an Ecdf plot of the variable 'b'.

ds <- rbind(data.frame(dat=crs$dataset[,][,"b"], grp="All"))

# The 'Hmisc' package provides the 'Ecdf' function.

library(Hmisc, quietly=TRUE)

# Plot the data.

Ecdf(ds[ds$grp=="All",1], col="#E495A5", xlab="b", lwd=2, ylab=expression(Proportion <= x), subtitles=FALSE)


# Add a title to the plot.

title(main="Distribution of b",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2022-05-25 00:23:05 x86_64-w64-mingw32 

# Generate just the data for an Ecdf plot of the variable 'c'.

ds <- rbind(data.frame(dat=crs$dataset[,][,"c"], grp="All"))

# The 'Hmisc' package provides the 'Ecdf' function.

library(Hmisc, quietly=TRUE)

# Plot the data.

Ecdf(ds[ds$grp=="All",1], col="#E495A5", xlab="c", lwd=2, ylab=expression(Proportion <= x), subtitles=FALSE)


# Add a title to the plot.

title(main="Distribution of c",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2022-05-25 00:23:05 x86_64-w64-mingw32 

# Generate just the data for an Ecdf plot of the variable 'd'.

ds <- rbind(data.frame(dat=crs$dataset[,][,"d"], grp="All"))

# The 'Hmisc' package provides the 'Ecdf' function.

library(Hmisc, quietly=TRUE)

# Plot the data.

Ecdf(ds[ds$grp=="All",1], col="#E495A5", xlab="d", lwd=2, ylab=expression(Proportion <= x), subtitles=FALSE)


# Add a title to the plot.

title(main="Distribution of d",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2022-05-25 00:23:05 x86_64-w64-mingw32 

# Generate just the data for an Ecdf plot of the variable 'e'.

ds <- rbind(data.frame(dat=crs$dataset[,][,"e"], grp="All"))

# The 'Hmisc' package provides the 'Ecdf' function.

library(Hmisc, quietly=TRUE)

# Plot the data.

Ecdf(ds[ds$grp=="All",1], col="#E495A5", xlab="e", lwd=2, ylab=expression(Proportion <= x), subtitles=FALSE)


# Add a title to the plot.

title(main="Distribution of e",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2022-05-25 00:23:06 x86_64-w64-mingw32 

# Generate just the data for an Ecdf plot of the variable 'f'.

ds <- rbind(data.frame(dat=crs$dataset[,][,"f"], grp="All"))

# The 'Hmisc' package provides the 'Ecdf' function.

library(Hmisc, quietly=TRUE)

# Plot the data.

Ecdf(ds[ds$grp=="All",1], col="#E495A5", xlab="f", lwd=2, ylab=expression(Proportion <= x), subtitles=FALSE)


# Add a title to the plot.

title(main="Distribution of f",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2022-05-25 00:23:06 x86_64-w64-mingw32 

# Generate just the data for an Ecdf plot of the variable 'g'.

ds <- rbind(data.frame(dat=crs$dataset[,][,"g"], grp="All"))

# The 'Hmisc' package provides the 'Ecdf' function.

library(Hmisc, quietly=TRUE)

# Plot the data.

Ecdf(ds[ds$grp=="All",1], col="#E495A5", xlab="g", lwd=2, ylab=expression(Proportion <= x), subtitles=FALSE)


# Add a title to the plot.

title(main="Distribution of g",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2022-05-25 00:23:06 x86_64-w64-mingw32 

# Generate just the data for an Ecdf plot of the variable 'h'.

ds <- rbind(data.frame(dat=crs$dataset[,][,"h"], grp="All"))

# The 'Hmisc' package provides the 'Ecdf' function.

library(Hmisc, quietly=TRUE)

# Plot the data.

Ecdf(ds[ds$grp=="All",1], col="#E495A5", xlab="h", lwd=2, ylab=expression(Proportion <= x), subtitles=FALSE)


# Add a title to the plot.

title(main="Distribution of h",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2022-05-25 00:23:06 x86_64-w64-mingw32 

# Generate just the data for an Ecdf plot of the variable 'i'.

ds <- rbind(data.frame(dat=crs$dataset[,][,"i"], grp="All"))

# The 'Hmisc' package provides the 'Ecdf' function.

library(Hmisc, quietly=TRUE)

# Plot the data.

Ecdf(ds[ds$grp=="All",1], col="#E495A5", xlab="i", lwd=2, ylab=expression(Proportion <= x), subtitles=FALSE)


# Add a title to the plot.

title(main="Distribution of i",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2022-05-25 00:24:00 x86_64-w64-mingw32 

# KMeans 

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# The 'reshape' package provides the 'rescaler' function.

library(reshape, quietly=TRUE)

# Time taken: 0.02 secs

#=======================================================================
# Rattle timestamp: 2022-05-25 00:24:20 x86_64-w64-mingw32 

# KMeans 

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# The 'reshape' package provides the 'rescaler' function.

library(reshape, quietly=TRUE)

wine.stand <- scale(ingredient[-1]) 
wssplot <- function(ingredient, nc=10, seed=1234){
  wss <- (nrow(ingredient)-1)*sum(apply(ingredient,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(ingredient, centers=i)$withinss)}
  plot(1:nc, wss,type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(wine.stand, nc=10) 

# Generate a kmeans cluster of size 5.

crs$kmeans <- kmeans(sapply(na.omit(crs$dataset[, crs$numeric]), rescaler, "range"), 5)

#=======================================================================
# Rattle timestamp: 2022-05-25 00:24:20 x86_64-w64-mingw32 

# Report on the cluster characteristics. 

# Cluster sizes:

paste(crs$kmeans$size, collapse=' ')

# Data means:

colMeans(sapply(na.omit(crs$dataset[, crs$numeric]), rescaler, "range"))

# Cluster centers:

crs$kmeans$centers

# Within cluster sum of squares:

crs$kmeans$withinss

# Time taken: 0.00 secs

# Generate a discriminant coordinates plot.

cluster::clusplot(na.omit(crs$dataset[, intersect(crs$input, crs$numeric)]), crs$kmeans$cluster, color=TRUE, shade=TRUE, main='Discriminant Coordinates ingredient')


#=======================================================================
# Rattle timestamp: 2022-05-25 00:24:27 x86_64-w64-mingw32 

# Display a scatterplot matrix for the KMeans clustering. 

#=======================================================================
# Rattle timestamp: 2022-05-25 00:24:45 x86_64-w64-mingw32 

# KMeans 

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# The 'reshape' package provides the 'rescaler' function.

library(reshape, quietly=TRUE)

# Generate a kmeans cluster of size 2.

crs$kmeans <- kmeans(sapply(na.omit(crs$dataset[, crs$numeric]), rescaler, "range"), 2)

#=======================================================================
# Rattle timestamp: 2022-05-25 00:24:45 x86_64-w64-mingw32 

# Report on the cluster characteristics. 

# Cluster sizes:

paste(crs$kmeans$size, collapse=' ')

# Data means:

colMeans(sapply(na.omit(crs$dataset[, crs$numeric]), rescaler, "range"))

# Cluster centers:

crs$kmeans$centers

# Within cluster sum of squares:

crs$kmeans$withinss

# Time taken: 0.00 secs

# Generate a discriminant coordinates plot.

cluster::clusplot(na.omit(crs$dataset[, intersect(crs$input, crs$numeric)]), crs$kmeans$cluster, color=TRUE, shade=TRUE, main='Discriminant Coordinates ingredient')


# Plot the data.

#It appears as if there are two distinct clusters of data as given by within the group sum of squares

 
