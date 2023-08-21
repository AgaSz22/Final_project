#===== Information =====#

### File: Metagear_screening.R
### Author: Agnes Szwarczynska
### Date: 27/06/2023
### Project: Master's thesis

#===== Required packages =====#

library(metagear)
library(tidyverse)

#===== Loading data =====# 

metagear_1 <- read_csv("metagear_1.csv")
metagear_2 <- read_csv("metagear_2.csv")
metagear_papers <- rbind(metagear_1, metagear_2)

#names(metagear_papers)

### Unifying column titles

names(metagear_papers)[names(metagear_papers) == "Authors"] <- "AUTHORS"
names(metagear_papers)[names(metagear_papers) == "Article Title"] <- "TITLE"
names(metagear_papers)[names(metagear_papers) == "Abstract"] <- "ABSTRACT"

#nrow(metagear_papers)

#===== Screening =====#

papers_for_scr <- effort_initialize(metagear_papers)
#names(papers_for_scr)

team <- c("Agnes")
papers_for_scr <- effort_distribute(papers_for_scr, reviewers = team, save_split = FALSE)
papers_for_scr <- effort_distribute(papers_for_scr, initialize = TRUE, reviewers = "Agnes", save_split = TRUE) # initialise the metagear GUI


abstract_screener("effort_Agnes.csv", aReviewer = "Agnes", highlightColor = "palegoldenrod" , highlightKeywords = c("telom", 
                                                                                 "telomere", 
                                                                                 "telomeres", 
                                                                                 "reproductive",
                                                                                 "reproduction",
                                                                                 "reproductive aging",
                                                                                 "reproductive quality",
                                                                                 "reproductive senescence",
                                                                                 "reproductive trade-offs",
                                                                                 "reproductive success",
                                                                                 "reproductive costs",
                                                                                 "reproductive investment",
                                                                                 "reproductive stress",
                                                                                 "parental investment",
                                                                                 "parental costs",
                                                                                 "parental quality",
                                                                                 "parental trade-offs",
                                                                                 "fecundity",
                                                                                 "fertility",
                                                                                 "rearing",
                                                                                 "shortening",
                                                                                 "shorter",
                                                                                 "short",
                                                                                 "long",
                                                                                 "longer",
                                                                                 "elongation",
                                                                                 "attrition"
                                                                                 ))
