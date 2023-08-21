
#===== Information =====#

### File: Publication_bias.R
### Author: Agnes Szwarczynska
### Date: 27/06/2023
### Project: Master's thesis

# Code for creating a funnel plot can be found 
# in the Bayesian_analysis.R file

#===== Required packages =====#

library(readr)
library(reshape2)
library(ggplot2)
library(sjPlot)

#===== Loading data =====#

data_for_B3 <- read_csv("data_for_B3.csv")

#===== Plotting =====#

### Zr (standardized correlation coefficient) vs Individuals

p <- ggplot(data=data_for_B3, aes(x=Individuals, y=Zr, group=1)) +
  geom_point()

p <- p + xlim(0, 500) + ylim(-1,1) + ### excluding the study with 4000 observations
theme(axis.text = element_text(size = 25),
      axis.title = element_text(size = 25),
      plot.margin = margin(t = 20,  # Top margin
                           r = 10,  
                           b = 20,  
                           l = 30)) +
  xlab("Individuals") + ylab("Zr")

save_plot("Individuals_pub_bias.svg", fig = p, width=15, height=12)

### Zr (standardized correlation coefficient) vs Year of publication

p_2 <- ggplot(data=data_for_B3, aes(x=Year, y=Zr, group=1)) +
  geom_point()

p_2 <- p_2 + xlim(2004,2025) + ylim(-1,1) +
  theme(axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.margin = margin(t = 20,  # Top margin
                             r = 50,  
                             b = 20,  
                             l = 30)) +
  xlab("Year") + ylab("Zr")


save_plot("Year_pub_bias.svg", fig = p_2, width=15, height=12)

#===== Study estimates versus year of publication - detecting bias =====#

#Bayesian_analysis.R file is needed to run this part
# as estimates come from the m_brms_3 model

#Zr estimates are normally distributed

aut_est <- all_authors$Author.Estimate.Intercept
years <- c(2019,2021,2016,2013,2016,2020,2018,2020,2021,2017,
           2018, 2018,2022,2022,2019,2004,2015,2019,2019,2014,
           2021,2022,2022,2015,2018,2017,2020,2011, 2016, 2017,
           2012,2018, 2018, 2019, 2022, 2022, 2021, 2016)

plot(years, aut_est)

cor.test(years, aut_est, method = "pearson") #alternatively use Spearman's rank correlation

