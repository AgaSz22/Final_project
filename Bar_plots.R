
#===== Information =====#

### File: Bar_plots.R
### Author: Agnes Szwarczynska
### Date: 27/06/2023
### Project: Master's thesis

#===== Required packages =====#

library(readr)
library(reshape2)
library(readxl)

#===== Loading data =====#

data_for_B3 <- read_excel("data_for_B3.xlsx")
data_c <- data_for_B3

#===== Preparing data =====#

### Preparing unique combinations of traits and study ID

unique_dataset <- unique(data_c[c("Study_ID", "Class")]) 
unique_dataset2 <- unique(data_c[c("Study_ID", "Method")])
unique_dataset3 <- unique(data_c[c("Study_ID", "Tissue")])
unique_dataset4 <- unique(data_c[c("Study_ID", "Longevity")])
unique_dataset5 <- unique(data_c[c("Study_ID", "Parental_care")])
unique_dataset6 <- unique(data_c[c("Study_ID", "TL_period_reproduction")])
max_life <-  unique(data_c[c("Study_ID", "Maximum_lifespan")]) 

### Counting the number of occurrences 

class_counts <- table(unique_dataset$Class)
method_counts <- table(unique_dataset2$Method)
tissue_counts <- table(unique_dataset3$Tissue)
longevity_counts <- table(unique_dataset4$Longevity)
pc_counts <- table(unique_dataset5$Parental_care)
tpr_counts <- table(unique_dataset6$TL_period_reproduction)

#===== Making bar plots =====#

### Main plot

par(mar = c(5, 5, 5, 5)) # setting up margins width

opar <- par(lwd = 2.5) # making borders of bars thicker

class_barplot <- barplot(class_counts, 
        xlab = "Class", 
        ylab = "Number of studies",
        cex.names = 1.5,
        cex.axis = 1.5,
        cex.lab = 1.7,
        col = c("white", "black", "white", "black"),
        ylim = c(0,30),
        space = c(0.2,0.2,0.2,0.2),
        )

text(x = class_barplot, y = class_counts,labels = class_counts, #adding numbers above the bars
     pos = 3, cex = 2, col = "black")

##### Methods bar plot

par(mar = c(5.5, 5.5, 2.5, 2.5)) # setting up margins width

method_barplot <- barplot(method_counts, 
        xlab = "Method", 
        ylab = "Number of studies",
        cex.names = 2,
        cex.axis = 2,
        cex.lab = 2.5,
        col = c("white", "black"),
        ylim = c(0,40),
        space = c(0.2,0.2),
)

text(x = method_barplot, y = method_counts,labels = method_counts, #adding numbers above the bars
     pos = 3, cex = 2, col = "black")

##### Tissue bar plot

par(mar = c(5.5, 5.5, 2.5, 2.5)) # setting up margins width

tissue_barplot <- barplot(tissue_counts, 
        xlab = "Tissue", 
        ylab = "Number of studies",
        cex.names = 2,
        cex.axis = 2,
        cex.lab = 2.5,
        col = c("white", "black"),
        ylim = c(0,40),
        space=c(0.2,0.2),
)

text(x = tissue_barplot, y = tissue_counts,labels = tissue_counts, #adding numbers above the bars
     pos = 3, cex = 2, col = "black")


#### Longevity bar plot 

barplot(longevity_counts, 
        xlab = "Longevity", 
        ylab = "Number of studies",
        cex.names= 2,
        cex.axis= 2,
        cex.lab= 2.5,
        col = c("white", "black"),
        ylim = c(0,35),
        space=c(0.2,0.2),
)

#### Parental care plot

m_pc_counts <- melt(pc_counts) ### preparing data so that the bars can be placed in descending order
m_pc_counts <- data.frame(m_pc_counts)
telo_des <- m_pc_counts[order(-m_pc_counts$value),]

par(mar = c(5.5, 5.5, 2.5, 2.5)) ### setting up margins width

parental_plot <- barplot(telo_des$value, 
        xlab = "Parental care", 
        ylab = "Number of studies",
        names.arg = telo_des$Var1, ### making bar labels follow the order
        cex.names = 2,
        cex.axis = 2,
        cex.lab = 2.5,
        col = c("white", "black", "white"),
        ylim = c(0,40),
        space = c(0.2,0.2, 0.2),

)

text(x= parental_plot, y=telo_des$value,labels = telo_des$value, #adding numbers above the bars
     pos = 3, cex = 2, col = "black")

##### TL measurement plot

m_tpr_counts <- melt(tpr_counts) ### preparing data so that the bars can be placed in descending order
m_tpr_counts <- data.frame(m_tpr_counts)
tpr_des <- m_tpr_counts[order(-m_tpr_counts$value),]

par(mar = c(5.5, 5.5, 2.5, 2.5)) ### setting up margins width

tpr_plot <- barplot(tpr_des$value, 
        xlab = "TL measurement", 
        ylab = "Number of studies",
        names.arg = tpr_des$Var1, ### making bar labels follow the order
        cex.names = 2,
        cex.axis = 2,
        cex.lab = 2.5,
        col = c("white", "black", "white"),
        ylim = c(0,40),
        space = c(0.2,0.2, 0.2),
        
)

text(x = tpr_plot, y = tpr_des$value, labels = tpr_des$value, #adding numbers above the bars
     pos = 3, cex = 2, col = "black")

#### Plotting maximum lifespan of a given species against a study ID
plot(data_c$Study_ID,data_c$Maximum_lifespan)

