#===== Information =====#

### File: Image_data_extraction.R
### Author: Agnes Szwarczynska
### Date: 27/06/2023
### Project: Master's thesis

#===== Required packages =====#

library(metagear)

#===== Data extraction =====#

### Image 1 ###

image_b <- "C:/Users/Agnieszka/Documents/R_data/meta/Cerchiara_2017b.jpg"
figure_display(image_b)

rawData <- figure_scatterPlot(image_b, 
                              point_shape = "circle",
                              axis_thickness = 3,
                              point_size = 5, 
                              X_min = - 0.2,
                              X_max = 0.4,
                              Y_min = 0.7,
                              Y_max = 1.3,
                              axis_sensitivity = 5)

### Image 2 ###

image_children <- "C:/Users/Agnieszka/Documents/R_data/meta/Cerchiara_2017b.jpg"
rawData <- figure_scatterPlot(image_children, 
                              point_shape = "circle",
                              axis_thickness = 1,
                              point_size = 2,
                              X_min = 0,
                              X_max = 6,
                              Y_min = 0,
                              Y_max = 3,
                              axis_sensitivity = 4)

### Image 3 ###
  
flycatchers_1 <- "C:/Users/Agnieszka/Documents/R_data/meta/flycatchers.jpg"
figure_display(flycatchers_1)
  
rawData <- figure_scatterPlot(flycatchers_1, 
                              point_shape = "circle",
                              axis_thickness = 1,
                              point_size = 2,
                              axis_sensitivity = 4)

### Image 4 ###

flycatchers_2 <- "C:/Users/Agnieszka/Documents/R_data/meta/flycatchers_2.jpg"
figure_display(flycatchers_2)

rawData <- figure_scatterPlot(flycatchers_2, 
                                point_shape = "circle",
                                axis_thickness = 1,
                                point_size = 2,
                                axis_sensitivity = 4)

### Image 5 ###

flycatchers_3 <- "C:/Users/Agnieszka/Documents/R_data/meta/flycatchers_3.jpg"
figure_display(flycatchers_3)
  
rawData <- figure_scatterPlot(flycatchers_3, 
                                point_shape = "circle",
                                axis_thickness = 1,
                                point_size = 2,
                                axis_sensitivity = 4)


### Image 6 ###
  
birds <- "C:/Users/Agnieszka/Documents/R_data/meta/F4.large.jpg"
figure_display(birds)
  
rawData <- figure_scatterPlot(birds, 
                                point_shape = "circle",
                                axis_thickness = 1,
                                point_size = 2,
                                axis_sensitivity = 4)
