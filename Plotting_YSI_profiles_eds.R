## In the Documents/R folder on your computer, make a folder and put the database and this script inside

## set working directory
setwd("~/Documents/R/BiOGeOChemistry-ISU/CLFLWD")

## next time you can skip the manual parts and just run the line of code

## this is a list of packages needed
packages = c('ggplot2', 'dplyr', 'tidyverse')

## this is code to install the packages or load them if already installed
for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

## import your dataset (make sure data is in working directory):
library(readxl)
CLFLWD_EDI_v2 <- read_excel("~/Documents/R/BiOGeOChemistry-ISU/CLFLWD/CLFLWD_EDI_v2.xlsx")


## rename dataset 
data = CLFLWD_EDI_v2

## We need to now remove any missing data
data = data %>%
  filter(Value != '-99999') %>%  # filter out non-existent data
  filter(Lake != "Keewahtin Lake") %>% # remove any observations from Keewahtin Lake
  filter(Measurement == "Specific Conductance" | Measurement == "Temperature" | Measurement == "Dissolved Oxygen" |
           Measurement == "Iron" | Measurement == "Ammonium" | Measurement == "Phosphate")  %>%  
  select(Lake, Date, Depth, Device, Measurement, Value, Units) %>%
  # select all of the desired columns, including the organizing columns "date" and "depth" except flag, which is no longer needed
  na.omit() %>% ## omit NA 
  filter(Value > 0) # remove negative values


## R likes to have things as a slighly different format
## convert to data frame
data <- as_tibble(data)

## if you click on data in the Environment, and hover over the column headers, you'll see that some numbers are as "double" format
## let's change the change data in Value and Depth columns to numeric
data$Value <- as.numeric(as.character(data$Value))
data$Depth <- as.numeric(as.character(data$Depth))

## drop date so month and year become categorical
data$Date <- format(data$Date, format="%Y%m")

## make MEasurement factor type and relabel them
data$Measurement <-as.factor(data$Measurement)
levels(data$Measurement) <- c("Ammonium", "Dissolved Oxygen", "Dissolved Iron", "Phosphate", "Sp. Conductance", "Temperature")
levels(data$Measurement)

# making a plot called SpC with all data vs depth 
# identifying by analyte with color by date
# all circle symbols
# connecting data points with a grey line
SpC = ggplot(data, aes(x=Depth ,y=Value)) + # telling ggplot we're using the depth column as y
  geom_point(aes(x = Depth, color=Date, shape=16)) + # adding an aesthetic of points as our X values
  # the color of the points refer to dates, the size of the point is 16
  geom_line(aes(x = Depth, y = Value, color=Date)) + #adding a black line between data points
  scale_shape_identity() +
  scale_x_reverse(name = "Depth (m)") + # we have continuous data and this is the axis label
  scale_y_continuous(name = "degrees Celsius, uSiemens/cm, mg/L, mg/L, mg/L, mg/L") + # we have continuous data and this is the axis label
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_classic() + # a clean background
  scale_color_manual(name="Date", labels=c("September 2023", "October 2023", "February 2024", "April 2024"), 
                   values=c("#009E73", "#E69F00", "#F0E442", "#0072B2")) # rename the legend labels and give them good colors
SpC


# Let's put each data plot in its own plot
# Facet Wrap for small blocks, grid for long plots
# set each element to its own x scale
SpC2 = SpC + facet_grid(.~factor(Measurement, levels=c('Temperature','Sp. Conductance',
                                                       'Dissolved Oxygen','Dissolved Iron', "Ammonium", "Phosphate")), scales="free_x") +
  theme(panel.spacing = unit(0.5, "lines")) +
  theme(legend.position = "bottom") 
SpC2


#save plot in working directory with dimensions that you want
ggsave("profiles.pdf", plot=SpC2, width=8, height=4, units="in")

## Can you do the same thing with temperature? dissolved oxygen?
## Could you include all of those in your data frame and make a faceted plot by measurement with colors indicating date?

