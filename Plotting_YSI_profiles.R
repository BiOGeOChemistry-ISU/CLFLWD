## In the Documents/R folder on your computer, make a folder and put the database and this script inside

## set working directory
## Session --> Set Working Directory --> Choose Directory
## copy what appears in the console below:

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
## in the 'Environment" click on "Import Dataset"--> From Excel, Browse and find the excel file
## look at the file to make sure it appears how you expect
## copy the code in code preview below:

## next time you can skip the manual parts and just run the line of code


## your dataset should appear in the Environment, but let's rename it for simplicity
## copy your file's name into the line of code below

data = COPY_HERE 

## Sort by the measurement you are interested in. let's start with specific conductance
## in the code below you'll revise data after filtering only those rows that are measuring specific conductance:
data = data %>%
   # select for temperature measurements
  filter(Measurement =="Specific Conductance")  

## We need to now remove any missing data
data = data %>%
  filter(Value != '-99999') %>%  # filter out non-existent data
  filter(Lake != "Keewahtin") # remove any observations from Keewahtin Lake
 
## We'll select for only the data we need now
data = data %>%
  # select all of the desired columns, including the organizing columns "date" and "depth"
  select(Date, Depth, Value) 

## R likes to have things as a slighly different format
## convert to data frame
data <- as_tibble(data)

## if you click on data in the Environment, and hover over the column headers, you'll see that some numbers are as "double" format
## let's change the change data in Value and Depth columns to numeric
data$Value <- as.numeric(as.character(data$Value))
data$Depth <- as.numeric(as.character(data$Depth))

# making a plot called SpC with all data vs depth 
# identifying by analyte with color by date
# all circle symbols
# connecting data points with a grey line
SpC = ggplot(data, aes(x= ,y=Depth)) + # telling ggplot we're using the depth column as y
  geom_point(aes(x = Value, color=Date, shape=16)) + # adding an aesthetic of points as our X values
  # the color of the points refer to dates, the size of the point is 16
  #geom_path(aes(x = Value, y = Depth), color='black') + #adding a black line between data points
  scale_shape_identity() +
  scale_x_continuous(name = "Specific Conductance (microSiemens m^-2 s^-1)") + # we have continuous data and this is the axis label
  scale_y_continuous(name = "Depth (m)") + # we have continuous data and this is the axis label
  theme_bw() + # a clean background
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete()
SpC


# Let's put each data plot in its own plot
# Facet Wrap for small blocks, grid for long plots
# set each element to its own x scale
SpC2 = SpC + facet_wrap(.~Date) +
  theme(panel.spacing = unit(2, "lines")) +
  theme(legend.position = "none") 
SpC2


#save plot in working directory with dimensions that you want
ggsave("SpC_profiles.pdf", plot=SpC2, width=6, height=4, units="in")

## Can you do the same thing with temperature? dissolved oxygen?
## Could you include all of those in your data frame and make a faceted plot by measurement with colors indicating date?

