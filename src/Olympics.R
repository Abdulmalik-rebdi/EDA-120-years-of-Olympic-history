# Install tidytuesdayR
install.packages("tidytuesdayR")

# Download all data files
tuesdata <- tidytuesdayR::tt_load("2021-07-27")

# Individual dataframes
olympics <- tuesdata$olympics

# A second dataframe is available
# It is not required, but may prove useful in certain cases
regions <- tuesdata$regions



library(tidyverse)

olympics

colnames(olympics)


olympics %>% 
  group_by(team) %>% 
  summarise(count = l()) %>% 
  split(1:4) %>% 
  ggplot(aes(x=team)) 
  geom_point()
  art <- filter(olympics,event == "art")
  art  
  
  
  

 