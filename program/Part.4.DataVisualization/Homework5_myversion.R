### Homework 5, Madison Harris

# import libraries
library(tidyverse) 
library(ggforce) 
library(ggsci)
library(patchwork)
library(Hmisc)

bloom_df <- read.csv('bloom_df.csv')
bloom_df[1:5,]
str(bloom_df) ## read as data.frame

##  Task 1
# Create two scatterplots of logbodysize vs. trophic_position grouped by reg

ggplot(data=city_df,aes(x=Temp,y=Ppt)) + 
  geom_point(aes(fill=State_name,shape=State_name),size=2,colour='black') + 
  stat_smooth(method='lm',linetype='dashed',colour='black',size=2) + 
  ggtitle('Temp vs. Precip. by State') + 
  scale_fill_manual(name='State:',values=c('red','blue','yellow','green','grey70')) + ## manually filling everything. Values as specific colors
  scale_shape_manual(name='State:',values=c(21,22,23,24,25)) + ## custom shapes
  scale_x_continuous(name='Temperature',limits=c(5,25),breaks=c(5,10,15,20,25),position = 'top') + # continuous x axis, limits, breaks at these points, on top of graph
  scale_y_continuous(name='Precipitation',limits=c(0,1500),breaks=c(0,250,500,1000,1250,1500)) 

## plot 1.1: a 2 trend lines (method = ‘lm’), one for each reg variable




# plot 1.2 a single trend line for the whole model
ggplot(data=bloom_df,aes(x=logbodysize,y=trophic_position,fill=reg)) +
  geom_point(aes(fill=reg,shape=reg),size=3,colour='black') +
  geom_smooth(method = lm) +
  ggtitle('Logbodysize vs Trophic_position, Plot 1.1') +
  scale_fill_manual(name='reg',values=c('blue','red')) +
  scale_shape_manual(name='reg',values=c(23,25))

## Task 2
#Create 4 plots:
  # bar and error bars (mean and 95% conf. int.)
  # point and error bars (mean and 95% conf. int.)
  # box plots
  # raw data + point and error bars (mean and 95% conf. int.)


## make long: 

bloom_long_df <- bloom_df %>%
  pivot_longer(cols = c(logbodysize, trophic_position), names_to = "Log", values_to = "value")
View(bloom_long_df)



##copy and paste ref from intro

Ppt_sum_df <- city_df %>%
  group_by(State) %>%
  summarise(mean = mean(Ppt, na.rm = TRUE), #Ppt mean
            sd = sd(Ppt, na.rm = TRUE), #Ppt standard deviation
            n = n()) %>% #Ppt count
  mutate(se = sd / sqrt(n), #Ppt standard error
         ci = 1.96*se) #Ppt 95% confidence interval


## my version
    ## logbodysize = Ppt. trophic = Temp. reg = state. Mean = Value

log_sum_df <- bloom_df %>%
  group_by(reg) %>%
  summarise(value= mean(logbodysize, na.rm = TRUE), #log mean
            sd = sd(logbodysize, na.rm = TRUE), #temp standard deviation
            n = n()) %>% #temp count
  mutate(se = sd / sqrt(n), #Temp standard error      ## using variables above, create new column called "se". Adds new variables. ?mutate 
         ci = 1.96 * se) #Temp 95% confidence interval
#log_sum_df$reg <- 'logbodysize'

View(log_sum_df)
Temp_sum_df
trophic_sum_df <- bloom_df %>%
  group_by(reg) %>%
  summarise(value= mean(trophic_position, na.rm = TRUE), #Ppt mean
            sd = sd(trophic_position, na.rm = TRUE), #Ppt standard deviation
            n = n()) %>% #Ppt count
  mutate(se = sd / sqrt(n), #Ppt standard error
         ci = 1.96*se) #Ppt 95% confidence interval
#trophic_sum_df$reg <- 'trophic_position'

View(trophic_sum_df)


lagtro_df <- rbind(log_sum_df,trophic_sum_df)  ## combine two dataframes into one. Just stacks them on top of each other
View(lagtro_df)

#   plot 1 --> bar and error bars (mean and 95% conf. int.)
      ## email trevor about this step: confused as to what he wants here. ALSO: ask about getting trendlines to stick to both datasets, not just one

ggplot(data = lagtro_df, aes(x = reg, y = value)) + geom_bar(stat = "identity")


ggplot(data=lagtro_df,aes(x=reg,y=value)) +
  geom_bar(stat='identity')
  #geom_errorbar(aes(ymin = value, ymax = value))

ggplot(data = lagtro_df, aes(x = reg, y = value, fill = reg)) +
  geom_bar(stat = "identity")

ggplot(data = lagtro_df, aes(x = reg, y = value, fill = reg)) +
  facet_wrap(~reg, nrow = 1, scales = "free") + geom_bar(stat = "identity")

ggplot(data = lagtro_df, aes(x = reg, y = value, fill = reg)) +
  facet_wrap(~reg, nrow = 1, scales = "free") + geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = value - ci, ymax = value + ci))




#Task 2: point and error bars (mean and 95% conf. int.)


