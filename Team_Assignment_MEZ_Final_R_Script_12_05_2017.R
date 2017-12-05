#Summary - this script analyzes the relationship between obesity and income. See detailed explanations above each individual plot.

#PACKAGES

install.packages(c("ggplot2", "reshape2", "maps", "mapproj", "fiftystater")) 

library("ggplot2")
#ggplot2 enables attractive visualizations
library("reshape2")
#reshape2 allows us to take repeating values within a column and transform them to their own columns;
#will use this to find difference in obesity between two income groups
library("maps")
#allows us to project data onto a map
library("mapproj")
#converts latitude/longitude into projected coordinates to improve map distortions
library("fiftystater")
#allows us to display Alaska and Hawaii in visually appealing way too, not just continguous 48 states


#DATA CLEANING

surveydata <- read.csv(file.choose())
#choose file from pop-up
#original data source: https://chronicdata.cdc.gov/api/views/hn4x-zwk7/rows.csv?accessType=DOWNLOAD

summary(surveydata)
#view summary of dataset to see all questions, demographic groups, and the number of instances of each

View(surveydata)
#confirm data imported properly

surveydata$data_value_diff <- surveydata$Data_Value -surveydata$Data_Value_Alt
#check if there is any difference between the values in Data_Value and Data_Value_Alt
unique(surveydata$data_value_diff)
#confirm the difference between the two values is always 0
surveydata$Data_Value_Alt <- NULL
surveydata$data_value_diff <- NULL
#delete the unnecessary repetitive column and the column with the difference to simplify dataset
View(surveydata)
#confirm columns deleted

unique(surveydata$Data_Value_Unit)
unique(surveydata$Data_Value_Footnote)
unique(surveydata$Data_Value_Footnote_Symbol)
unique(surveydata$Data_Value_Type)
#check if these fields have a different value anywhere

surveydata$Data_Value_Type <- NULL
surveydata$Data_Value_Unit <- NULL
surveydata$Data_Value_Footnote <- NULL
surveydata$Data_Value_Footnote_Symbol <- NULL
#they do not, and do not provided necessary information for this analysis, so delete to simplify dataset

View(surveydata)
#confirm deleted

surveydata$year_start_end_diff <- surveydata$YearEnd - surveydata$YearStart
#check if there is any difference between the values of Year Start and Year End
unique(surveydata$year_start_end_diff)
#confirm the difference is always 0
surveydata$YearEnd <- NULL
surveydata$year_start_end_diff <- NULL
#delete Year End column and column with difference to simplify dataset
names(surveydata)[names(surveydata) == 'YearStart'] <- 'Year'
#rename Year Start column to Year for simplicity in future parts of script

surveydata$Datasource <- NULL
surveydata$Class <- NULL
surveydata$Topic <- NULL
surveydata$ClassID <- NULL
surveydata$TopicID <- NULL
surveydata$QuestionID <- NULL
surveydata$DataValueTypeID <- NULL
surveydata$LocationID <- NULL
surveydata$StratificationCategoryId1 <- NULL
surveydata$StratificationID1 <- NULL
surveydata$StratificationID1 <- NULL
surveydata$Low_Confidence_Limit <- NULL
surveydata$High_Confidence_Limit <- NULL
#delete unnecessary columns that will not be used in analysis, to simplify dataset

surveydata <- surveydata[!(surveydata$Income %in% c("Data not reported")),]
surveydata <- surveydata[!(surveydata$Income %in% c(NA)),]
#delete rows where data was not reported or NA so R functions can run properly

surveydata$Income<-as.character(surveydata$Income)
surveydata$Income[surveydata$Income=="Less than $15,000"] <- "$0 - $15,000"
#change this income value to start with $0 so by default it will display in order, being the lowest income group

obesity <- subset(surveydata,Question=="Percent of adults aged 18 years and older who have obesity")
#create subset that only looks at the obesity rate question, not the behavioral questions
names(obesity)[names(obesity) == 'Data_Value'] <- 'Obesity_Rate'
#since we are now only looking at the obesity question, Data_Value is always obesity rate; rename for clarity

View(obesity)
#confirm complete

obesity<-obesity[!(obesity$LocationDesc=="Puerto Rico" | obesity$LocationDesc=="Guam"),]
#client does not work in PR or Guam, so removing those rows
#PR also has less than 1/2 avg income of poorest state, so the income grouping we will analyze is not appropriate for it

obesity_income <- subset(obesity, StratificationCategory1=="Income", select=c(Year,LocationDesc,LocationAbbr,Obesity_Rate,Income))
#subset obesity data to only look at income groups, and exclude rows with other groupings like gender

box_plot <- ggplot(data = obesity_income,
                   aes(x = Income,y = Obesity_Rate,fill=Income)) +
  geom_boxplot() + ylab("Obesity Percentage") +
  ggtitle("Box Plot for Income vs. Obesity Rates") +
  expand_limits(y = 0) +
  scale_y_continuous(labels = function(x){ paste0(x, "%") }) +
  theme(plot.title = element_text(hjust = 0.5, size=18, face="bold"),
        axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))

box_plot
#box plot as part of our data cleaning and processing allows us to check for data anomalies
#can see max and min (end of vertical lines) and outliers (dots past vertical lines)
#and whether the median (horiztontal line) is approximately in the middle of the box (if not, shows that distribution of dataset is skewed)
#there are outlying dots in the highest income group where obesity rate is less that 15, so we will take a closer look at those

outliers <- subset(obesity_income, Obesity_Rate<15)
#boxplot showed outlying dots in highest income group where obesity rate was lower than 15
#create a subset to investigate these outliers
#outliers are all DC, 2011 - 2015. DC is the only exclusively urban location in dataset; will keep this in mind for analysis


#DATA ANALYSIS AND VISUALIZATIONS


#PLOT 1 - OBESITY RATE PER STATE IN 2015
#By using a state map, we can start our analysis by looking for regional trends in obesity rates

obesity_states_2015 <- subset(obesity, Stratification1=="Total" & Year=="2015" & LocationDesc!="National")
#Use dataset that looks at Total population (not subcategories like income) for most recent year available
#Only states will be mapped, so remove National

names(obesity_states_2015)[names(obesity_states_2015) == 'LocationDesc'] <- 'state'
#Change "LocationDesc" column name to "state" for clarity and ease of use

obesity_states_2015$statelower <- tolower(obesity_states_2015$state)
#Change state names to lowercase so map function are properly able to identify the state in the dataset

obesity2015_map <- ggplot(obesity_states_2015, aes(map_id = statelower)) +
  geom_map(aes(fill = Obesity_Rate), map = fifty_states) +
  #from package; fifty US states and D.C. with Alaska and Hawaii as insets
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  borders("state") +
  #add borders to see states more clearly
  coord_map() +
  #improve map distortion
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  #no axis breaks nor labels
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        plot.title=element_text(hjust = 0.5, size=18, face="bold")) +
        #center and increase size of title
  fifty_states_inset_boxes() +
  #add box around Alaska and Hawaii so it's clearer that they're shown out of true geographical place
  scale_fill_gradient(low ='rosybrown1', high ='red3') +
  #color scale for lower to higher obesity rates
  labs(title="Obesity Rate per State in 2015",fill="Obesity Rate")
  #map and legend titles

obesity2015_map
#Display map


#PLOT 2 - OBESITY PER INCOME GROUP OVER TIME

#Line Graph to show both the difference in level of obesity across income groups
#And show the increase over time in all income groups
#This shows the problem is getting worse in all income groups, and that the problem affects the poor the most

obesity_income_states <- obesity_income[!(obesity_income$LocationDesc=="National"),]
#remove national average, so this subset only looks at the states

agg_obesity_income <-aggregate(obesity_income_states$Obesity_Rate,
                               by=list(Year=obesity_income_states$Year, Income=obesity_income_states$Income), mean)
#aggregate obesity across all states per year

names(agg_obesity_income)[names(agg_obesity_income) == 'x'] <- 'Obesity_Rate'
#rename aggregated obesity rate from 'x' (automatic from aggregate function) to Obesity_Rate for clarity

incomegroup_linegraph <- ggplot(data = agg_obesity_income, aes(x = Year, y = Obesity_Rate, color = Income)) +
  geom_line(aes(group = Income), size=1.25) +
  #create a line for each income group, and increase thickness of lines so they are easier to see
  geom_point(size=3) +
  #increase size of points on line, so they are still visible after line thickness increase
  ylab("Rate of Obesity") +
  #relabel y axis (x axis label is already appropriate by default)
  ggtitle("Obesity Rates of Each Income Category from 2011 - 2015") +
  #add title to plot
  scale_y_continuous(labels = function(x){ paste0(x, "%") }) +
  #add % sign to values on the y axis, to make it easier to understand
  expand_limits(y = 0) +
  #start y axis at 0, to avoid misleading proportions of difference
  #expanding limits made all information on the plot seem small in comparison to all the empty spaces
  #so, style changes increasing size of elements are to compensate for this and make it look substantial despite the empty space
  theme(legend.justification = "top",
        #move legend to top of right side, to line up with respective lines for easier reading
        legend.text=element_text(size=11),
        #increase size of legend text to make it easier to read
        plot.title = element_text(hjust = 0.5, size=18, face="bold"),
        #center plot title (default is left aligned)
        axis.text=element_text(size=12),
        #increase size of values of axes for easier reading
        axis.title=element_text(size=12,face="bold"))
#increase size and bold titles of axes for easier reading

incomegroup_linegraph
#display

#SAME PLOT, ZOOM IN
#Match formatting of previous plot for continuity, but remove expand_limits that set y to 0. By default R shows a more zoomed-in version

incomegroup_linegraph_zoom <- ggplot(data = agg_obesity_income, aes(x = Year, y = Obesity_Rate, color = Income)) +
  geom_line(aes(group = Income), size=1.25) +
  geom_point(size=3) +
  ylab("Rate of Obesity") +
  ggtitle("Obesity Rates of Each Income Category from 2011 - 2015") +
  scale_y_continuous(labels = function(x){ paste0(x, "%") }) +
  theme(legend.justification = "top",
        legend.text=element_text(size=11),
        plot.title = element_text(hjust = 0.5, size=18, face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))

incomegroup_linegraph_zoom
#display zoomed in plot


#PLOT 3 - SHOW INCREASING GAP IN OBESITY BETWEEN HIGH AND LOW INCOME

#Using National value as location, show the increase over time in the difference between the lowest income group obesity rate and the highest income group obesity rate
#The disparity between low income obesity rates and high income obesity rates is increasing.

obesity_income_gap <- dcast(obesity_income,Year + LocationAbbr ~ Income, value.var="Obesity_Rate")
#reshape dataset so that income groups become their own columns

obesity_income_gap$Difference <- obesity_income_gap$"$0 - $15,000" - obesity_income_gap$"$75,000 or greater"
#add a new column that displays the difference between the obesity percentage in the lowest income group
#and the obesity percentage in the highest income group


obesity_income_gap_natl <- subset(obesity_income_gap,LocationAbbr=="US")

gap_increase <- ggplot(data = obesity_income_gap_natl, aes(x = Year, y = Difference)) + 
  geom_line(aes(group = LocationAbbr), size=1.25, color='darkblue') + 
  geom_point(size=3, color='darkblue') + 
  ylab("Gap in Obesity Rates") +
  labs(title="Difference in Obesity Rates between Highest and Lowest Income Groups - National") +
  expand_limits(y = 0) +
  scale_y_continuous(breaks=seq(0, 12, 1)) +
  theme(plot.title = element_text(hjust = 0.5, size=18, face="bold"),
        #center plot title (default is left aligned)
        axis.text=element_text(size=12),
        #increase size of values of axes for easier reading
        axis.title=element_text(size=12,face="bold"))
        #increase size and bold titles of axes for easier reading

gap_increase
#display graph


#PLOT 4 - SHOW STATES WITH HIGH OBESITY AND LARGE GAP BETWEEN HIGH AND LOW INCOME

#y axis is the % of obesity in lowest income group
#x axis is the difference between the obesity % of lowest income group and the obesity % of highest income group
#programs should be targeted at places with high obesity rates in low income, but especially in those areas where gap is greatest
#programs should be focused on areas in top right quadrant
#interesting outliers like DC (largest gap) and Kentucky (high obesity, but very small gap)

obesity_income_gap_2015 <- subset(obesity_income_gap, Year==2015 & LocationAbbr !="US")
#use most recent year values for difference between highest income and lowest income obesity percentage
#don't include US - not needed, and label overlapped with other labels when used in scatterplot, making it harder to read

names(obesity_income_gap_2015)[names(obesity_income_gap_2015) == '$0 - $15,000'] <- 'Lowest_Income_Obesity'

#working!
obesity_income_gap_scatterplot <- ggplot(obesity_income_gap_2015,
                                         aes(x=Difference, y=Lowest_Income_Obesity,
                                             label=LocationAbbr, color='darkred')) +
  guides(color=FALSE) +
  labs(title="Obesity Rates of Lowest Income Group and Gap in Obesity between Lowest and Highest Income Groups - 2015") +
  xlab("Gap in Obesity Between Lowest and Highest Income Groups") +
  ylab("Obesity Rates of Lowest Income Group") +
  geom_point(size=3) + 
  geom_text(aes(label=LocationAbbr),hjust=.5, vjust=-.5, size=6) +
  scale_y_continuous(labels = function(x){ paste0(x, "%") }) +
  theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"),
        #center plot title (default is left aligned)
        axis.text=element_text(size=12),
        #increase size of values of axes for easier reading
        axis.title=element_text(size=12,face="bold"))
#add % sign to values on the y axis, to make it easier to understand

#Tried installing package ggrepel to avoid the few overlapping data labels
#However it put the labels on all different sides of the points, which changed the shape of the data visualization, and made it harder to spot trends and outliers
#Used state abbreviations instead to improve the issue

obesity_income_gap_scatterplot
#view scatterplot
