# Create data frame "obesity" from CSV file w/headers
obesity <- read.csv("C:/Users/mgiezeman/Desktop/obesity.csv", header = T)

#START DATA CLEANUP (rows 4-65; **jump to row 67 to see draft R script work due 11.14.17**)

#check if the columns YearStart and YearEnd are the same (find the difference)
obesity$year_start_end_diff<-obesity$YearEnd - obesity$YearStart

#display the unique values of the difference; see there is only 0 difference
unique(obesity$year_start_end_diff)

#delete column YearEnd
obesity$YearEnd<-NULL

#rename column YearStart as Year
names(obesity)[names(obesity)=='YearStart']<-'Year'

#delete column LocationAbbr, as we only need LocationDesc that has the full state name
obesity$LocationAbbr<-NULL

#delete column Datasource. We know all entries are the same, and that our data source is the survey
obesity$Datasource<-NULL

#delete column Class. Class had three sub-categories (Fruits and vegetables, Obesity/Weight Status, Physical Activity) since we are not interested in Class, we are interested in specific questions.
obesity$Class<-NULL

#delete column Topic. Topic had three sub-categories (Fruits and vegetables, Obesity/Weight Status, Physical Activity) since we are not interested in Topic, we are interested in specific questions.
obesity$Topic<-NULL

#check if the values are same for Data_Value and Data_Value_Alt. Get difference, then view unique values.
obesity$data_value_diff<-obesity$Data_Value - obesity$Data_Value_Alt
unique(obesity$data_value_diff)

#values are all the same, so delete Data_Value_Alt
obesity$Data_Value_Alt<-NULL

#check if there is any data other than "Value" in Data_Value_Type
unique(obesity$Data_Value_Type)

#there is no data other than "Value" in Data_Value_Type, so delete column
obesity$Data_Value_Type<-NULL

#check if there is any useful data in columns Data_Value_Unit, Data_Value_Footnote, and Data_Value_Footnote_Symbol
unique(obesity$Data_Value_Unit)
unique(obesity$Data_Value_Footnote)
unique(obesity$Data_Value_Footnote_Symbol)

#there is nothing we will use, so we will delete these 3 columns
obesity$Data_Value_Unit<-NULL
obesity$Data_Value_Footnote<-NULL
obesity$Data_Value_Footnote_Symbol<-NULL

#delete all of the following columns since we are not going to use them for our analyses, and they are repetition of other entries: GeoLocation, ClassID, TopicID, QuestionID, DataValueTypeID, LocationID
obesity$GeoLocation <- NULL
obesity$ClassID <- NULL
obesity$TopicID <- NULL
obesity$QuestionID <- NULL
obesity$DataValueTypeID <- NULL
obesity$LocationID <- NULL

#delete columns created for this cleanup process
obesity$year_start_end_diff <- NULL
obesity$data_value_diff <- NULL

#END DATA CLEANUP

#START DRAFT R SCRIPT

#Get a subset of rows where the Question is % obesity, as this is the main indicator for our analyses
percentobesity <- subset(obesity,Question=="Percent of adults aged 18 years and older who have obesity")

#find the highest, lowest, median, and mean in % obesity, to get a general sense of how wide-ranging the values are across different categories, and understand which values represent categories with above-average and below-average rates of obesity
summary(percentobesity$Data_Value)

#Make a histogram of this subset to visualize where most categories fall, and the highs and lows
hist(percentobesity$Data_Value,
     main="Histogram of % Obesity in Stratifications")

#get a subset of obesity rates for the most recent year, 2015, so we can analyze the most up-to-date data. Only select columns showing state and subset of people.
obesity2015 <-subset(percentobesity,Year==2015,select=c(LocationDesc,Data_Value,Stratification1,StratificationCategory1))

#find which 20 categories have the highest rates of obesity in 2015
head(obesity2015[order(obesity2015$Data_Value,decreasing=T),],n=20)

#find which 20 categories have the lowest rates of obesity in 2015
head(obesity2015[order(obesity2015$Data_Value,decreasing=F),],n=20)

#we want to look at how income in particular correlates with obesity rates, so we create a subset that only includes income categories, not including rows where Income was not reported.
obesity2015income <- subset(obesity2015,StratificationCategory1=='Income' & !(Stratification1=="Data not reported"))

#in the income subset, get the highest rate of obesity per state 
obesity2015income.agg<-aggregate(Data_Value ~ LocationDesc,obesity2015income,max)
#merge with original to see the income category of that highest rate
obesity2015income.max<-merge(obesity2015income.agg,obesity2015income)

#in the income subset, get the lowest rate of obesity per state
obesity2015income.agg2<-aggregate(Data_Value ~ LocationDesc,obesity2015income,min)
#merge with original to see the income category of that lowest rate
obesity2015income.min<-merge(obesity2015income.agg2,obesity2015income)

#we realized "National" was still a value for state, and we need to remove it, since the data is represented elsewhere.
obesity2015income<-obesity2015income[!(obesity2015income$LocationDesc=="National"),]

#look at relationship between obesity value and income, to determine whether obesity rates rise as income level falls, and guide how obesity-prevention efforts should be targeted
regObesityIncome<-lm(Data_Value ~ Stratification1,obesity2015income)

#View regression
regObesityIncome

#plot regression to see relationship between income and obesity
plot(regObesityIncome)

#summary to see relationship between income and obesity
summary(regObesityIncome)

#add a new column with a numerical value (1-6) to represent income level, so we can perform analysis more clearly
obesity2015income$incomelevel1<-ifelse(obesity2015income$Stratification1=="Less than $15,000",1,
                                      ifelse(obesity2015income$Stratification1=="$15,000 - $24,999",2,
                                             ifelse(obesity2015income$Stratification1=="$25,000 - $34,999",3,
                                                    ifelse(obesity2015income$Stratification1=="$35,000 - $49,999",4,
                                                           ifelse(obesity2015income$Stratification1=="$50,000 - $74,999",5,
                                                                  6)))))

#replace regression with one done with numerical value
regObesityIncome<-lm(Data_Value ~ incomelevel1,obesity2015income)
#view new regression
regObesityIncome
#plot regression to see relationship between income and obesity
plot(regObesityIncome)
#summary
summary(regObesityIncome)
