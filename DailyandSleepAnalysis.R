##setup the work directory where the csv files are stored
setwd("/Fitabase Data 4.12.16-5.12.16")

##Loading the files
EverydayActs <- read.csv("dailyActivity_merged.csv")
Sleepday <- read.csv("sleepDay_merged.csv")

## scanning the data
EverydayActs %>% count(Id)
Sleepday %>% count(Id)

## check data info
str(EverydayActs)
str(Sleepday)

##Removing null values from the data set
EverydayActs <- na.omit(EverydayActs)
Sleepday <- na.omit(Sleepday)

##check for the missing values
which(is.na(EverydayActs))
which(is.na(Sleepday))

##Check Unique values
n_distinct(EverydayActs$Id)
n_distinct(Sleepday$Id)

ggplot(data = EverydayActs, aes(x = TotalSteps, y = Calories))+geom_point()+geom_smooth(colour = "purple")+ labs(title = 'Calories vs. Total Steps', x = 'Total Steps', y = 'Calories')
cor(EverydayActs$TotalSteps, EverydayActs$Calories, method=c("pearson", "kendall", "spearman"))

ggplot(data = Sleepday, aes(x = TotalMinutesAsleep, y = TotalTimeInBed))+geom_point()+geom_smooth(colour = "purple")+ labs(title = 'Total Time In Bed vs. Total Minutes Asleep', x = 'Total Minutes Asleep', y = 'Total Time In Bed')
cor(Sleepday$TotalMinutesAsleep, Sleepday$TotalTimeInBed, method=c("pearson", "kendall", "spearman"))

combineData <- merge(Sleepday, EverydayActs, all = TRUE)
colnames(combineData)
which(is.na(combineData))
combineData <- na.omit(combineData)

ggplot(data = combineData, aes(x = TotalMinutesAsleep, y = TotalSteps))+geom_point()+geom_smooth(colour = "purple")+ labs(title = 'Total Steps vs. Total Minutes Asleep', x = 'Total Minutes Asleep', y = 'Total Steps')
cor(combineData$TotalSteps, combineData$TotalMinutesAsleep, method=c("pearson", "kendall", "spearman"))


Sedentary <- sum(combineData$SedentaryMinutes)
Active <- sum(combineData$VeryActiveMinutes)
Fairly <- sum(combineData$FairlyActiveMinutes)
Lightly <- sum(combineData$LightlyActiveMinutes)

x <- c(Sedentary,Lightly,Fairly,Active)
labels <- c("Sedentary", "Lightly Active", "Fairly Active", "Very Active")
lab <- paste0(round(x/sum(x) * 100, 2), "%")
col <- hcl.colors(length(x), "Spectral")
pie3D(x,main = "Active Minutes Of Users (in Percent)", col = hcl.colors(length(x), "Spectral"),shade = 0.5,theta = 1.5, labels = lab)
legend("topright",c("Sedentary", "Lightly Active", "Fairly Active", "Very Active"),cex = 0.7, fill = col)
