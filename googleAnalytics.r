
library(zoo)
library(ggplot2)
library(gganimate)
library(chron)
library(nnet)
library(kknn)
library(earth)
library(dummies)

# Create an empty dataframe
df = data.frame()


# Read only csv files in directory (except the lat long file) and append them to df.
for (i in list.files())
{
  index = which(strsplit(i, "")[[1]]==".")
  if (substr(i,index+1,nchar(i)) == 'csv'){
    dfTemp = read.csv(i,skip = 6)
    df = rbind(df,dfTemp)
  }
}


# Remove temp df
rm(dfTemp)

# Read Lat Long File
latLongDf = read.csv('uscitiesv1.4.csv')

# Take required columns
latLongDf = latLongDf[,c('city','lat','lng')]

# Change column names to merge df and Lat Long df.
colnames(latLongDf) = c('City','lat','long')

# Merge by city. Cities in 2 different states with same name will be considered as one city.
# To avoid this, add state in google analytics data and merge by city and state.
df = merge(x = df, y = latLongDf,by = 'City', all.x = TRUE)

# Remove Lat Long df
rm(latLongDf)

# Filter out observations without Lat and long values.
df = df[!is.na(df$lat),]


# Convert Date.Range to character to get year and month.
df$Date.Range = as.character(df$Date.Range)


df$year = substr(df$Date.Range,nchar(df$Date.Range)-4,nchar(df$Date.Range))

df$month = substr(df$Date.Range,1,3)

df$yearMonth = as.yearmon(paste(trimws(df$month),trimws(df$year)),"%b %Y")

df$year = as.factor(df$year)

df$month = as.factor(df$month)

# Remove Data Range
df$Date.Range = NULL

# Convert Revenue to numeric
df$Revenue = as.numeric(gsub(',','',substr(df$Revenue,2,nchar(as.character(df$Revenue)))))

# All Users segment is the addition of values by segment. Filter them out to avoid duplication.
df = df[df$Segment != 'All Users',]

# Filter out rows without Revenue
df = df[df$Revenue > 0,]

# Convert factor variables to numeric
df$Users = as.numeric(df$Users)
df$New.Users = as.numeric(df$New.Users)
df$Sessions = as.numeric(df$Sessions)
df$Transactions = as.numeric(df$Transactions)

# Convert average session duration to number
df$Avg..Session.Duration =  60 * 24 * as.numeric(times(df$Avg..Session.Duration))



# Basic Visualizations

# Revenue by segment
g = ggplot(df,aes(x = Segment, y = Revenue))
g +  geom_bar(stat='identity',fill = "#4db1da")

# We infer that 'Made a purchase' has high value of revenue, which makes sense.
     
# Histogram of Sessions
hist(df$Sessions)

# Aggregate revenue by lat long and year month to view monthly change in revenue by region.
revenueAgg = aggregate(Revenue ~ lat+long+yearMonth,df,FUN = sum)

# Get US map data.
usa = map_data("usa")


usaM = ggplot(data = usa) + 
  geom_polygon(aes(x = long, y = lat,group = group), fill = "gray", color = "black") +
  coord_fixed(1.3)


# Filter out regions that are outside US mainland.
revenueAgg = revenueAgg[(revenueAgg$lat >= 24.7433195 & revenueAgg$lat <= 49.3457868),]
revenueAgg = revenueAgg[(revenueAgg$long >= -124.7844079 & revenueAgg$long <= -66.9513812),]

# Use frame to capture the change in Revenue by month
map = usaM +
  geom_point(aes(x=long, y=lat,frame=revenueAgg$yearMonth), data=revenueAgg, col= "midnightblue", alpha=0.4, size=revenueAgg$Revenue*0.0001) + 
  scale_size_continuous(range=range(revenueAgg$Revenue))

# Animate plot to see the change.
gganimate(map, interval = 1)

# Output animation as a gif.
gganimate(map,interval = 1,"MonthlyChange.gif",title_frame =T,ani.width=800, ani.height=820, dpi=800)

# Output animation as a Mp4. Not very effective.
gganimate(map, "MonthlyChange.mp4", ani.width = 400, ani.height = 400,dpi=800, interval = 5)


# Can also plot cumulative revenue over months by region. Add cumulative = TRUE in the plot aes.
mapCumulative = usaM +
  geom_point(aes(x=long, y=lat,frame=revenueAgg$yearMonth,cumulative = TRUE), data=revenueAgg, col= "midnightblue", alpha=0.4, size=revenueAgg$Revenue*0.0001) + 
  scale_size_continuous(range=range(revenueAgg$Revenue))

# Animate cumulative map.
gganimate(mapCumulative, interval = 1)

gganimate(mapCumulative,interval = 1,"MonthlyCumulativeChange.gif",title_frame =T,ani.width=800, ani.height=820, dpi=800)


# Lets try to predict revenue with the data.
dfPrediction = df

# Remove bounce rate
dfPrediction$Bounce.Rate = NULL

# Take features like Segment, No. of Users, Sessions, No. of New users etc to predict revenue.
dfPrediction = dfPrediction[,2:9]

# Create dummies for factor variable Segment
dummyDf = dummy.data.frame(dfPrediction[,], names = c("Segment"))

# Split them into train and test data.
trainRows = sample(1:nrow(dummyDf),0.667*nrow(dummyDf))
train = dummyDf[trainRows,]
test = dummyDf[-trainRows,]

# Store dependent variable values
trainRevenue = train$Revenue
testRevenue = test$Revenue

# Delete dependent variable. 
train$Revenue = NULL
test$Revenue = NULL

# Perform PCA on train data to find out which features explain the variation in data.
prin_comp = prcomp(train, scale. = T)

# Std dev of pc
std_dev = prin_comp$sdev

# variance of pc
pr_var = std_dev^2

# proportion of variance explain to get scree plot
prop_varex = pr_var/sum(pr_var)

# Scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")


# Cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
# From scree plot, we see, 97-98% of explanation is explained by 8 variables.


# Add dependent variable to train and test
trainData = data.frame(Revenue = trainRevenue, prin_comp$x)

trainData = trainData[,1:9]

testData = predict(prin_comp, newdata = test)
testData = as.data.frame(testData)
testData = testData[,1:8]

testData$Revenue = testRevenue

# Perform k-fold validation in train to identify the best model from NNET, MARS and KKNN.
set.seed(1)
nFold= 10
# Randomly choose which fold each row is in
valNum= floor(runif(nrow(train))*nFold)+1
# Create a matrix where we store prediction error
modelPerformance= matrix(NA,nFold,3)



for(fold in 1:nFold){
  
  # Get the training and validation data for this fold
  trainingData= subset(trainData,valNum!=fold)
  validationData= subset(trainData,valNum==fold)
  
  # Estimate the model for this training data
  model1 = nnet(Revenue ~.,data=trainingData,linout=1,size = 1,maxit = 10000, skip = TRUE)
  model2 = earth(Revenue ~.,data=trainingData,trace=2,thres=0.1)
  model3 = kknn(Revenue ~ .,trainingData,validationData,k=1,distance = 1)
  
  
  # Calculate out of sample MSE for this validationData
  valid1 = mean(abs(validationData$Revenue -predict(model1,validationData)))
  valid2 = mean(abs(validationData$Revenue -predict(model2,validationData)))
  valid3 = mean(abs(validationData$Revenue -model3$fitted.values))
  
  # Store model performance
  modelPerformance[fold,] = c(valid1,valid2,valid3)
}

modelPerformance

# We see KKNN has the best performance

# Use KKNN model to train on entire train data and predict on test data.
kknnModel = kknn(Revenue ~ .,trainData,testData,k=1,distance = 1)

# Mean Error
diff = mean(abs(testData$Revenue -kknnModel$fitted.values))

diff


