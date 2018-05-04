# GoogleAnalytics
Visualize and predict SiteRevenue in GoogleAnalytics data using R. 

# Data
Monthly Data was taken from Google Analytics report exports for the period of Aug 2017 to Apr 2018. 

Data Size: ~15Mb

# Approach

- Read all GoogleAnalytics data files and put the data into a dataframe. 
- Get Lat Long coordinates of each city from 'uscitiesv1.4.csv' data (merge). Cities in 2 different states with same name will be considered as one city. To avoid this, add state in google analytics data and merge by city and state
- Filter out observations without Lat and long values.
- Prepare data for analysis.
- Basic Visualizations
- Cool animated viz of Monthly Change & Cumulative Revenue using gganimate.

Monthly Change in Revenue by Region

![Monthly Change in Revenue by Region](https://github.com/RaghavRajaram/GoogleAnalytics/blob/master/MonthlyChange.gif)

Monthly Cumulative Revenue by Region

![Monthly Cumulative Revenue by Region](https://github.com/RaghavRajaram/GoogleAnalytics/blob/master/MonthlyCumulativeChange.gif)

- Perform PCA on features.

Scree Plot

![Scree](https://github.com/RaghavRajaram/GoogleAnalytics/blob/master/Scree.PNG)

Cumulative Scree Plot

![Scree](https://github.com/RaghavRajaram/GoogleAnalytics/blob/master/CumulativeScree.PNG)

- Scree plots suggest that 8 components explain ~98% of variation in data. Limit prinicipal components to 8.
- Create train and test data. Perform k-fold cross validation to identify best model (from KKNN, MARS & NNET) for predictions.
- Identify best model to predict Revenue (KKNN). Mean error of 51.84. Mean of Revenue = 862.96. Median of Revenue = 149