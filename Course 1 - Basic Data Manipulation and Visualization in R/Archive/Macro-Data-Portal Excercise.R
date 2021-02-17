
# Problem 3: Basic Statistical Analysis using the Macro Data Portal --------------------------
# Open a browser and go to the Macro Data Portal Website: https://mepd.shinyapps.io/Macro-Data-Portal/
# The dataset shown is the total fiscal operations, Updated to October 2020
# Click on the 'Variables' tab, and examine the variables. This should look familiar to you from the GFS excel sheet.
# Look at the 'Class' column. Do you recognize the R type and in brackets behind the storage type of the variables?

# Now on your left where is says 'Select Variables', select all the time variables (Date-Month)
# and additionally the variables 'REV_TOT', 'REV_URA', 'REV_NON_URA', 'GRA_TOT'. Click 'Apply Selection'
# Go to the 'Summary' tab and briefly examine the statistics of the data.
# Now go to the plot window. What do you notice? Can you see the impact of COVID-19?
# On the top left you can click, on 'Joint' to see all series in one chart, then go back to 'Individual Series'. 
# We will look at this a bit more closely. Go back to the 'Table' tab and hit the 'Filter Data' checkbox.
# Click on the blue question mark and briefly comprehend the message. 
# Now you will only consider data since 2017. Type 'Year >= 2017' in the filter query box and hit 'Apply Filter'. 
# Go back to the 'Plot' tab. You should now only see data from 2017 in the plot. 
# To more clearly see the impact of COVID-19 on revenue, we will get rid of some seasonality in the plot. 
# On your left there is a checkbox called 'Transform Data'. Hit that checkbox. 
# In the field 'Data Transformations' scroll down until you find a filter called 'cubic spline trend (very soft)'.
# At the bottom hit the checkbox 'Add Transformed Variables' and click the 'Transform' button. 
# You should now see two lines in each chart, the original series in red and a smooth trend in green. 
# The trend can give you an idea of what the impact of COVID-19 on revenue could have been. 
# Now we will use this trend to seasonally adjust the data. Therefore click on 'cubic spline trend (very soft)'
# and remove it by hitting delete or backspace on your keyboard. Now scroll down all the way to the bottom of the
# list of filters and hit 'Seasonal adj. on cubic spline (very soft)'. 
# What this will do is subtract the smooth trend from the data, then regress the residuals on a series of monthly dummy
# variables, and add the residuals from the regression back to the smooth trend. 
# Examine this seasonally adjusted data: In which month was the greatest impact on URA revenue? (you may want to use the 'Table' tab to also look at the numbers.)

# Finally, we will aggregate this data to quarterly frequency and look at this again.
# To do this, first go back to the 'Table' tab. This data runs up to October 2020, which is part of the new quarter. 
# Let's remove this value by changing the filter query to 'Year >= 2017 & Date < "2020-10-01"'. 
# Now further down hit the 'Aggregate' checkbox. Select 'Year' and 'Quarter' (or alternatively 'FY' and 'QFY') as aggregator ID variables
# In the 'Aggregator Functions' box, remove the mean and select the sum. Then hit 'Aggregate'. 
# Go back to the 'Plot' window. In which Quarter of 2020 was the greatest revenue shortfall?. 

# Finally, you will download this data and open it in R. Go back to the 'Table' tab and at the bottom left of the page hit
# 'R' and then download. Your browser should then bring up a window where you can save of choose to open the file. 
# Click on 'Open with', and then search for 'Rstudio' as the program to open the file and hit OK. 
# Rstudio should now open and you will be requested to give a name to the dataset, the default being 'Overall Fiscal Operations'. 
# Change this to 'REV' and hit OK. You should now see the 'REV' dataset in your global environment, along with 
# a line of code printed in the console that imported your data. Click on 'REV' in the global environment to view the data in Rstudio.
# summarize the dat aby typing summary(REV) into the console.

# That's it, you survived the first session and homework assignment. We will be taking off from here and doing some analysis
# ourself in R after the next session. 



