# Modeling Home Prices (R)
## Regression Analysis of Boston Home Prices with R 

(Project can be found at: http://www.inertia7.com/projects/regression-boston-housing-r)

# Table of Contents 
* [Abstract](#Abstract)
* [Contributors](#Contributors)
* [Packages Required](#Packages-Required)
* [Steps Required](#Steps-Required)


## <a name="Abstract"></a> Abstract
This project focuses on finding the best linear regression model to estimate the median value (in $1000's) of owner-occupied homes in the Suburbs of Boston.

Understanding house prices (and related variables) are relevant to economists and policy-makers when allocating budgets for domestic purposes. Linear regression is a fundamental concept in statistical-learning, from which other more sophisticated methods arise. Although this may not be the most precise, the statistical foundation in which this procedure is built on is worth exploring.

For this project we leverage the power of R for both the statistical computations as well as the visualizations.

The data used in this project is available at the UCI Machine Learning Repository. This modeling problem is referenced in the 2014 version of the book "An Introduction to Statistical Learning" by Gareth Games, Daniela Witten, Trevor Hastie, and Robert Tibshinari. 

## <a name="Contributors"></a> Contributors

- David A. Campos
- Raul Eulogio
- Amil Khan

## <a name="Packages-Required"></a> Packages Required
Here are the required packages which will ensure the script will run smoothly:

	usdm
	car
	MASS
	DAAG
	lmtest
	ggplot2
	ggfortify
	GGally

To make sure you have the packages we use in this project use the command(you will only have to use this once): 

	install.packages("packageName") 

You will have now downloaded the package so within your script you run: 

	require(packageName)

This must be done before each **Rstudio** session, and written at the start of every script to ensure your code will be easily reproducible!

## <a name="Steps-Required"></a>Steps Required 

### Create plotly Account (Optional)	
If you would like to have the images you create (using **plotly** and **ggplot2**) published so that you can customise the plots to your liking or brag about the interactivety of your visuals simply create a [plolty account](https://plot.ly/). Once you do so you will have access to your username and more importantly your API key, these will be necessary to publishing your plots (If you do not wish to publish your plots skip this step). 

### Using Plotly account in Rstudio session
Important to note, when posting on GitHub never publish API keys (this is a common mistake I see people do). Once you gain access to your API key, have **plotly** in your current working directory, you run:

	Sys.setenv("plotly_username"="userName")
	Sys.setenv("plotly_api_key"="d1X4Hrmbe")

From here you will be able to publish your **ggplotly** visuals by running (our **ggplot2** object is called linearRegressionPlot for this example):

	plotly_POST(linearRegressionPlot, filename = "linearRegressionPlot")

If ran correctly this line of code should open up a browser with your newly published **plotly** graph!
### Create appropriate working directory
Once the preliminary process of ensure your **Rstudio** has all parameters to ensure the code will run smoothly we suggest create an appropriate directory. For those using git we recommend using the  following line on a terminal:

	git clone git@github.com:wH4teVr folder-name

But if you are doing it manually you choose the "Clone or download" button and choose "Download ZIP". From here you must take note of where the file is downloaded, once you are able to find the file location you must set the appropriate working directory. 

For this example we set the file into "/user/home/myProjects/timeSeriesR" so recall we have to set the working directory in **Rstudio** or you will receive errors especially when trying to read in the csv file. Therefore you run at the stop of your script:
For linux:

	setwd("/user/home/myProjects/linearRegressionR")

For windows(Important when finding directorys you will have):

	C:\home\myDocuments\myProjects\linearRegressionR

Which will give you an error (since R considers "\" as an escape code, the correct form is:

	setwd("C:/home/myDocuments/myProjects/linearRegressionR")

Once you have done this you can read the csv file which contains the S&P 500 closing values for which we did our analysis on, and you can proceed to the time series analysis done through R!
