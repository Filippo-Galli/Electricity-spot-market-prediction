# Introduction 
The Italian electricity market operates through auctions where buyers and producers submit their bids and offers. Our method examines each individual bid and offer within these auctions, enabling us to construct hourly purchase and sale curves. The market equilibrium, where these two curves intersect, establishes a uniform price for both sales and purchases. This intersection point defines the market clearing price and the corresponding quantity. We suggest modeling these hourly curves with functional models. This approach is particularly suitable because purchase and sale curves, typically represented by thousands of data points, are best understood as varying continuously. The data is provided by Gestore Mercati Energetici (GME).

# Our  Goal 
Do explorative analysis on this data and try to create an hour a-head predictor of the equilibrium price. 

# What we have done
We have done a lot of explorative analysis on the data, we have created a prediction model that is able to predict the equilibrium price with a MAPE of 0.1. We have also created a market simulator that is able to simulate the market and see how the price changes when a offer and bid are insert from the form.

# What this repo contains? 
## R Scripts
This repo contains 3 R file: 
- `xml_to_csv.R`: this is the file used to extract data from xml format and create csv. To use it you need to create a _data_ folder in which save all xml file and a folder _csv_ where the result are saved. 
- `progetto.R`: this is the file used to do the explorative analysis and the prediction, it is a bit messy but it works very well 
- `main.R`: this file the clean version of our code, it does some exploratory analysis and also create and test our prediction model. 
- `market_simulator.R`: this script is used to simulate the market. After creating a Microsoft Forms connected to your OneDrive, you can use this script to simulate the market and see how the price changes  when a offer and bid are insert from the form.
## Folders
- `data`: this folder contains all the xml file used to create the csv file (need to be created)
- `csv`: this folder contains all the csv file created by the `xml_to_csv.R` script (need to be created)
- `slides`: this folder contains the slides used to keep up to date our tutor during this semester
- `img`: this folder contains all the image used in the slides and poster 

## Other files
- `MarketCoupling.txt`: this file contains the term that we need to add to the equilibrium price to have the correct price in a real world scenario.
- `test_results.csv`: this file contains the results of the test of our prediction model.

# What we have done in main.R

Our main.R script is divided in 4 main parts:
- Data exploration: in this part we have done some explorative analysis on the data, we have created some plots to understand the data better.
- smoothing: in this part we have created a smoothing function that is able to smooth the data and create a continuous curve. In this way we have a common "reference system" for our curves.
- fPCA: we do Functional Principal Components Analysis to understand the main components of the data and to see if we can use them to predict the equilibrium price using scores associated with the first 10 principal components.
- Prediction: we have created a prediction model using ARIMA to predict scores of first 10 principal component and then reconstruct our curves and the equilibrium point.

## Results
Our model has a mean percentage error of 10% for the price variable and 5% for the quantity variable.

## Note 
The correct price in a real world scenario has a little modification since from european regulation we need a term which is found from `MarketCoupling.txt` but which we doens't predict.


# How to use market_simulator.R
To use the market simulator you need to create a Microsoft Forms connected to your OneDrive. The form must have 3 questions:
- The first question must be if you want to do an offer or a bid; 
- The second question must be the quantity of energy you want to offer/bid;
- The third question must be the price you want to offer/bid.

After creating the form you need to saved on your OneDrive the Excel file with the responses of the form. After doing this you can select from the script the path on your OneDrive where the Excel file is saved and run the script. It automatically fetch data avery 5 seconds and update the market curves. 
There is also the option to fetch data from a local file, choose the option that best suits your needs and comment/uncomment the correct line into `market_simulator.R` script.


