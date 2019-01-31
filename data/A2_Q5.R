#Assignment 2 of Computational Statistics
houseprices = read.csv("RealEstate.csv")

#Fitting linear model to predict price
price_pred = lm(Price ~. , data = houseprices)