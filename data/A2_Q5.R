#Assignment 2 of Computational Statistics
houseprices = read.csv("RealEstate.csv")

#Fitting linear model to predict price
price_pred = lm(Price ~. , data = houseprices)
summary(price_pred)

#Short Sale case
ssale <- houseprices[ which(houseprices$Status == 'Short Sale'), ]
price_pred1 = lm(Price ~ Bedrooms + Bathrooms + Size + Price.SQ.Ft + Location + MLS , data = ssale)
summary(price_pred1)

#Foreclosure case
foreclosure <- houseprices[ which(houseprices$Status == 'Foreclosure'), ]
price_pred2 = lm(Price ~ Bedrooms + Bathrooms + Size + Price.SQ.Ft + Location + MLS , data = foreclosure)
summary(price_pred2)

#Regular Case
regular <- houseprices[ which(houseprices$Status == 'Regular'), ]
price_pred1 = lm(Price ~ Bedrooms + Bathrooms + Size + Price.SQ.Ft + Location + MLS , data = regular)
summary(price_pred1)
