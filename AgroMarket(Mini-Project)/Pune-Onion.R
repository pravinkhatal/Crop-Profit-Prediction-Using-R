PuneMarketPriceSheetOfOnion<-read.csv("Pune-Onion.csv")
PuneMarketPriceSheetOfOnion

MOdelOfPuneforOnion <- lm(formula = ModePrice~Quantity+MaxPrice+MinPrice,data = PuneMarketPriceSheetOfOnion)
MOdelOfPuneforOnion

summary(MOdelOfPuneforOnion)

intersectOfPuneforOnion <- summary(MOdelOfPuneforOnion)$coefficients[1, 1]
intersectOfPuneforOnion

coefficientOfQuantityOfPuneforOnion <- summary(MOdelOfPuneforOnion)$coefficients[2, 1]
coefficientOfMaxPriceOfPuneforOnion <- summary(MOdelOfPuneforOnion)$coefficients[3, 1]
coefficientOfMinPriceOfPuneforOnion <- summary(MOdelOfPuneforOnion)$coefficients[4, 1]


Quantity<-c(PuneMarketPriceSheetOfOnion$Quantity)
MaxPrice<-c(PuneMarketPriceSheetOfOnion$MaxPrice)
MinPrice<-c(PuneMarketPriceSheetOfOnion$MinPrice)



price <- intersectOfPuneforOnion + coefficientOfQuantityOfPuneforOnion * Quantity[1] +coefficientOfMinPriceOfPuneforOnion * MinPrice[1] + coefficientOfMaxPriceOfPuneforOnion * MaxPrice[1]
price

PuneMarketPriceforOnion <- c(intersectOfPuneforOnion, coefficientOfQuantityOfPuneforOnion,coefficientOfMinPriceOfPuneforOnion,coefficientOfMaxPriceOfPuneforOnion)

