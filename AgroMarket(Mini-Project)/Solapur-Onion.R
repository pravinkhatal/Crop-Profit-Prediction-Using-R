SolapurMarketPriceSheetOfOnion<-read.csv("Solapur-Onion.csv")
SolapurMarketPriceSheetOfOnion

MOdelOfSolapurforOnion <- lm(formula = ModePrice~Quantity+MaxPrice+MinPrice,data = SolapurMarketPriceSheetOfOnion)
MOdelOfSolapurforOnion

summary(MOdelOfSolapurforOnion)

intersectOfSolapurforOnion <- summary(MOdelOfSolapurforOnion)$coefficients[1, 1]
intersectOfSolapurforOnion

coefficientOfQuantityOfSolapurforOnion <- summary(MOdelOfSolapurforOnion)$coefficients[2, 1]
coefficientOfMaxPriceOfSolapurforOnion <- summary(MOdelOfSolapurforOnion)$coefficients[3, 1]
coefficientOfMinPriceOfSolapurforOnion <- summary(MOdelOfSolapurforOnion)$coefficients[4, 1]


Quantity<-c(SolapurMarketPriceSheetOfOnion$Quantity)
MaxPrice<-c(SolapurMarketPriceSheetOfOnion$MaxPrice)
MinPrice<-c(SolapurMarketPriceSheetOfOnion$MinPrice)



price <- intersectOfSolapurforOnion + coefficientOfQuantityOfSolapurforOnion * Quantity[1] +coefficientOfMinPriceOfSolapurforOnion * MinPrice[1] + coefficientOfMaxPriceOfSolapurforOnion * MaxPrice[1]
price

SolapurMarketPriceforOnion <- c(intersectOfSolapurforOnion, coefficientOfQuantityOfSolapurforOnion,coefficientOfMinPriceOfSolapurforOnion,coefficientOfMaxPriceOfSolapurforOnion)

