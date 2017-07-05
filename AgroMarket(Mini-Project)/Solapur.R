SolapurMarketPriceSheetofWheat<-read.csv("Solapur.csv")
SolapurMarketPriceSheetofWheat

MOdelOfSolapurforWheat <- lm(formula = ModePrice~Quantity+MaxPrice+MinPrice,data = SolapurMarketPriceSheetofWheat)
MOdelOfSolapurforWheat

summary(MOdelOfSolapurforWheat)

intersectOfSolapurforWheat <- summary(MOdelOfSolapurforWheat)$coefficients[1, 1]
intersectOfSolapurforWheat

coefficientOfQuantityOfSolapurforWheat <- summary(MOdelOfSolapurforWheat)$coefficients[2, 1]
coefficientOfMaxPriceOfSolapurforWheat <- summary(MOdelOfSolapurforWheat)$coefficients[3, 1]
coefficientOfMinPriceOfSolapurforWheat <- summary(MOdelOfSolapurforWheat)$coefficients[4, 1]

Quantity<-c(SolapurMarketPriceSheetofWheat$Quantity)
MaxPrice<-c(SolapurMarketPriceSheetofWheat$MaxPrice)
MinPrice<-c(SolapurMarketPriceSheetofWheat$MinPrice)


price <- intersectOfSolapurforWheat + coefficientOfQuantityOfSolapurforWheat * Quantity[1] +coefficientOfMinPriceOfSolapurforWheat * MinPrice[1] + coefficientOfMaxPriceOfSolapurforWheat * MaxPrice[1]
price


SolapurMarketPriceforWheat <- c(intersectOfSolapurforWheat, coefficientOfQuantityOfSolapurforWheat,coefficientOfMinPriceOfSolapurforWheat,coefficientOfMaxPriceOfSolapurforWheat)
