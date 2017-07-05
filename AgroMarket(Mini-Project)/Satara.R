SataraMarketPriceSheetOfWheat<-read.csv("Satara.csv")
SataraMarketPriceSheetOfWheat

MOdelOfSataraforWheat <- lm(formula = ModePrice~Quantity+MaxPrice+MinPrice,data = SataraMarketPriceSheetOfWheat)
MOdelOfSataraforWheat

summary(MOdelOfSataraforWheat)

intersectOfSataraforWheat <- summary(MOdelOfSataraforWheat)$coefficients[1, 1]
intersectOfSataraforWheat

coefficientOfQuantityOfSataraforWheat <- summary(MOdelOfSataraforWheat)$coefficients[2, 1]
coefficientOfMaxPriceOfSataraforWheat <- summary(MOdelOfSataraforWheat)$coefficients[3, 1]
coefficientOfMinPriceOfSataraforWheat <- summary(MOdelOfSataraforWheat)$coefficients[4, 1]

Quantity<-c(SataraMarketPriceSheetOfWheat$Quantity)
MaxPrice<-c(SataraMarketPriceSheetOfWheat$MaxPrice)
MinPrice<-c(SataraMarketPriceSheetOfWheat$MinPrice)


price <- intersectOfSataraforWheat + coefficientOfQuantityOfSataraforWheat * Quantity[1] +coefficientOfMinPriceOfSataraforWheat * MinPrice[1] + coefficientOfMaxPriceOfSataraforWheat * MaxPrice[1]
price


SataraMarketPriceforWheat <- c(intersectOfSataraforWheat, coefficientOfQuantityOfSataraforWheat, coefficientOfMinPriceOfSataraforWheat, coefficientOfMaxPriceOfSataraforWheat)
