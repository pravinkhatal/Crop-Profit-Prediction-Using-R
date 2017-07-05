SangliMarketPriceSheetOfWheat<-read.csv("Sangli.csv")
SangliMarketPriceSheetOfWheat

MOdelOfSangliforWheat <- lm(formula = ModePrice~Quantity+MaxPrice+MinPrice,data = SangliMarketPriceSheetOfWheat)
MOdelOfSangliforWheat

summary(MOdelOfSangliforWheat)

intersectOfSangliforWheat <- summary(MOdelOfSangli)$coefficients[1, 1]
intersectOfSangliforWheat

coefficientOfQuantityOfSangliforWheat <- summary(MOdelOfSangli)$coefficients[2, 1]
coefficientOfMaxPriceOfSangliforWheat <- summary(MOdelOfSangli)$coefficients[3, 1]
coefficientOfMinPriceOfSangliforWheat <- summary(MOdelOfSangli)$coefficients[4, 1]

Quantity<-c(SangliMarketPriceSheetOfWheat$Quantity)
MaxPrice<-c(SangliMarketPriceSheetOfWheat$MaxPrice)
MinPrice<-c(SangliMarketPriceSheetOfWheat$MinPrice)


price <- intersectOfSangliforWheat + coefficientOfQuantityOfSangliforWheat * Quantity[1] +coefficientOfMinPriceOfSangliforWheat * MinPrice[1] + coefficientOfMaxPriceOfSangliforWheat * MaxPrice[1]
price

SangliMarketPriceforWheat <- c(intersectOfSangliforWheat, coefficientOfQuantityOfSangliforWheat,coefficientOfMinPriceOfSangliforWheat,coefficientOfMaxPriceOfSangliforWheat)
