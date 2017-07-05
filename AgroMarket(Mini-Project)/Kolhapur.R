KolhapurMarketPriceSheetofWheat<-read.csv("Kolhapur.csv")
KolhapurMarketPriceSheetofWheat

MOdelOfKolhapurforWheat <- lm(formula = ModePrice~Quantity+MaxPrice+MinPrice,data = KolhapurMarketPriceSheetofWheat)
MOdelOfKolhapurforWheat

summary(MOdelOfKolhapurforWheat)

intersectOfKolhapurforWheat <- summary(MOdelOfKolhapurforWheat)$coefficients[1, 1]
intersectOfKolhapurforWheat

coefficientOfQuantityOfKolhapurforWheat <- summary(MOdelOfKolhapurforWheat)$coefficients[2, 1]
coefficientOfMaxPriceOfKolhapurforWheat <- summary(MOdelOfKolhapurforWheat)$coefficients[3, 1]
coefficientOfMinPriceOfKolhapurforWheat <- summary(MOdelOfKolhapurforWheat)$coefficients[4, 1]

Quantity<-c(KolhapurMarketPriceSheetofWheat$Quantity)
MaxPrice<-c(KolhapurMarketPriceSheetofWheat$MaxPrice)
MinPrice<-c(KolhapurMarketPriceSheetofWheat$MinPrice)


price <- intersectOfKolhapurforWheat + coefficientOfQuantityOfKolhapurforWheat * Quantity[1] +coefficientOfMinPriceOfKolhapurforWheat * MinPrice[1] + coefficientOfMaxPriceOfKolhapurforWheat * MaxPrice[1]
price

KolhapurMarketPriceforWheat <- c(intersectOfKolhapurforWheat, coefficientOfQuantityOfKolhapurforWheat, coefficientOfMinPriceOfKolhapurforWheat, coefficientOfMaxPriceOfKolhapurforWheat)
