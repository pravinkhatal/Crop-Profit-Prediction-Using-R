NasikMarketPriceSheetOfOnion<-read.csv("Nasik-Onion.csv")
NasikMarketPriceSheetOfOnion

MOdelOfNasikforOnion <- lm(formula = ModePrice~Quantity+MaxPrice+MinPrice,data = NasikMarketPriceSheetOfOnion)
MOdelOfNasikforOnion

summary(MOdelOfNasikforOnion)

intersectOfNasikforOnion <- summary(MOdelOfNasikforOnion)$coefficients[1, 1]
intersectOfNasikforOnion

coefficientOfQuantityOfNasikforOnion <- summary(MOdelOfNasikforOnion)$coefficients[2, 1]
coefficientOfMaxPriceOfNasikforOnion <- summary(MOdelOfNasikforOnion)$coefficients[3, 1]
coefficientOfMinPriceOfNasikforOnion <- summary(MOdelOfNasikforOnion)$coefficients[4, 1]


Quantity<-c(NasikMarketPriceSheetOfOnion$Quantity)
MaxPrice<-c(NasikMarketPriceSheetOfOnion$MaxPrice)
MinPrice<-c(NasikMarketPriceSheetOfOnion$MinPrice)



price <- intersectOfNasikforOnion + coefficientOfQuantityOfNasikforOnion * Quantity[1] +coefficientOfMinPriceOfNasikforOnion * MinPrice[1] + coefficientOfMaxPriceOfNasikforOnion * MaxPrice[1]
price

NasikMarketPriceforOnion <- c(intersectOfNasikforOnion, coefficientOfQuantityOfNasikforOnion,coefficientOfMinPriceOfNasikforOnion,coefficientOfMaxPriceOfNasikforOnion)

