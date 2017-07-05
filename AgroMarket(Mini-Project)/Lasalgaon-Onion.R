LasalgaonMarketPriceSheetOfOnion<-read.csv("lasalgaon-Onion.csv")
LasalgaonMarketPriceSheetOfOnion

MOdelOfLasalgaonforOnion <- lm(formula = ModePrice~Quantity+MaxPrice+MinPrice,data = LasalgaonMarketPriceSheetOfOnion)
MOdelOfLasalgaonforOnion

summary(MOdelOfLasalgaonforOnion)

intersectOfLasalgaonforOnion <- summary(MOdelOfLasalgaonforOnion)$coefficients[1, 1]
intersectOfLasalgaonforOnion

coefficientOfQuantityOfLasalgaonforOnion <- summary(MOdelOfLasalgaonforOnion)$coefficients[2, 1]
coefficientOfMaxPriceOfLasalgaonforOnion <- summary(MOdelOfLasalgaonforOnion)$coefficients[3, 1]
coefficientOfMinPriceOfLasalgaonforOnion <- summary(MOdelOfLasalgaonforOnion)$coefficients[4, 1]


Quantity<-c(LasalgaonMarketPriceSheetOfOnion$Quantity)
MaxPrice<-c(LasalgaonMarketPriceSheetOfOnion$MaxPrice)
MinPrice<-c(LasalgaonMarketPriceSheetOfOnion$MinPrice)
getwd()


price <- intersectOfLasalgaonforOnion + coefficientOfQuantityOfLasalgaonforOnion * Quantity[1] +coefficientOfMinPriceOfLasalgaonforOnion * MinPrice[1] + coefficientOfMaxPriceOfLasalgaonforOnion * MaxPrice[1]
price

LasalgaonMarketPriceforOnion <- c(intersectOfLasalgaonforOnion, coefficientOfQuantityOfLasalgaonforOnion,coefficientOfMinPriceOfLasalgaonforOnion,coefficientOfMaxPriceOfLasalgaonforOnion)

