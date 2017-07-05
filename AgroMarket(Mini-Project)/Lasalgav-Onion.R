LasalgavMarketPriceSheetOfOnion<-read.csv("lasalgav_Onion.csv")
LasalgavMarketPriceSheetOfOnion

MOdelOfLasalgavforOnion <- lm(formula = ModePrice~Quantity+MaxPrice+MinPrice,data = LasalgavMarketPriceSheetOfOnion)
MOdelOfLasalgavforOnion

summary(MOdelOfLasalgavforOnion)

intersectOfLasalgavforOnion <- summary(MOdelOfLasalgavforOnion)$coefficients[1, 1]
intersectOfLasalgavforOnion

coefficientOfQuantityOfLasalgavforOnion <- summary(MOdelOfLasalgavforOnion)$coefficients[2, 1]
coefficientOfMaxPriceOfLasalgavforOnion <- summary(MOdelOfLasalgavforOnion)$coefficients[3, 1]
coefficientOfMinPriceOfLasalgavforOnion <- summary(MOdelOfLasalgavforOnion)$coefficients[4, 1]


Quantity<-c(LasalgavMarketPriceSheetOfOnion$Quantity)
MaxPrice<-c(LasalgavMarketPriceSheetOfOnion$MaxPrice)
MinPrice<-c(LasalgavMarketPriceSheetOfOnion$MinPrice)
getwd()


price <- intersectOfLasalgavforOnion + coefficientOfQuantityOfLasalgavforOnion * Quantity[1] +coefficientOfMinPriceOfLasalgavforOnion * MinPrice[1] + coefficientOfMaxPriceOfLasalgavforOnion * MaxPrice[1]
price

LasalgavMarketPriceforOnion <- c(intersectOfLasalgavforOnion, coefficientOfQuantityOfLasalgavforOnion,coefficientOfMinPriceOfLasalgavforOnion,coefficientOfMaxPriceOfLasalgavforOnion)

