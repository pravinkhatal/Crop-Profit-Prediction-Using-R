LaturMarketPriceSheetOfWheat<-read.csv("Latur.csv")
LaturMarketPriceSheetOfWheat

MOdelOflaturforWheat <- lm(formula = ModePrice~Quantity+MaxPrice+MinPrice,data = LaturMarketPriceSheetOfWheat)
MOdelOfSangliforWheat

summary(MOdelOflaturforWheat)

intersectOfLaturforWheat <- summary(MOdelOflaturforWheat)$coefficients[1, 1]
intersectOfLaturforWheat

coefficientOfQuantityOfLaturforWheat <- summary(MOdelOflaturforWheat)$coefficients[2, 1]
coefficientOfMaxPriceOfLaturforWheat <- summary(MOdelOflaturforWheat)$coefficients[3, 1]
coefficientOfMinPriceOfLaturforWheat <- summary(MOdelOflaturforWheat)$coefficients[4, 1]

Quantity<-c(LaturMarketPriceSheetOfWheat$Quantity)
MaxPrice<-c(LaturMarketPriceSheetOfWheat$MaxPrice)
MinPrice<-c(LaturMarketPriceSheetOfWheat$MinPrice)


price <- intersectOfLaturforWheat + coefficientOfQuantityOfLaturforWheat * Quantity[1] +coefficientOfMinPriceOfLaturforWheat * MinPrice[1] + coefficientOfMaxPriceOfLaturforWheat * MaxPrice[1]
price

LaturMarketPriceforWheat <- c(intersectOfLaturforWheat, coefficientOfQuantityOfLaturforWheat,coefficientOfMinPriceOfLaturforWheat, coefficientOfMaxPriceOfLaturforWheat)
