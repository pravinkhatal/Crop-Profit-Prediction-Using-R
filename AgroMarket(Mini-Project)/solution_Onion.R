DataListforOnion <- c(NasikMarketPriceforOnion, SolapurMarketPriceforOnion, PuneMarketPriceforOnion, LasalgaonMarketPriceforOnion)
DataMatrixforOnion <- matrix(DataListforOnion, byrow = TRUE, nrow = 4)
DataMatrixforOnion

District <- c("Nasik","Solapur","Pune", "Lasalgaon")
Coefficients <- c("intersect", "Coefficient1", "coefficient2", "coefficient3")


rownames(DataMatrixforOnion) <- District
colnames(DataMatrixforOnion) <- Coefficients

DataMatrixforOnion
rows <- NROW(DataMatrixforOnion)
MarketListforOnion <- list()

TransportRate <- 5

AgroMarket <- function(HomePlace, Quantity, MinPrice, MaxPrice){
  maxProfit <- 0
  place <- ""
  MarketList <- data.frame()
  cityList <- list()
  cityMarket <- list()
  for(iterator in 1:rows){
    price <- DataMatrixforOnion[iterator, 1] + DataMatrixforOnion[iterator, 2] * Quantity + DataMatrixforOnion[iterator, 3] * MinPrice + DataMatrixforOnion[iterator, 4] * MaxPrice 
    distance <- distance2Points(HomePlace, rownames(DataMatrixforOnion)[iterator])
    amount <- price * Quantity - distance * TransportRate
    cityList <- c(cityList,rownames(DataMatrixforOnion)[iterator])
    cityMarket <- c(cityMarket,amount)
    
  }
  MarketList <- as.data.frame(cbind(cityList,cityMarket))
  return(MarketList)
}


AgroMarket("Latur", 10, 1900, 2950)
