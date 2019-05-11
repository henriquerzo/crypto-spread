library('dplyr')
library('XML')
library('RCurl')
library('rlist')

exchanges <- c('binance', 'poloniex', 'bitfinex', 'bittrex', 'cex-io', 'hitbtc', 'huobi', 'lbank', 'okex', 'zb-com', 'upbit', 'bitforex')

min_price <- 800000

table <- data.frame()

for (exchange in exchanges) {
  theurl <- getURL(paste("https://coinmarketcap.com/exchanges/", exchange, '/', sep=""),.opts = list(ssl.verifypeer = FALSE) )
  tables <- readHTMLTable(theurl)
  tables <- list.clean(tables, fun = is.null, recursive = FALSE)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  
  df <- tables[[which.max(n.rows)]]
  
  df <- df %>% select('Currency', 'Pair', 'Volume (24h)', 'Price')
  
  colnames(df) <- c('currency', 'pair', 'volume', 'price')
  
  df <- df %>% mutate_all(funs(as.character))
  
  df <- df %>% mutate_at(vars(volume:price), funs(as.numeric(gsub('[$,* BTC\n]', '', .)))) %>%
    filter(volume >= min_price)
  
  df$volume <- prettyNum(df$volume, big.mark=",")
  
  df$exchange <- rep(exchange, nrow(df))
  
  table <- rbind(table, df)
}

table_aux <- data.frame()

for (i in 1:length(exchanges)) {
  for (j in 1:length(exchanges)) {
    if (j > i) {
      print(paste(exchanges[i],  exchanges[j], sep=' '))
      
      exchange1 <- exchanges[i]
      exchange2 <- exchanges[j]
      
      exchange1 <- table %>% filter(exchange == exchange1)
      exchange2 <- table %>% filter(exchange == exchange2)
      
      df <- inner_join(exchange1, exchange2, by=c('currency', 'pair'))
      
      df$percentage <- ((df$price.y / df$price.x) - 1) * 100
      
      table_aux <- rbind(table_aux, df)
      
    }
      
  }
}
  
table_final <- (
  table_aux  
  %>% select('currency', 'pair', 'volume.x', 'volume.y', 'price.x', 'price.y', 'exchange.x', 'exchange.y', 'percentage') 
  #%>% filter(percentage > 1.5 | percentage < -1.5) 
  %>% filter(!(grepl("USD", pair, fixed = TRUE)))
  %>% filter(!(grepl("EUR", pair, fixed = TRUE)))
)
