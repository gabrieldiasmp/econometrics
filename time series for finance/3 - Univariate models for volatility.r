
rm(list=ls())

lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))
library(tidyverse)
library(fpp3)
library(tidyquant)


stock <- tidyquant::tq_get(c("^GSPC","^N225","^IXIC","^NYA"), 
   get = "stock.prices" ,from="1990-01-01", to="2012-12-31") %>% 
  mutate(symbol = ifelse(symbol == "^GSPC","S&P500",symbol)) %>% 
  mutate(symbol = ifelse(symbol == "^N225","Nikkei",symbol)) %>% 
  mutate(symbol = ifelse(symbol == "^IXIC","NASDAQ",symbol)) %>% 
  mutate(symbol = ifelse(symbol == "^NYA","NYSE",symbol)) %>% 
  mutate(date = as_date(date)) %>% 
  as_tsibble(index = date,key = symbol) %>% 
  group_by_key() %>% 
  mutate(rtn = difference(log(close))) %>% 
  filter(!is.na(rtn)) %>% 
  group_by_key() %>% 
  mutate(trading_day = row_number()) 

stock %>% select(symbol,date,close,rtn)

stock %>% 
  update_tsibble(index = trading_day) %>% 
  model(AR = ARIMA(rtn~1+pdq(5,0,0))) %>% 
  augment() %>% 
  features(.resid,list(T = length, 
                    ~stat_arch_lm(.,lag = 10)))%>% 
  mutate(stat_arch_lm = T*stat_arch_lm, 
         pval_arch_lm = 1-pchisq(stat_arch_lm, df = 10)) ###lagrange multiplier test

## Box-pierce test
stock %>% 
  update_tsibble(index = trading_day) %>% 
  model(AR = ARIMA(rtn~1+pdq(5,0,0))) %>% 
  augment() %>% 
  features(.resid^2,list( 
                    ~box_pierce(.,lag = 10, dof = 5))) 

IXICrtn <- stock %>% 
  filter(symbol == "NASDAQ") %>% 
  filter(!is.na(rtn)) %>% 
  pull(rtn)

IXICrtn <- 100*IXICrtn

spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                            garchOrder=c(11,0)),
                          mean.model=list(armaOrder=c(2,0), 
                                          include.mean=TRUE))

rugarch::ugarchfit(data = IXICrtn, spec = spec)

spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                            garchOrder=c(11,0)),
                          mean.model=list(armaOrder=c(2,0), 
                                          include.mean=TRUE))

rugarch::ugarchfit(data = IXICrtn, spec = spec)

spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                            garchOrder=c(1,1)),
                          mean.model=list(armaOrder=c(2,0), 
                                          include.mean=TRUE))

rugarch::ugarchfit(data = IXICrtn, spec = spec)

spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                            garchOrder=c(1,1)),
                          mean.model=list(armaOrder=c(2,0), 
                                          include.mean=TRUE))
rugarch::ugarchfit(data = IXICrtn, spec = spec)

spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                             garchOrder=c(2,1)),
                        mean.model=list(armaOrder=c(1,0), 
                                        include.mean=TRUE))
rugarch::ugarchfit(data = IXICrtn, spec = spec)

spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                             garchOrder=c(2,1)),
                        mean.model=list(armaOrder=c(1,0), 
                                        include.mean=TRUE))

rugarch::ugarchfit(data = IXICrtn, spec = spec)

fit <- rugarch::ugarchfit(data = IXICrtn, spec = spec)
fit@fit$z %>% ts() %>% as_tsibble() %>% 
  ACF(lag_max = 20) %>% autoplot()

fit <- rugarch::ugarchfit(data = IXICrtn, spec = spec)
fit@fit$z^2 %>% ts() %>% as_tsibble() %>% 
  ACF(lag_max = 20) %>% autoplot()

qqnorm(fit@fit$z,main="",col="red")
qqline(fit@fit$z,col="blue")

spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                             garchOrder=c(2,1)),
                    mean.model=list(armaOrder=c(1,0), 
                                    include.mean=TRUE))
fit <- rugarch::ugarchfit(data = IXICrtn, spec = spec)
rugarch::ugarchforecast(fit,n.ahead = 10)

spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                             garchOrder=c(2,1)),
                    mean.model=list(armaOrder=c(1,0), 
                                    include.mean=TRUE))

fit <- rugarch::ugarchfit(data = IXICrtn, spec = spec)

rugarch::ugarchforecast(fit,n.ahead = 10)

spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                                                 garchOrder=c(2,1)),
                             mean.model=list(armaOrder=c(1,0), 
                                             include.mean=TRUE))

rec_forc <- c()
.init = 5600
for (i in .init:(length(IXICrtn)-1)){
  fit <- rugarch::ugarchfit(data = IXICrtn[1:i], spec = spec)
  forc <- rugarch::ugarchforecast(fit,n.ahead = 1)
  rec_forc[i-.init+1] <- forc@forecast$sigmaFor
}
tibble(index = 1:(length(rec_forc)), 
       rec_forc = rec_forc, 
       abs_IXICrtn = abs(IXICrtn[(.init+1):length(IXICrtn)])) %>% 
  as_tsibble(index = index) %>% 
  pivot_longer(-index) %>% 
  autoplot() 

spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                                                 garchOrder=c(2,1)),
                             mean.model=list(armaOrder=c(1,0), 
                                             include.mean=TRUE))


rec_forc <- c()
.init = 5600
for (i in .init:(length(IXICrtn)-1)){
  fit <- rugarch::ugarchfit(data = IXICrtn[1:i], spec = spec)
  forc <- rugarch::ugarchforecast(fit,n.ahead = 1)
  rec_forc[i-.init+1] <- forc@forecast$sigmaFor
}

tibble(index = 1:(length(rec_forc)), 
       rec_forc = rec_forc, 
       abs_IXICrtn = abs(IXICrtn[(.init+1):length(IXICrtn)])) %>% 
  as_tsibble(index = index) %>% 
  pivot_longer(-index) %>% 
  autoplot() 

spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                                                 garchOrder=c(2,1)),
                             mean.model=list(armaOrder=c(1,0),
                                             include.mean=TRUE),
                             distribution.model="std")

rugarch::ugarchfit(data = IXICrtn, spec = spec)

spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                                                 garchOrder=c(2,1)),
                             mean.model=list(armaOrder=c(1,0),
                                             include.mean=TRUE),
                             distribution.model="std")

rugarch::ugarchfit(data = IXICrtn, spec = spec)

spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                                                 garchOrder=c(1,1)),
                             mean.model=list(armaOrder=c(1,0),
                                             include.mean=TRUE,
                                             archm=TRUE))

rugarch::ugarchfit(data = IXICrtn, spec = spec)

spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", 
                                                 garchOrder=c(1,1)),
                             mean.model=list(armaOrder=c(1,0),
                                             include.mean=TRUE,
                                             archm=TRUE))

rugarch::ugarchfit(data = IXICrtn, spec = spec)

spec <-  rugarch::ugarchspec(variance.model=list(model="gjrGARCH", 
                                                 garchOrder=c(1,1)),
                             mean.model=list(armaOrder=c(1,0),
                                             include.mean=TRUE))

rugarch::ugarchfit(data = IXICrtn, spec = spec)

spec <-  rugarch::ugarchspec(variance.model=list(model="gjrGARCH", 
                                                 garchOrder=c(1,1)),
                             mean.model=list(armaOrder=c(1,0), include.mean=TRUE))

rugarch::ugarchfit(data = IXICrtn, spec = spec)

spec <-  rugarch::ugarchspec(variance.model=list(model="eGARCH", 
                                                 garchOrder=c(1,1)),
                             mean.model=list(armaOrder=c(1,0), 
                                             include.mean=TRUE))

rugarch::ugarchfit(data = IXICrtn, spec = spec)

spec <-  rugarch::ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                             mean.model=list(armaOrder=c(1,0), 
                                             include.mean=TRUE))

rugarch::ugarchfit(data = IXICrtn, spec = spec)
