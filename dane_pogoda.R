install.packages("GSODR") 

library("GSODR")
library("dplyr")

# stacja Okecie 123750-99999
pogoda_wwa <- get_GSOD(years = 2014:2016, station = "123750-99999")

pogoda_wwa <- pogoda_wwa %>%
    select(STN_NAME, YEARMODA, TEMP)

#stacja Krzesiny 123260-99999
pogoda_poz <- get_GSOD(years = 2014:2016, station = "123260-99999")

pogoda_poz <- pogoda_poz %>%
    select(STN_NAME, YEARMODA, TEMP)

save(pogoda_wwa, pogoda_poz, file = 'pogoda_hist.RData')
