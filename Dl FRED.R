library("conflicted")
library("tidyverse")
library("fredr")
# https://fred.stlouisfed.org/tags/series?t=nigeria
# https://fred.stlouisfed.org/tags/series?t=nigeria%3Bworld bank
api_key <- readLines("FRED API key.txt")
fredr_set_key(api_key)
# https://fred.stlouisfed.org/tags/series?t=gdp%3Bnigeria%3Bworld%20bank&ob=pv&od=desc
gdp_descr <- fredr_series_search_id("MKTGDPNGA646NWDB")
gdp_descr$title
gdp_descr$units_short
gdp_descr$notes
gdp <- fredr("MKTGDPNGA646NWDB")
rgdp_descr <- fredr_series_search_id("RGDPNANGA666NRUG")
# Net Lending (+) / Net Borrowing (-) (balance from Financial Account) as Direct Investment for Nigeria https://fred.stlouisfed.org/series/NGABFDBP6GDPPT
Capital Importation https://microdata.nigerianstat.gov.ng/index.php/catalog/143/related-materials
# Share of Gross Capital Formation at Current Purchasing Power Parities for Nigeria (CSHICPNGA156NRUG)
# Central government debt, total (% of GDP) for Nigeria (DEBTTLNGA188A) MAIS seulement 2003 à 2013
# Inflation, consumer prices for Nigeria (FPCPITOTLZGNGA) (ou Consumer Price Index for Nigeria (DDOE01NGA086NWDB)) OK
https://fred.stlouisfed.org/searchresults/?st=Retained&t=nigeria
https://fred.stlouisfed.org/searchresults/?st=Education&t=nigeria
https://fred.stlouisfed.org/searchresults/?st=Health&t=nigeria
https://fred.stlouisfed.org/searchresults/?st=Security&t=nigeria
Youth Unemployment Rate for Nigeria (SLUEM1524ZSNGA)
Expenditure-side Real GDP at Current Purchasing Power Parities for Nigeria (CGDPESNGA666NRUG)
https://fred.stlouisfed.org/searchresults/?st=natural&t=nigeria
Openness at constant prices for Nigeria (OPENRPNGA156NUPN)
https://nigeria.opendataforafrica.org/data/
