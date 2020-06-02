library(curl)
link <- 'https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdeathsinvolvingcovid19inthecaresectorenglandandwales%2fcurrent/deathsinvolvingcovid19inthecaresectordataset.xlsx'

destfile <- here::here('data', 'original data', "deathsinvolvingcovid19inthecaresectordataset.xlsx")
curl_download(link, destfile = destfile)
