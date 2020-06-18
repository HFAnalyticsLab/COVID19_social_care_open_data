library(curl)
link <- 'https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdeathsinvolvingcovid19inthecaresectorenglandandwales%2fcurrent/deathsinvolvingcovid19inthecaresectordataset.xlsx'

destfile <- here::here('data', 'original data', "deathsinvolvingcovid19inthecaresectordataset.xlsx")
curl_download(link, destfile = destfile)

## Outbreaks data


link <- 'https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/891406/Care_home_outbreaks_of_COVID-19_Management_Information.ods'

destfile <- here::here('data', 'original data', "Care_home_outbreaks_of_COVID-19_Management_Information.ods")
curl_download(link, destfile = destfile)
