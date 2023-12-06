library(httr)
library(jsonlite)

# Get tchr/stud ratio-------

api_url <- "https://api.uddannelsesstatistik.dk/Api/v1/statistik"
api_key <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkb21haW51c2VyIjoiZHZoX3B1YmxpY3VzZXIiLCJ0b2tlbmlkIjoiMTc3MzUzNmEtNDVkYi00ODYxLWJlNjYtOTc5YTM0ODgzNGY5IiwidXNlcmlkIjoiNWQxOGExZWMtOTkxOC00NDQzLWJjM2YtM2M3OTExMDBlZDlhIiwiZXhwIjoxNzgxMjU1NTQ3LCJpc3MiOiJodHRwczovL2RvdG5ldGRldGFpbC5uZXQiLCJhdWQiOiJodHRwczovL2RvdG5ldGRldGFpbC5uZXQifQ.dDYluKvTIuAHdWO9a9HSg_nKMVUWqtji7M2J2ktsMcE"



query <- '{
   "område": "GS",
   "emne": "PERS",
   "underemne": "PERSOEX",
   "nøgletal": [
      "Årsværk",
      "Elever pr årsværk",
      "Elever pr lærerårsværk"
   ],
   "detaljering": [
      "[Institution].[Institutionsnummer]",
      "[Personalekategori].[Personalekategori]",
      "[Skoleår].[Skoleår]"
   ],
   "filtre": {
      "[Personalekategori].[Personalekategori]": [
         "Lærere"
      ]
   },
   "indlejret": false,
   "tomme_rækker": false,
   "formattering": "json",
   "side": 1
}'





n_students <- uddstat_api_call(query, api_url = api_url, key=api_key)

library(ggplot2)

stud_tchr_ratio$ratio <- as.numeric(gsub(",", "\\.", stud_tchr_ratio$`Elever pr lærerårsværk`))

stud_tchr_ratio$year <- as.numeric(gsub("([0-9]{4}).*?$", "\\1", stud_tchr_ratio$`[Skoleår].[Skoleår].[Skoleår]`))

q10 <- seq(0.05, 0.95, by = 0.05)

p <- ggplot(stud_tchr_ratio, aes(year,ratio))
p <- p+geom_quantile(quantiles = q10, method="rqss")
p


quant_10 <- function(x){quantile(x, probs=q10)}

p <- ggplot(result, aes(year,ratio))
p <- p+stat_summary(fun=quant_10, geom="point")
p


# Get grades at school level---------


query <- '{
   "område": "GS",
   "emne": "KARA",
   "underemne": "KVALDM",
   "nøgletal": [
      "Gennemsnit - Dansk bundne prøver",
      "Gennemsnit - Matematik bundne prøver"
   ],
   "detaljering": [
      "[Institution].[Institution]",
      "[Institution].[Institutionsnummer]",
      "[Skoleår].[Skoleår]"
   ],
   "indlejret": false,
   "tomme_rækker": false,
   "formattering": "json",
   "side": 1
}'

query <- '{
   "område": "GS",
   "emne": "KARA",
   "underemne": "KARAGNS",
   "nøgletal": [
      "Gennemsnit - Obl. prøver"
   ],
   "detaljering": [
      "[Institution].[Institution]",
      "[Institution].[Institutionsnummer]",
      "[Skoleår].[Skoleår]"
   ],
   "indlejret": false,
   "tomme_rækker": false,
   "formattering": "json",
   "side": 1
}'


headers <- c("content-type" = "application/json", "authorization" = paste("Bearer", api_key))

response <- POST(api_url, body = query, encode = "json", add_headers(.headers = headers))
grades <- fromJSON(content(response, "text"))

page                    <- 1
api_response_length     <- 1
data                    <- list()

while (api_response_length > 0) {
  
  cat(paste("processing page", page, "\n"))
  
  query_new <- gsub('"side": 1', paste('"side": ', page), query)
  
  #cat(query_new)
  
  
  response <- POST(api_url, body = query_new, encode = "json", add_headers(.headers = headers))
  result <- fromJSON(content(response, "text"))
  
  data[[page]]          <- result
  api_response_length   <- length(result)
  page                  <- page + 1
}

grades           <- do.call(rbind, data)



# get N students---------

query <- '{
   "område": "GS",
   "emne": "ELEV",
   "underemne": "ELEVEX",
   "nøgletal": [
      "Antal elever",
      "Andel der modtager seg specialundervisning"
   ],
   "detaljering": [
      "[Institution].[Institution]",
      "[Institution].[Institutionsnummer]",
      "[Skoleår].[Skoleår]"
   ],
   "indlejret": false,
   "tomme_rækker": false,
   "formattering": "json",
   "side": 1
}'

headers <- c("content-type" = "application/json", "authorization" = paste("Bearer", api_key))

response <- POST(api_url, body = query, encode = "json", add_headers(.headers = headers))
n_students <- fromJSON(content(response, "text"))


page                    <- 1
api_response_length     <- 1
data                    <- list()

while (api_response_length > 0) {
  
  cat(paste("processing page", page, "\n"))
  
  query_new <- gsub('"side": 1', paste('"side": ', page), query)
  
  #cat(query_new)
  
  
  response <- POST(api_url, body = query_new, encode = "json", add_headers(.headers = headers))
  result <- fromJSON(content(response, "text"))
  
  data[[page]]          <- result
  api_response_length   <- length(result)
  page                  <- page + 1
}

n_students          <- do.call(rbind, data)

# get student ethnicity-------

query <- '{
   "område": "GS",
   "emne": "ELEV",
   "underemne": "ELEVEX",
   "nøgletal": [
      "Antal elever"
   ],
   "detaljering": [
      "[Institution].[Institution]",
      "[Institution].[Institutionsnummer]",
      "[Herkomst].[Herkomstgruppe]",
      "[Skoleår].[Skoleår]"
   ],
   "indlejret": false,
   "tomme_rækker": false,
   "formattering": "json",
   "side": 1
}'

headers <- c("content-type" = "application/json", "authorization" = paste("Bearer", api_key))

response <- POST(api_url, body = query, encode = "json", add_headers(.headers = headers))
stud_ethn <- fromJSON(content(response, "text"))

stud_ethn$`Antal elever` <- gsub("\\.", "", stud_ethn$`Antal elever`)


library(reshape2)

data_wide <- dcast(stud_ethn, `[Institution].[Institution].[Institution]` + 
                     `[Institution].[Institutionsnummer].[Institutionsnummer]`+
                     `[Skoleår].[Skoleår].[Skoleår]` ~ `[Herkomst].[Herkomstgruppe].[Herkomstgruppe]`, value.var="Antal elever")

data_wide$ethn_share <- as.numeric(data_wide$Udenlandsk)/(as.numeric(data_wide$Dansk)+as.numeric(data_wide$Udenlandsk))





page                    <- 1
api_response_length     <- 1
data                    <- list()

while (api_response_length > 0) {
  
  cat(paste("processing page", page, "\n"))
  
  query_new <- gsub('"side": 1', paste('"side": ', page), query)
  
  #cat(query_new)
  
  
  response <- POST(api_url, body = query_new, encode = "json", add_headers(.headers = headers))
  result <- fromJSON(content(response, "text"))
  
  data[[page]]          <- result
  api_response_length   <- length(result)
  page                  <- page + 1
}

stud_ethn          <- do.call(rbind, data)

stud_ethn$`Antal elever` <- gsub("\\.", "", stud_ethn$`Antal elever`)

data_wide_ethn <- dcast(stud_ethn, `[Institution].[Institution].[Institution]` + 
                     `[Institution].[Institutionsnummer].[Institutionsnummer]`+
                     `[Skoleår].[Skoleår].[Skoleår]` ~ `[Herkomst].[Herkomstgruppe].[Herkomstgruppe]`, value.var="Antal elever")

data_wide_ethn$ethn_share <- as.numeric(data_wide$Udenlandsk)/(as.numeric(data_wide$Dansk)+as.numeric(data_wide$Udenlandsk))


#get student parental background-------



query <- '{
    "område": "GS",
   "emne": "ELEV",
   "underemne": "ELEVEX",
   "nøgletal": [
      "Antal elever"
   ],
   "detaljering": [
      "[Forældres Højest Fuldførte Uddannelse].[Uddannelsesovergruppe]",
      "[Institution].[Institution]",
      "[Institution].[Institutionsnummer]",
      "[Skoleår].[Skoleår]"
   ],
   "indlejret": false,
   "tomme_rækker": false,
   "formattering": "json",
   "side": 1
}'

headers <- c("content-type" = "application/json", "authorization" = paste("Bearer", api_key))

response <- POST(api_url, body = query, encode = "json", add_headers(.headers = headers))
stud_back <- fromJSON(content(response, "text"))

stud_back$`Antal elever` <- gsub("\\.", "", stud_back$`Antal elever`)


library(reshape2)

data_wide_back <- dcast(stud_back, `[Institution].[Institution].[Institution]` + 
                     `[Institution].[Institutionsnummer].[Institutionsnummer]`+
                     `[Skoleår].[Skoleår].[Skoleår]` ~ `[Forældres Højest Fuldførte Uddannelse].[Uddannelsesovergruppe].[Uddannelsesovergruppe]`, 
                   value.var="Antal elever")





page                    <- 1
api_response_length     <- 1
data                    <- list()

while (api_response_length > 0) {
  
  cat(paste("processing page", page, "\n"))
  
  query_new <- gsub('"side": 1', paste('"side": ', page), query)
  
  #cat(query_new)
  
  
  response <- POST(api_url, body = query_new, encode = "json", add_headers(.headers = headers))
  result <- fromJSON(content(response, "text"))
  
  data[[page]]          <- result
  api_response_length   <- length(result)
  page                  <- page + 1
}

stud_back          <- do.call(rbind, data)

stud_back$`Antal elever` <- as.numeric(gsub("\\.", "", stud_back$`Antal elever`))


library(reshape2)

data_wide_back <- dcast(stud_back, `[Institution].[Institution].[Institution]` + 
                          `[Institution].[Institutionsnummer].[Institutionsnummer]`+
                          `[Skoleår].[Skoleår].[Skoleår]` ~ `[Forældres Højest Fuldførte Uddannelse].[Uddannelsesovergruppe].[Uddannelsesovergruppe]`, 
                        value.var="Antal elever")

data_wide_back$share_college <- data_wide_back$`Videregående uddannelser`/base::rowSums(data_wide_back[c("Grundskoleuddannelse",
                                                                                                         "Ungdomsuddannelse",
                                                                                                         "Videregående uddannelser")])

names(data_wide_back)


# get student absence-----


query <- '{
   "område": "GS",
   "emne": "ELEVFRAV",
   "underemne": "FRAVAAR",
   "nøgletal": [
      "Gennemsnitligt fravær per skoleår",
      "Ulovligt fravær per skoleår"
   ],
   "detaljering": [
      "[Institution].[Institution]",
      "[Institution].[Institutionsnummer]",
      "[Tid].[Skoleår]"
   ],
   "indlejret": false,
   "tomme_rækker": false,
   "formattering": "json",
   "side": 1
}'

headers <- c("content-type" = "application/json", "authorization" = paste("Bearer", api_key))

response <- POST(api_url, body = query, encode = "json", add_headers(.headers = headers))
stud_abs <- fromJSON(content(response, "text"))


page                    <- 1
api_response_length     <- 1
data                    <- list()

while (api_response_length > 0) {
  
  cat(paste("processing page", page, "\n"))
  
  query_new <- gsub('"side": 1', paste('"side": ', page), query)
  
  #cat(query_new)
  
  
  response <- POST(api_url, body = query_new, encode = "json", add_headers(.headers = headers))
  result <- fromJSON(content(response, "text"))
  
  data[[page]]          <- result
  api_response_length   <- length(result)
  page                  <- page + 1
}

stud_abs         <- do.call(rbind, data)




# get student wellbeing--------



query <- '{
   "område": "GS",
   "emne": "TRIV",
   "underemne": "KVALGEN",
   "nøgletal": [
      "Samlet Indikatorsvar"
   ],
   "detaljering": [
      "[Institution].[Institution]",
      "[Institution].[Institutionsnummer]",
      "[Skoleår].[Skoleår]"
   ],
   "indlejret": false,
   "tomme_rækker": false,
   "formattering": "json",
   "side": 1
}'

headers <- c("content-type" = "application/json", "authorization" = paste("Bearer", api_key))

response <- POST(api_url, body = query, encode = "json", add_headers(.headers = headers))
stud_well <- fromJSON(content(response, "text"))


page                    <- 1
api_response_length     <- 1
data                    <- list()

while (api_response_length > 0) {
  
  cat(paste("processing page", page, "\n"))
  
  query_new <- gsub('"side": 1', paste('"side": ', page), query)
  
  #cat(query_new)
  
  
  response <- POST(api_url, body = query_new, encode = "json", add_headers(.headers = headers))
  result <- fromJSON(content(response, "text"))
  
  data[[page]]          <- result
  api_response_length   <- length(result)
  page                  <- page + 1
}

stud_well         <- do.call(rbind, data)





# get teacher qualifications-------


query <- '{
   "område": "GS",
   "emne": "KOMP",
   "underemne": "KOMPEX",
   "nøgletal": [
      "Med kompetence andel"
   ],
   "detaljering": [
      "[Institution].[Institution]",
      "[Institution].[Institutionsnummer]",
      "[Skoleår].[Skoleår]"
   ],
   "indlejret": false,
   "tomme_rækker": false,
   "formattering": "json",
   "side": 1
}'

headers <- c("content-type" = "application/json", "authorization" = paste("Bearer", api_key))

response <- POST(api_url, body = query, encode = "json", add_headers(.headers = headers))
tch_qual <- fromJSON(content(response, "text"))


page                    <- 1
api_response_length     <- 1
data                    <- list()

while (api_response_length > 0) {
  
  cat(paste("processing page", page, "\n"))
  
  query_new <- gsub('"side": 1', paste('"side": ', page), query)
  
  #cat(query_new)
  
  
  response <- POST(api_url, body = query_new, encode = "json", add_headers(.headers = headers))
  result <- fromJSON(content(response, "text"))
  
  data[[page]]          <- result
  api_response_length   <- length(result)
  page                  <- page + 1
}

tch_qual         <- do.call(rbind, data)



# get teaching hours-------


query <- '{
   "område": "GS",
   "emne": "PERS",
   "underemne": "PERSOEX",
   "nøgletal": [
      "Undervisning i alt"
   ],
   "detaljering": [
      "[Institution].[Institution]",
      "[Institution].[Institutionsnummer]",
      "[Skoleår].[Skoleår]"
   ],
   "indlejret": false,
   "tomme_rækker": false,
   "formattering": "json",
   "side": 1
}'

headers <- c("content-type" = "application/json", "authorization" = paste("Bearer", api_key))

response <- POST(api_url, body = query, encode = "json", add_headers(.headers = headers))
tching_hours <- fromJSON(content(response, "text"))


page                    <- 1
api_response_length     <- 1
data                    <- list()

while (api_response_length > 0) {
  
  cat(paste("processing page", page, "\n"))
  
  query_new <- gsub('"side": 1', paste('"side": ', page), query)
  
  #cat(query_new)
  
  
  response <- POST(api_url, body = query_new, encode = "json", add_headers(.headers = headers))
  result <- fromJSON(content(response, "text"))
  
  data[[page]]          <- result
  api_response_length   <- length(result)
  page                  <- page + 1
}

stud_well         <- do.call(rbind, data)






# merge data-------

library(dplyr)

dat_merge <- dplyr::left_join(stud_tchr_ratio, grades)

dat_merge <- dplyr::left_join(dat_merge, n_students)



names(dat_merge) <- c("inst_nr", "personnel cat", "year", "n_teachers", "student_employee_ratio", "stud_tchr_ratio", "schl_name",
                      "grade_danish", "grade_math")

names(dat_merge) <- c("inst_nr", "personnel cat", "year", "n_teachers", "student_employee_ratio", "stud_tchr_ratio", "schl_name",
                      "grades", "n_students")

dat_merge$year_2 <- as.numeric(gsub(".*?/(.*?)","\\1", dat_merge$year))



# Descriptive results------

## Descriptive stats-----

## Raw association-----

dat_merge$grade_danish <- gsub(",", "\\.", dat_merge$grade_danish)

dat_merge$grade_math <- gsub(",", "\\.", dat_merge$grade_math)

dat_merge$grades <- as.numeric(gsub(",", "\\.", dat_merge$grades))



dat_merge$stud_tchr_ratio <- as.numeric(gsub(",", "\\.", dat_merge$stud_tchr_ratio))


dat_merge$grade_danish <- as.numeric(as.character(dat_merge$grade_danish))

dat_merge$grade_math<- as.numeric(as.character(dat_merge$grade_math))


dat_merge$stud_tchr_ratio <- as.numeric(dat_merge$stud_tchr_ratio)

str(dat_merge)

p <- ggplot(dat_merge, aes(stud_tchr_ratio, grade_danish))
p <- p+geom_smooth()
p

p <- ggplot(dat_merge, aes(stud_tchr_ratio, grade_math))
p <- p+geom_smooth()
p


p <- ggplot(dat_merge, aes(stud_tchr_ratio, grades))
p <- p+geom_smooth()
p


#fixed effects model--------

library(fixest)

mod <-  feols(grades ~ stud_tchr_ratio | inst_nr, dat_merge)

summary(mod)

mod_dat <- dat_merge[mod$obs_selection$obsRemoved,]

setdiff(dat_merge$inst_nr, mod_dat$inst_nr)

dat_merge$stud_tchr_ratio_mean <- ave(dat_merge$stud_tchr_ratio, list(dat_merge$inst_nr),
                                      FUN=function(x){mean(x,na.rm=TRUE)})

dat_merge$stud_tchr_ratio_demean <- dat_merge$stud_tchr_ratio-dat_merge$stud_tchr_ratio_mean 

dat_merge$grades_mean <- ave(dat_merge$grades, list(dat_merge$inst_nr),
                                      FUN=function(x){mean(x,na.rm=TRUE)})

dat_merge$grades_demean <- dat_merge$grades-dat_merge$grades_mean 

summary(lm(grades_demean~stud_tchr_ratio_demean, data=dat_merge))



p <- ggplot(dat_merge, aes(stud_tchr_ratio_demean, grades_demean))
p <- p+geom_smooth()
p

#varying slope

mod <-  feols(grades ~ stud_tchr_ratio | inst_nr, dat_merge)

summary(mod)

# Effect heterogeneity-------

#did effect of ratio differ between schools that experienced an influx of students that either raised or lowered the
#avg. parental background of schools?

#did effect of ratio differ across schools that were at different quantiles of student achievement? 
#compute the highest quantile of stud ach during the period for each school, or perhaps the avg. quantile
#during the period


## Effect size-----

# How much variance within schools compared to total variance?-----

#compare demeaned stud/tchr ratio for total sample vs demeaned ratio within schools


dat_merge$stud_tchr_ratio_demean_sample <- dat_merge$stud_tchr_ratio-mean(dat_merge$stud_tchr_ratio, na.rm=TRUE)

sd(dat_merge$stud_tchr_ratio_demean_sample, na.rm=TRUE)

sd(dat_merge$stud_tchr_ratio_demean, na.rm=TRUE)

p <- ggplot(dat_merge, aes(stud_tchr_ratio_demean_sample))
p <- p+geom_density()
p <- p+geom_density(aes(x=dat_merge$stud_tchr_ratio_demean), color="red")
p

## range within schools-----

sample_range <- min(dat_merge$stud_tchr_ratio, na.rm=TRUE)-max(dat_merge$stud_tchr_ratio, na.rm=TRUE)

ranges <- aggregate(dat_merge$stud_tchr_ratio, list(dat_merge$inst_nr), function(x){
  

  
  c(range=(min(x, na.rm=TRUE)-max(x, na.rm=TRUE)), 
    perc_sample=(min(x, na.rm=TRUE)-max(x, na.rm=TRUE))/sample_range)
  

  
})

ranges <- do.call(data.frame, ranges)

str(ranges)

p <- ggplot(ranges, aes(x.perc_sample))
p <- p+stat_ecdf()
#p <- p+geom_vline(xintercept=sample_range)
p

p <- ggplot(ranges, aes(x.range))
p <- p+geom_histogram()
p <- p+geom_vline(xintercept=sample_range)
p