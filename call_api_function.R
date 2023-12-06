uddstat_api_call <- function(query, api_url, key){
  
  headers <- c("content-type" = "application/json", "authorization" = paste("Bearer", api_key))
  
  
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
  
  result         <- do.call(rbind, data)
  
  return(result)
  
}


api_url <- "https://api.uddannelsesstatistik.dk/Api/v1/statistik"
api_key <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkb21haW51c2VyIjoiZHZoX3B1YmxpY3VzZXIiLCJ0b2tlbmlkIjoiMTc3MzUzNmEtNDVkYi00ODYxLWJlNjYtOTc5YTM0ODgzNGY5IiwidXNlcmlkIjoiNWQxOGExZWMtOTkxOC00NDQzLWJjM2YtM2M3OTExMDBlZDlhIiwiZXhwIjoxNzgxMjU1NTQ3LCJpc3MiOiJodHRwczovL2RvdG5ldGRldGFpbC5uZXQiLCJhdWQiOiJodHRwczovL2RvdG5ldGRldGFpbC5uZXQifQ.dDYluKvTIuAHdWO9a9HSg_nKMVUWqtji7M2J2ktsMcE"



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

n_students <- uddstat_api_call(query, api_url = api_url, key=api_key)