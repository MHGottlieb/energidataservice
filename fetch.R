# Dependencies ------------------------------------------------------------

  library(RCurl)
  library(jsonlite)

# Henter Data -------------------------------------------------------------
  # se: energidataservice.dk/dataset/electricitybalance/resource_extract/498c68e3-d248-4965-b36f-3aa738130adc
  
  # Parametre til Energinet
  
  resource_id1 <- "498c68e3-d248-4965-b36f-3aa738130adc" # Electricity balance data (validated)
  resource_id2 <- "02356e88-7c4e-4ee9-b896-275d217cc1b9" # Electricity balance data (Non-validated)
  url <- "https://api.energidataservice.dk/datastore_search?resource_id="
  
  # Henter valideret data
  
  request1 <- paste0(url, resource_id1) # Validated
  limit1 <- fromJSON(getURL(request1))$result$total # antal observationer tilgaengelig 
  
  request <- paste0(request1, "&limit=", limit1) # henter samlet datasaet
  df <- fromJSON(getURL(request)) # ca. 25 Mb JSON, tager et minuts tid
  df <- df$result$records # isolerer relevant data
  message(paste0("Success! Hentede ", limit1, " observationer")) 
  
  # Henter non-valideret data
  
  request2 <- paste0(url, resource_id2) # Non-validated
  limit2 <- fromJSON(getURL(request2))$result$total # antal observationer tilgaengelig 
  
  request <- paste0(request2, "&limit=", limit2) # henter samlet datasaet
  df_non_valideret <- getURL(request) # ca. 1.5 Mb JSON
  df_non_valideret <- gsub("NaN", "null", df_non_valideret)
  df_non_valideret <- fromJSON(df_non_valideret)
  df_non_valideret <- df_non_valideret$result$records # isolerer relevant data
  message(paste0("Success! Hentede ", limit2, " observationer"))  

  # Rydder op og fjerner data, der ikke anvendes

  df <- df[,c(4,13,1,5,9,6,8,14,15,10,11,7)]
  df$HourDK <- strftime(sub("T", " ", df$HourDK), format="%Y-%m-%d %H:%M") #formaterer tid
  
  df_nv <- cbind(df_non_valideret[,c(6, 17, 18)], NA, NA, NA, df_non_valideret[,c(11, 1, 12, 4, 10, 2)])
  df_nv$HourDK <- strftime(sub("T", " ", df_nv$HourDK), format="%Y-%m-%d %H:%M")
  names(df_nv)[c(4:6)] <- c("NetCon", "CentralProd", "LocalProd") # tager to tomme kolonner med, blot så df og df_nv er "ens"
  
  remove(limit1, limit2, request, request1, request2, resource_id1, resource_id2, url, df_non_valideret)
  
  # Det her er en skoenhed. Den siger "tag alle kolonner med tal og sum hver kolonne efter time - medtag NA'erne"
  
  df <- aggregate(as.matrix(df[,-c(1:2,12)]) ~ HourDK, data=df, FUN=sum, na.action=na.pass)
  df <- df[order(df$HourDK, decreasing=TRUE),]
  
  # Og det samme for df_nv
  
  df_nv <- aggregate(as.matrix(df_nv[,-c(1:2,12)]) ~ HourDK, data=df_nv, FUN=sum, na.action=na.pass)
  df_nv <- df_nv[order(df_nv$HourDK, decreasing=TRUE),]
  
  # Kalder variable det samme mellem de to data frames
  
  names(df_nv)[c(2,5,8)] <- c("GrossCon", "LocalPowerProd", "SolarPowerProd")
  
  # Noterer valideret og ikke-valideret
  
  df$Valideret <- TRUE
  df_nv$Valideret <- FALSE
  
# Merger data til én tabel ------------------------------------------------
  
  # Nedenstaaende er en dum, hacky maade at føje de to datarammer sammen. Det er grimt, men det virker!
  # Loppet går kolonnerne i den validerede data igennem for NA'er. Hvis der er NA'er tilstede,
  # Tager den data for de samme timer i den ikke-validerede data og overskriver NA'erne.
  
  df_recent <- df[which(df$HourDK %in% df_nv$HourDK),] # vælger timer med overlap mellem df og df_nv
  
  for(x in c(2:10)){
    if(!sum(is.na(df_recent[,x]))==0){ # hvis der er NA-værdier i df-kolonne x
      temp <- df_nv[df_nv$HourDK %in% df_recent$HourDK[which(is.na(df_recent[,x]))],x] # kopier tilsvarende værdier fra df_nv...
      df_recent[is.na(df_recent[,x]),x] <- temp # ... og indsæt på de pladser i df, der mangler data
      df_recent$Valideret[is.na(df_recent[,x])] <- FALSE # noterer, at disse observationer ikker er (100%) valideret
      message(paste0("Indført ", length(temp), " værdier i ", names(df_recent)[x])) # Status
    }
  }
  
  # binder sammen til een dataframe
  
  df_samlet <- rbind(
    df_nv[which(!df_nv$HourDK %in% df_recent$HourDK),], #nyeste observationer
    df_recent, # overlap ml. ikke-validerede og validerede
    df[which(!df$HourDK %in% df_recent$HourDK),]) # resten af de validerede observationer
  
  # rydder op
  
  remove(df, df_nv, df_recent, x, temp)
  

  
  
  
  
  

# Med data på plads: Her begynder det sjove! ------------------------------

  df <- df_samlet # tager kopi af data
  
  #To nye variable
  df$WindPower <- df$OffshoreWindPower+df$OnshoreWindPower
  df$Year <- format(as.Date(df$HourDK), format="%Y")
  
  min(df$HourDK[!is.na(df$OffshoreWindPower)]) # Vinddata starter 1.1.2011
  min(df$HourDK[!is.na(df$GrossCon)]) # Forbrusdata starter 1.1.2010
  min(df$HourDK[!is.na(df$SolarPowerProd)]) # Vinddata starter 1.1.2011

  forbrug <- aggregate(GrossCon/1e+06 ~ Year, df, sum) # bruttoforbrug per aar
  vind <- aggregate(WindPower/1e+06 ~ Year, df, sum) # bruttoforbrug per aar
  sol <- aggregate(SolarPowerProd/1e+06 ~Year, df, sum)
  
  vind_andel <- cbind(forbrug[-1,], vind[-1])
  names(vind_andel)[2:3] <- c("Bruttoforbrug, TWh", "Vind, TWh")
  vind_andel$"andel (%)" <- vind_andel$`Vind, TWh`*100/vind_andel$`Bruttoforbrug, TWh`
  
  sol_andel <- cbind(forbrug[4:8,], sol[-1])
  names(sol_andel)[2:3] <- c("Bruttoforbrug, TWh", "Sol, TWh")
  sol_andel$"andel (%)" <- sol_andel$`Sol, TWh`*100/sol_andel$`Bruttoforbrug, TWh`
  
  #Rydder op
  
  remove(vind, sol, forbrug)
  
  
  # Undersøger vindproduktion pr. måned
  
  df_samlet$month <- format(as.Date(df_samlet$HourDK), "%m")
  df_samlet$year <-  format(as.Date(df_samlet$HourDK), "%Y")
  df_samlet <- df_samlet[df_samlet$year==2017,]
  
  df_samlet$Wind <- df_samlet$OnshoreWindPower+df_samlet$OffshoreWindPower
  
  maaned <- aggregate(Wind/1e+06 ~ month, df_samlet, sum)
  
  
  
  
  
