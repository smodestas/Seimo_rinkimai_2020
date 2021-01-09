rm(list = objects())

#### Setting up Selenium ####

rD <- rsDriver(browser="chrome", port=4538L, verbose=F,  chromever = "85.0.4183.87")
remDr <- rD[["client"]]


#### Scraping data #### 

remDr$navigate("https://rezultatai.vrk.lt/?srcUrl=/rinkimai/1104/1/1746/rezultatai/lt/rezultataiVienm.html")

html <- remDr$getPageSource()[[1]]


#### Vienmanda?i? duomenys #####

apygardos <- read_html(html) %>% html_nodes("#pub-content > div:nth-child(2) > table:nth-child(9) > tbody > tr > td > a")  %>% html_text
apygardos_urls <-  read_html(html) %>% html_nodes("#pub-content > div:nth-child(2) > table:nth-child(9) > tbody > tr > td > a")  %>% html_attr("href") 

rezultatai_apygardos <- vector("list", length = length(apygardos))

names(rezultatai_apygardos) <- rep("NA", length(rezultatai_apygardos))

rezultatai_apylinkes <- vector("list", length = length(apygardos))


for (i in 1:length(apygardos)){
  
  if (!(names(rezultatai_apygardos)[i] %in% apygardos)) {
    
    print(paste("Scraping", apygardos[i], "..."))
    
    ok <- FALSE
    counter <- 0
    
    while (ok == FALSE & counter <= 5) {
      
      counter <- counter + 1
      
      out <- tryCatch({
        
        remDr$navigate(paste0("https://rezultatai.vrk.lt/",apygardos_urls[i]))
        Sys.sleep(abs(rnorm(1,1,sd = 1)))
        

        
        html <- remDr$getPageSource()[[1]]
        rez_apygardoje <-  read_html(html) %>% 
          html_nodes("#pub-content > 
                     div:nth-child(2) > 
                     table:nth-child(11)") %>% html_table(fill = T) %>% as.data.frame
        
        if (nrow(rez_apygardoje) == 0){
          
          rez_apygardoje <-  read_html(html) %>% 
            html_nodes("#pub-content > 
                     div:nth-child(2) > 
                     table:nth-child(12)") %>% html_table(fill = T) %>% as.data.frame
          
        }
        
        
        names(rez_apygardoje) <-  rez_apygardoje[1,] %>% str_replace_all("%","") %>% 
                                  str_squish %>% str_replace_all("\\s","_")
        
        rez_apygardoje <- rez_apygardoje[-c(1,nrow(rez_apygardoje)),]
        
        rez_apygardoje <- rez_apygardoje %>% select(-"NA") 
        
        rez_apygardoje$Apygarda <- rep(apygardos[i], length(rez_apygardoje[,1]))
        
        rezultatai_apygardos[[i]] <- rez_apygardoje
        
        rm(rez_apygardoje)
      
        
        apylinkes <- read_html(html) %>% 
          html_nodes("#pub-content > div:nth-child(2) > 
                     table:nth-child(16) > tbody >
                     tr>
                     td > a")  %>% 
          html_text %>% str_squish()
        
        if (length(apylinkes) == 0){
          
          
          apylinkes <- read_html(html) %>% 
            html_nodes("#pub-content > div:nth-child(2) > 
                     table:nth-child(17) > tbody >
                     tr>
                     td > a")  %>% 
            html_text %>% str_squish()
          
        }
        
        
        apylinkes_url <- read_html(html) %>%
          html_nodes("#pub-content > div:nth-child(2) > 
                     table:nth-child(16) > tbody >
                     tr>
                     td > a")  %>% 
          html_attr("href")
        
        if (length(apylinkes_url) == 0){
          
          
          apylinkes_url <- read_html(html) %>% 
            html_nodes("#pub-content > div:nth-child(2) > 
                     table:nth-child(17) > tbody >
                     tr>
                     td > a")  %>% 
            html_attr("href") %>% str_squish()
          
        }
        
        rezultatai_apylinkes_ls <- vector("list", length = length(apylinkes))

        
        for (k in 1:length(apylinkes)){
          
          if ((k%%5 == 0)){
            
            print("Sleeping")
            Sys.sleep(rnorm(1,18,sd = 4))
            
          }
          
          
          remDr$navigate(paste0("https://rezultatai.vrk.lt/",apylinkes_url[k]))
          
          Sys.sleep(1)
          
          html <- remDr$getPageSource()[[1]]
          
          
          rez_apylinkeje <-  read_html(html) %>% 
            html_nodes("#pub-content > div:nth-child(2) > 
                       table:nth-child(8)") %>% 
            html_table(fill = T) %>% as.data.frame
          
          
          
          names(rez_apylinkeje) <-  rez_apylinkeje[1,] %>% str_replace_all("%","") %>% 
                                    str_squish %>% str_replace_all("\\s","_")
          
          rez_apylinkeje <- rez_apylinkeje[-c(1,nrow(rez_apylinkeje)),]
          
          if ("NA" %in% colnames(rez_apylinkeje)){
            
            rez_apylinkeje <- rez_apylinkeje %>% select(-"NA")
            
          }
          
          rez_apylinkeje$apylinke <- rep(apylinkes[k], length(rez_apylinkeje[,1]))
          
          
          rezultatai_apylinkes_ls[[k]] <- rez_apylinkeje
          
          rm(rez_apylinkeje)
          
          
          
        }
        
        rezultatai_apylinkes[[i]] <- rezultatai_apylinkes_ls %>% bind_rows
        
      },
      
      error = function(e) {
        Sys.sleep(2)
        e
      })
      if ("error" %in% class(out)) {
        cat(".")
      } else {
        ok <- TRUE
        cat(" Done.")
      }
    }
    cat("\n")
    names(rezultatai_apygardos)[[i]] <- apygardos[i]
  }
}




vienmandat_apygard <- rezultatai_apygardos %>% bind_rows


vienmandat_apyl <- rezultatai_apylinkes %>%  bind_rows


vienmandat <- full_join(vienmandat_apygard,vienmandat_apyl, by = c("Kandidatas", "I?k?l?"),
                        suffix = c("_apygardoje", "_apylinkeje"))


vienmandat$Apygarda <- vienmandat$Apygarda %>% as.character %>% str_squish
vienmandat$apylinke <- vienmandat$apylinke %>% as.character %>% str_squish


saveRDS(vienmandat,file = "vienmandat.RDS")

##### daugiamandat? ##### 

remDr$navigate("https://rezultatai.vrk.lt/?srcUrl=/rinkimai/1104/1/1746/rezultatai/lt/rezultataiDaugmVrt.html")

html <- remDr$getPageSource()[[1]]

daugiamandat_bendr <- read_html(html) %>%
                      html_nodes("#pub-content > div:nth-child(2) > table:nth-child(4)") %>% 
                      html_table(fill = T)

apygardos <- read_html(html) %>% 
  html_nodes("#pub-content > div:nth-child(2) > table:nth-child(7) > tbody > tr > td > b > a")  %>% html_text

apygardos_urls <-  read_html(html) %>% html_nodes("#pub-content > div:nth-child(2) > 
                                                  table:nth-child(7) > tbody > tr > td > b > a")  %>% html_attr("href") 


rezultatai_apygardos <- vector("list", length = length(apygardos))

names(rezultatai_apygardos) <- rep("NA", length(rezultatai_apygardos))

rezultatai_apylinkes <- vector("list", length = length(apygardos))


for (i in 1:length(apygardos)){
  
  if (!(names(rezultatai_apygardos)[i] %in% apygardos)) {
    
    print(paste("Scraping", apygardos[i], "..."))
    
    ok <- FALSE
    counter <- 0
    
    while (ok == FALSE & counter <= 5) {
      
      counter <- counter + 1
      
      out <- tryCatch({
        
        remDr$navigate(paste0("https://rezultatai.vrk.lt/",apygardos_urls[i]))
        Sys.sleep(abs(rnorm(1,1,sd = 1)))
        
        html <- remDr$getPageSource()[[1]]
       
        rez_apygardoje <-  read_html(html) %>% 
          html_nodes("#pub-content >
                      div:nth-child(2) > 
                      table:nth-child(7)") %>% html_table(fill = T) %>% as.data.frame
        
        
        names(rez_apygardoje) <-  rez_apygardoje[1,] %>% str_replace_all("%|[:punct:]","") %>% 
          str_squish %>% str_replace_all("\\s","_")
        
        rez_apygardoje <- rez_apygardoje[-c(1,nrow(rez_apygardoje)),]
        
        rez_apygardoje$Apygarda <- rep(apygardos[i], length(rez_apygardoje[,1]))

        rezultatai_apygardos[[i]] <- rez_apygardoje
        
        rm(rez_apygardoje)
        
    
        apylinkes <- read_html(html) %>% 
          html_nodes("#pub-content > div:nth-child(2) >
                     table:nth-child(13) > tbody >
                     tr > td > b > a")  %>% 
          html_text %>% str_squish() 
        
        apylinkes_url <- read_html(html) %>%
          html_nodes("#pub-content > div:nth-child(2) >
                     table:nth-child(13) > tbody >
                     tr > td > b > a") %>% html_attr("href")
        
        rezultatai_apylinkes_ls <- vector("list", length = length(apylinkes))
        
        
        for (k in 1:length(apylinkes)){
          
          if ((k%%4 == 0)){
            
            print("Sleeping")
            Sys.sleep(rnorm(1,18,sd = 4))
            
          }
          
          
          remDr$navigate(paste0("https://rezultatai.vrk.lt/",apylinkes_url[k]))
          
          Sys.sleep(1)
          
          html <- remDr$getPageSource()[[1]]
          
          
          rez_apylinkeje <-  read_html(html) %>% 
            html_nodes("#pub-content > div:nth-child(2) > table:nth-child(8)") %>% 
            html_table(fill = T) %>% as.data.frame
          
          
          
          names(rez_apylinkeje) <-  rez_apylinkeje[1,] %>% str_replace_all("%|[:punct:]","") %>% 
            str_squish %>% str_replace_all("\\s","_")
          
          rez_apylinkeje <- rez_apylinkeje[-c(1,nrow(rez_apylinkeje)),]
          
          if ("NA" %in% colnames(rez_apylinkeje)){
            
            rez_apylinkeje <- rez_apylinkeje %>% select(-"NA")
            
          }
          
          rez_apylinkeje$apylinke <- rep(apylinkes[k], nrow(rez_apylinkeje))
          rez_apylinkeje$Apygarda <- apygardos[i]
          
          
          rezultatai_apylinkes_ls[[k]] <- rez_apylinkeje
          
          rm(rez_apylinkeje)
          
        }
        
        rezultatai_apylinkes[[i]] <- rezultatai_apylinkes_ls %>% bind_rows
        
      },
      
      error = function(e) {
        Sys.sleep(2)
        e
      })
      if ("error" %in% class(out)) {
        cat(".")
      } else {
        ok <- TRUE
        cat(" Done.")
      }
    }
    cat("\n")
    names(rezultatai_apygardos)[[i]] <- apygardos[i]
  }
}


daugiamandat_apygard <- rezultatai_apygardos %>% bind_rows
daugiamandat_apylink <- rezultatai_apylinkes %>% bind_rows

daugiamandat <- full_join(daugiamandat_apygard %>% select(-VRK_suteiktas_Nr),
                          daugiamandat_apylink %>% select(-VRK_suteiktas_Nr), by = c("Pavadinimas","Apygarda"),
                          suffix = c("_apygardoje", "_apylinkeje"))

saveRDS(daugiamandat,"daugiamandat.RDS")

daugiamandat <- full_join(daugiamandat,daugiamandat_apygard, by = c("partija"))
daugiamandat <- full_join(daugiamandat,daugiamandat_apyl, by = c("partija"),)

daugiamandat_bendr <- daugiamandat_bendr %>% bind_rows()

names(daugiamandat_bendr) <- daugiamandat_bendr[1,]

daugiamandat_bendr <- daugiamandat_bendr[-c(1,nrow(daugiamandat_bendr)),]


saveRDS(daugiamandat_bendr,"bendras.RDS")

remDr$close()
