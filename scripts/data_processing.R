rm(list = objects())


p_needed <-
  c("dplyr", "ggplot2",
    "stringr", "rlist",
    "tools", "tidyr")

packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]

if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
sapply(p_needed, require, character.only = TRUE)


rm(list = objects())
### Loading data ####

vienmandat <- readRDS("./raw-data/srapped-data/vienmandat.RDS")
daugiamandat <- readRDS("./raw-data/srapped-data/daugiamandat.RDS")
bendras <-  readRDS("./raw-data/srapped-data/bendras.RDS")

#### Cleaning data ####


vienmandat$Kandidatas <- vienmandat$Kandidatas %>% tolower %>% toTitleCase


vienmandat[,c(3:7,10:15)] <- vienmandat[,c(3:7,10:15)] %>%  
  mutate_at(.vars = vars(everything()),
            .funs = ~ str_replace(., pattern = ",", replacement = ".") %>%
              as.numeric)


daugiamandat[,c(3:7,11:15)] <- daugiamandat[,c(3:7,11:15)] %>%  
  mutate_at(.vars = vars(everything()),
            .funs = ~ str_replace(., pattern = ",", replacement = ".") %>%
              as.numeric)

vienmandat <- vienmandat %>% mutate(Apygardos_nr = str_extract(Apygarda,"[:alnum:]{1,3}[.]") %>% 
                                      str_remove("[:punct:]") %>% as.numeric,
                                    Apylinkes_nr = str_extract(apylinke,"[:alnum:]{1,3}[.]") %>%
                                      str_remove("[:punct:]") %>% as.numeric) %>% 
  mutate_at(.vars = vars(Apygarda,apylinke),
            .funs = ~ str_replace(., pattern = "[:alnum:]{1,3}[.]", replacement = "") %>% 
              str_squish)


daugiamandat <- daugiamandat %>% mutate(Apygardos_nr = str_extract(Apygarda,"[:alnum:]{1,3}[.]") %>% 
                                          str_remove("[:punct:]") %>% as.numeric,
                                        Apylinkes_nr = str_extract(apylinke,"[:alnum:]{1,3}[.]") %>%
                                          str_remove("[:punct:]") %>% as.numeric) %>% 
  mutate_at(.vars = vars(Apygarda,apylinke),
            .funs = ~ str_replace(., pattern = "[:alnum:]{1,3}[.]", replacement = "") %>% 
              str_squish)



vienmandat <- vienmandat %>%  replace_na(list(apylinkėse = 0,
                                              paštu_apylinkeje = 0,
                                              iš_viso_apylinkeje = 0,
                                              nuo_galiojančių_biuletenių_apylinkeje = 0,
                                              nuo_dalyvavusių_rinkėjų_apylinkeje = 0,
                                              nuo_dalyvavusių_rinkėjų_apylinkeje = 0,
                                              nuo_galiojančių_biuletenių_apygardoje = 0))


daugiamandat$party_color <- ifelse(daugiamandat$Pavadinimas == "Tėvynės sąjunga – Lietuvos krikščionys demokratai","green4",
                                   ifelse(daugiamandat$Pavadinimas == "Lietuvos valstiečių ir žaliųjų sąjunga", "chartreuse1",
                                          ifelse(daugiamandat$Pavadinimas == "Lietuvos socialdemokratų partija", "red",
                                                 ifelse(daugiamandat$Pavadinimas == "Lietuvos Respublikos liberalų sąjūdis","orange",
                                                        ifelse(daugiamandat$Pavadinimas == "Lietuvos lenkų rinkimų akcija - Krikščioniškų šeimų sąjunga","red4",
                                                               ifelse(daugiamandat$Pavadinimas == "Darbo partija", "navy",
                                                                      ifelse(daugiamandat$Pavadinimas == "Laisvės partija", "maroon1",
                                                                             ifelse(daugiamandat$Pavadinimas == "Lietuvos socialdemokratų darbo partija","tomato","grey"))))))))



vienmandat$party_color <- ifelse(vienmandat$Iškėlė == "Tėvynės sąjunga – Lietuvos krikščionys demokratai","green4",
                                 ifelse(vienmandat$Iškėlė == "Lietuvos valstiečių ir žaliųjų sąjunga", "chartreuse1",
                                        ifelse(vienmandat$Iškėlė == "Lietuvos socialdemokratų partija", "red",
                                               ifelse(vienmandat$Iškėlė == "Lietuvos Respublikos liberalų sąjūdis","orange",
                                                      ifelse(vienmandat$Iškėlė == "Lietuvos lenkų rinkimų akcija - Krikščioniškų šeimų sąjunga","red4",
                                                             ifelse(vienmandat$Iškėlė == "Darbo partija", "navy",
                                                                    ifelse(vienmandat$Iškėlė == "Laisvės partija", "maroon1",
                                                                           ifelse(vienmandat$Iškėlė == "Lietuvos socialdemokratų darbo partija","tomato","grey"))))))))





vienmandat$apylinke <-  str_replace_all(vienmandat$apylinke,".\\s",".")  
daugiamandat$apylinke <-  str_replace_all(daugiamandat$apylinke,".\\s",".") 


vienmandat$Apygarda <- str_replace(vienmandat$Apygarda, "–","-")
daugiamandat$Apygarda <-  str_replace(daugiamandat$Apygarda, "–","-")


bendras <- bendras %>% select(-`VRK suteiktas Nr.`,-`Partija, koalicija`)

bendras$Pavadinimas <- bendras$Pavadinimas %>% tolower %>% toTitleCase
names(bendras) <- c("partija",  "apylinkėse",
                    "paštu", "iš_viso" ,"proc_nuo_balsavusiu",
                    "mandatai")

#### Saving data ####


saveRDS(bendras, file = "./processed-data/bednras.RDS")
saveRDS(daugiamandat, file = "./processed-data/daugiamandat.RDS")
saveRDS(vienmandat, file = "./processed-data/vienmandat.RDS")

