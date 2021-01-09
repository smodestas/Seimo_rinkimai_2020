
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


daugiamandat <- daugiamandat %>% select(-Pirmumobalsai_apygardoje,-Pirmumobalsai_apylinkeje)

#### Saving data ####


saveRDS(bendras, file = "./processed-data/bendras.RDS")
saveRDS(daugiamandat, file = "./processed-data/daugiamandat.RDS")
saveRDS(vienmandat, file = "./processed-data/vienmandat.RDS")



#### Merging election data with spatial data ####



#### reading shapefile 

zemelapis_apylinkiu <- st_read("./raw-data/Apylinkiu_ribos_2020/Apylinkės_2020.shp")
zemelapis_apygardu <- st_read("./raw-data/Apygardu_ribos_2020/Apygardos_2020.shp")


zemelapis_apylinkiu_df <- st_transform(zemelapis_apylinkiu, '+proj=longlat +datum=WGS84')
zemelapis_apygardu_df <- st_transform(zemelapis_apygardu, '+proj=longlat +datum=WGS84')



names(zemelapis_apylinkiu_df)[1:5] <- c("APL_ID","APL_NUM","APL_PAV","APG_NUM","APG_PAV")
names(zemelapis_apygardu_df)[3:4] <- c("APG_NUM","APG_PAV")

zemelapis_apylinkiu_df <- zemelapis_apylinkiu_df %>% arrange(APG_NUM,APL_NUM)
zemelapis_apygardu_df <- zemelapis_apygardu_df %>% arrange(APG_NUM)



zemelapis_apylinkiu_df$APL_PAV <-  str_replace_all(zemelapis_apylinkiu_df$APL_PAV,".\\s",".")  



rm(zemelapis_apylinkiu,zemelapis_apygardu)

#### merging map data with election data ####


zemelapis_apylinkiu_df_vnmd <- zemelapis_apylinkiu_df %>% left_join(vienmandat %>% select(Kandidatas,Iškėlė, 
                                                                                          iš_viso_apylinkeje,
                                                                                          nuo_dalyvavusių_rinkėjų_apylinkeje,
                                                                                          Apylinkes_nr,
                                                                                          apylinke,
                                                                                          Apygardos_nr,party_color),
                                                                    by = c("APG_NUM" = "Apygardos_nr",
                                                                           "APL_NUM" = "Apylinkes_nr",
                                                                           "APL_PAV" = "apylinke"))

zemelapis_apygardu_df_vnmd <- zemelapis_apygardu_df %>% left_join(vienmandat %>% select(Kandidatas,Iškėlė, 
                                                                                        iš_viso_apygardoje,
                                                                                        nuo_dalyvavusių_rinkėjų_apygardoje,
                                                                                        Apygardos_nr,
                                                                                        party_color),
                                                                  by = c("APG_NUM" = "Apygardos_nr")) 




zemelapis_apylinkiu_df_dgmd <- zemelapis_apylinkiu_df %>% left_join(daugiamandat %>% select(Pavadinimas,iš_viso_apylinkeje,
                                                                                            nuo_galiojančių_biuletenių,
                                                                                            Apygarda,
                                                                                            Apygardos_nr,
                                                                                            apylinke,
                                                                                            Apylinkes_nr,
                                                                                            party_color),
                                                                    by = c("APG_NUM" = "Apygardos_nr",
                                                                           "APL_NUM" = "Apylinkes_nr",
                                                                           "APL_PAV" = "apylinke"))


daugiamandat$Apygardos_nr <- daugiamandat$Apygardos_nr %>% as.integer
zemelapis_apygardu_df_dgmd <- zemelapis_apygardu_df %>% inner_join(daugiamandat %>% select(Pavadinimas,iš_viso_apygardoje,
                                                                                           nuo_dalyvavusių_rinkėjų,
                                                                                           Apygarda,
                                                                                           Apygardos_nr,
                                                                                           party_color),
                                                                   by = c("APG_NUM" = "Apygardos_nr")) 


names(zemelapis_apygardu_df_dgmd)[6:7] <- c("is_viso_apygardoje","nuo_dalyvavusiu_rinkeju")
#### Saving spatial data #### 

saveRDS(zemelapis_apygardu_df_dgmd, "processed-data/zemelapis_apygardu_df_dgmd.RDS")
saveRDS(zemelapis_apylinkiu_df_dgmd, "processed-data/zemelapis_apylinkiu_df_dgmd.RDS")
saveRDS(zemelapis_apygardu_df_vnmd, "processed-data/zemelapis_apygardu_df_vnmd.RDS")
saveRDS(zemelapis_apylinkiu_df_vnmd, "processed-data/zemelapis_apylinkiu_df_vnmd.RDS")

