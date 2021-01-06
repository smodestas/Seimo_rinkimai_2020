rm(list = objects())


setwd("C:/Users/modes/Desktop/R/Rinkimai_2020/")

p_needed <-
  c("dplyr","ggplot2","stringr","rlist","tools",
    "tidyr","rJava","leaflet","sf",
    "rmapshaper", "shiny","DT")


packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]

if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
sapply(p_needed, require, character.only = TRUE)


### Loading data ####

vienmandat <- readRDS("vienmandat.RDS")
daugiamandat <- readRDS("daugiamandat.RDS")
bendras <-  readRDS("bendras.RDS")

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



vienmandat <- vienmandat %>%  replace_na(list(apylinkëse = 0,
                                              paðtu_apylinkeje = 0,
                                              ið_viso_apylinkeje = 0,
                                              nuo_galiojanèiø_biuleteniø_apylinkeje = 0,
                                              nuo_dalyvavusiø_rinkëjø_apylinkeje = 0,
                                              nuo_dalyvavusiø_rinkëjø_apylinkeje = 0,
                                              nuo_galiojanèiø_biuleteniø_apygardoje = 0))


daugiamandat$party_color <- ifelse(daugiamandat$Pavadinimas == "Tëvynës sàjunga ??? Lietuvos krikðèionys demokratai","green4",
                               ifelse(daugiamandat$Pavadinimas == "Lietuvos valstieèiø ir þaliøjø sàjunga", "chartreuse1",
                                      ifelse(daugiamandat$Pavadinimas == "Lietuvos socialdemokratø partija", "red",
                                             ifelse(daugiamandat$Pavadinimas == "Lietuvos Respublikos liberalø sàjûdis","orange",
                                                    ifelse(daugiamandat$Pavadinimas == "Lietuvos lenkø rinkimø akcija - Krikðèioniðkø ðeimø sàjunga","red4",
                                                           ifelse(daugiamandat$Pavadinimas == "Darbo partija", "navy",
                                                                  ifelse(daugiamandat$Pavadinimas == "Laisvës partija", "maroon1",
                                                                         ifelse(daugiamandat$Pavadinimas == "Lietuvos socialdemokratø darbo partija","tomato","grey"))))))))



vienmandat$party_color <- ifelse(vienmandat$Iðkëlë == "Tëvynës sàjunga ??? Lietuvos krikðèionys demokratai","green4",
                                 ifelse(vienmandat$Iðkëlë == "Lietuvos valstieèiø ir þaliøjø sàjunga", "chartreuse1",
                                        ifelse(vienmandat$Iðkëlë == "Lietuvos socialdemokratø partija", "red",
                                               ifelse(vienmandat$Iðkëlë == "Lietuvos Respublikos liberalø sàjûdis","orange",
                                                      ifelse(vienmandat$Iðkëlë == "Lietuvos lenkø rinkimø akcija - Krikðèioniðkø ðeimø sàjunga","red4",
                                                             ifelse(vienmandat$Iðkëlë == "Darbo partija", "navy",
                                                                    ifelse(vienmandat$Iðkëlë == "Laisvës partija", "maroon1",
                                                                           ifelse(vienmandat$Iðkëlë == "Lietuvos socialdemokratø darbo partija","tomato","grey"))))))))





vienmandat$apylinke <-  str_replace_all(vienmandat$apylinke,".\\s",".")  
daugiamandat$apylinke <-  str_replace_all(daugiamandat$apylinke,".\\s",".") 


vienmandat$Apygarda <- str_replace(vienmandat$Apygarda, "???","-")
daugiamandat$Apygarda <-  str_replace(daugiamandat$Apygarda, "???","-")

###### 


#### reading shapefile 

zemelapis_apylinkiu <- st_read("Apylinkiu_ribos_2020/Apylinkës_2020.shp")
zemelapis_apygardu <- st_read("Apygardu_ribos_2020/Apygardos_2020.shp")


zemelapis_apylinkiu_df <- st_transform(zemelapis_apylinkiu, '+proj=longlat +datum=WGS84')
zemelapis_apygardu_df <- st_transform(zemelapis_apygardu, '+proj=longlat +datum=WGS84')



names(zemelapis_apylinkiu_df)[1:5] <- c("APL_ID","APL_NUM","APL_PAV","APG_NUM","APG_PAV")
names(zemelapis_apygardu_df)[3:4] <- c("APG_NUM","APG_PAV")

zemelapis_apylinkiu_df <- zemelapis_apylinkiu_df %>% arrange(APG_NUM,APL_NUM)
zemelapis_apygardu_df <- zemelapis_apygardu_df %>% arrange(APG_NUM)



zemelapis_apylinkiu_df$APL_PAV <-  str_replace_all(zemelapis_apylinkiu_df$APL_PAV,".\\s",".")  



rm(zemelapis_apylinkiu,zemelapis_apygardu)

#### Þemëlapis #####


zemelapis_apylinkiu_df_vnmd <- zemelapis_apylinkiu_df %>% left_join(vienmandat %>% select(-3:-8),
                                                                    by = c("APG_NUM" = "Apygardos_nr",
                                                                           "APL_NUM" = "Apylinkes_nr",
                                                                           "APL_PAV" = "apylinke"))

zemelapis_apygardu_df_vnmd <- zemelapis_apygardu_df %>% inner_join(vienmandat %>% select(-8,-10:-16,-18),
                                                                   by = c("APG_NUM" = "Apygardos_nr")) 




zemelapis_apylinkiu_df_dgmd <- zemelapis_apylinkiu_df %>% left_join(daugiamandat %>% select(-2:-9),
                                                                    by = c("APG_NUM" = "Apygardos_nr",
                                                                           "APL_NUM" = "Apylinkes_nr",
                                                                           "APL_PAV" = "apylinke"))


daugiamandat$Apygardos_nr <- daugiamandat$Apygardos_nr %>% as.integer
zemelapis_apygardu_df_dgmd <- zemelapis_apygardu_df %>% inner_join(daugiamandat %>% select(-2,-8,-9,-10:-15),
                                                                   by = c("APG_NUM" = "Apygardos_nr")) 






##########


ui <- navbarPage("Seimo Rinkimai 2020",id="nav",
                 
                 tabPanel('Rinkimø rezultatai þemëlapyje',
                          div(class = "outer",
                              tags$style(type = "text/css")),
                          leafletOutput('map', height = "95vh"),
                          
                          absolutePanel(id = "controls_vienmand", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        h2("Seimo Rinkimai 2020"),
                                        radioButtons("tipas","Rezultatai:",
                                                     c("Vienmandatëje" = "vienmandat",
                                                       "Daugiamandatëje" = "daugiamandat"),
                                                     inline = T),
                                        selectInput("apygarda", "Apygarda",
                                                    choices = c("Visos",vienmandat$Apygarda %>% unique)),
                                        conditionalPanel(
                                          condition = "input.tipas == 'vienmandat'",
                                          radioButtons("rez_vienmandat", "",
                                                       c("Kandidatai surinkæ daugiausia balsø" = "nugaletojas_vienmandat",
                                                         "Kandidatø rezultatai apylinkëse" = "kandidatai_vienmandat"),
                                                       selected = "nugaletojas_vienmandat"),
                                          conditionalPanel(
                                            condition = "input.rez_vienmandat == 'kandidatai_vienmandat'",
                                            uiOutput("kandidatas_vienmand")
                                          )
                                          
                                        ),
                                        conditionalPanel(
                                          condition = "input.tipas == 'daugiamandat'",
                                          radioButtons("rez_daugiamandat", "Rezultatai:",
                                                       c("Daugiausia balsø surinkusi partija apylinkëse" = "nugaletojas_daugiamandat",
                                                         "Partijos rezultatai apylinkëse" = "kandidatai_daugiamandat"),
                                                       selected = "nugaletojas_daugiamandat"),
                                          conditionalPanel(
                                            condition = "input.rez_daugiamandat == 'kandidatai_daugiamandat'",
                                            uiOutput("kandidatas_daugiamandat")
                                          )
                                          
                                          
                                        )
                          
                          
                          )
                          
                 ),
                 tabPanel('Rinkimø rezultatai lentelëje',
                          div(class = "outer",
                              tags$style(type = "text/css")),

                          fluidRow(
                            column(4,
                                   selectInput("apygard",
                                               "Apygarda:",
                                               c("Visos:",
                                                 unique(as.character(daugiamandat$Apygarda))))
                            ),
                            column(4,
                                   uiOutput("apyl")
                            ),
                            column(4,
                                   selectInput("partija",
                                               "Pavadinimas:",
                                               c("Visos:",
                                                 unique(as.character(daugiamandat$Pavadinimas))))
                            )
                          ),
                          # Create a new row for the table.
                          DT::dataTableOutput("table")
                 )
                 

                 
)






server <- function(input, output, session) {
  output$kandidatas_vienmand <- renderUI({
    kandidatai <- zemelapis_apylinkiu_df_vnmd %>% as.data.frame %>%
      filter(APG_PAV == input$apygarda) %>%
      select(Kandidatas) %>% pull(Kandidatas)
    selectInput("kandidatas_vienmand", "Kandidatas apylinkeje", choices = kandidatai)
  })
  
  
  
  
  output$kandidatas_daugiamandat <- renderUI({
    kandidatai <- zemelapis_apylinkiu_df_dgmd %>% as.data.frame %>%
      filter(APG_PAV == input$apygarda) %>%
      select(Pavadinimas) %>% pull(Pavadinimas)
    selectInput("kandidatas_daugiamandat", "Partija", choices = kandidatai)
  })
  
  
  output$apyl <- renderUI({
    apylinkes <- daugiamandat  %>%
      filter(Apygarda == input$apygard) %>%
      pull(apylinke) %>% unique
    selectInput("apyl", "Apylinkë", choices = c("Visos:", apylinkes))
  })
  
  
  
  #### Þemëlapis ####
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addPolygons(
        data = zemelapis_apygardu_df,
        fillColor = "transparent",
        weight = 1,
        opacity = 1,
        color = "black",
        group = "Apygarda"
      ) %>%
      addPolygons(
        data = zemelapis_apygardu_df,
        weight = 5,
        opacity = 2,
        color = "black",
        fillColor = "transparent",
        group = "Apygardos_ribos"
      ) %>%
      
      addPolygons(
        fillColor = "transparent",
        data = zemelapis_apylinkiu_df,
        weight = 1,
        opacity = 1,
        color = "black",
        fillOpacity = .1,
        group = "Apylinkes"
      ) %>%
      groupOptions("Apylinkes", zoomLevels = 10:20) %>%
      groupOptions("Apygardos_ribos", zoomLevels = 10:20) %>%
      groupOptions("Apygarda", zoomLevels = 1:9) %>%
      
      addProviderTiles(providers$CartoDB.Positron)
    
  })
  
  observe({
    if (input$tipas == "vienmandat") {
      if (input$rez_vienmandat == "nugaletojas_vienmandat") {
        nugaletojas_apylinkese <- zemelapis_apylinkiu_df_vnmd %>%
          group_by(APL_ID) %>%
          filter(ið_viso_apylinkeje == max(ið_viso_apylinkeje)) %>%
          mutate(
            Kandidatas = ifelse(n() > 1, paste(Kandidatas, collapse = ", "), Kandidatas),
            party_color = ifelse(n() > 1, "grey", party_color)
          ) %>% slice(1) %>% ungroup
        
        
        nugaletojas_apygardose <- zemelapis_apygardu_df_vnmd %>%
          group_by(APG_NUM) %>%
          filter(ið_viso_apygardoje == max(ið_viso_apygardoje)) %>%
          slice(1) %>%
          ungroup
        
        colors <- colorFactor(
          palette = vienmandat$party_color %>% unique,
          vienmandat$party_color,
          ordered = T
        )
        
        
        
        labels_apylinkiu <- sprintf(
          "<strong>%s %s apylinkë</strong><br/>%s ",
          nugaletojas_apylinkese$APL_NUM,
          nugaletojas_apylinkese$APL_PAV,
          nugaletojas_apylinkese$Kandidatas
        ) %>%
          lapply(htmltools::HTML)
        
        labels_apygardu <- sprintf(
          "<strong>%s %s apygarda</strong><br/>%s ",
          nugaletojas_apygardose$APG_NUM,
          nugaletojas_apygardose$APG_PAV,
          nugaletojas_apygardose$Kandidatas
        ) %>%
          lapply(htmltools::HTML)
        
        
        leafletProxy("map") %>%
          clearGroup(
            c(
              "Apygardos_ribos_by_participant",
              "Apylinkes_by_participant",
              "Apygardos_ribos_winner",
              "Apylinkes_winner",
              "Apygarda_winner"
            )
          ) %>%
          addPolygons(
            data = nugaletojas_apygardose,
            fillColor = ~ colors(party_color),
            weight = 1,
            opacity = 1,
            layerId = zemelapis_apygardu_df$APG_PAV,
            color = "black",
            highlight = highlightOptions(
              weight = 5,
              color = "white",
              fillOpacity = 1,
              bringToFront = TRUE
            ),
            label = labels_apygardu,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            ),
            group = "Apygarda_winner"
          ) %>%
          
          addPolygons(
            data = nugaletojas_apygardose,
            weight = 5,
            opacity = 2,
            color = "black",
            fillColor = "transparent",
            group = "Apygardos_ribos_winner"
          ) %>%
          
          addPolygons(
            fillColor = ~ colors(party_color),
            data = nugaletojas_apylinkese,
            weight = 1,
            layerId = nugaletojas_apylinkese$APL_ID,
            opacity = 1,
            color = "black",
            fillOpacity = .1,
            highlight = highlightOptions(
              weight = 5,
              color = "white",
              fillOpacity = .1,
              bringToFront = TRUE
            ),
            label = labels_apylinkiu,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            ),
            group = "Apylinkes_winner"
          ) %>%
          
          groupOptions("Apylinkes_winner", zoomLevels = 10:20) %>%
          groupOptions("Apygardos_ribos_winner", zoomLevels = 10:20) %>%
          groupOptions("Apygarda_winner", zoomLevels = 1:9)
      }
      
      
      
    }
    
    if (input$tipas == "daugiamandat") {
      if (input$rez_daugiamandat == "nugaletojas_daugiamandat") {
        nugaletojas_apylinkese_daugiamandat <-
          zemelapis_apylinkiu_df_dgmd %>%
          group_by(APL_ID) %>%
          filter(ið_viso_apylinkeje == max(ið_viso_apylinkeje)) %>%
          mutate(
            Pavadinimas = ifelse(n() > 1, paste(Pavadinimas, collapse = ", "), Pavadinimas),
            party_color = ifelse(n() > 1, "grey", party_color)
          ) %>% slice(1) %>% ungroup
        
        
        nugaletojas_apygardose_daugiamandat <-
          zemelapis_apygardu_df_dgmd %>%
          group_by(APG_NUM) %>%
          filter(ið_viso_apygardoje == max(ið_viso_apygardoje)) %>%
          slice(1) %>%
          ungroup
        
        colors <- colorFactor(daugiamandat$party_color %>% unique,
                              daugiamandat$party_color,
                              ordered = T)
        
        
        
        labels_apylinkiu_daugiamandat <- sprintf(
          "<strong>%s %s apylinkë</strong><br/>%s ",
          nugaletojas_apylinkese_daugiamandat$APL_NUM,
          nugaletojas_apylinkese_daugiamandat$APL_PAV,
          nugaletojas_apylinkese_daugiamandat$Pavadinimas
        ) %>%
          lapply(htmltools::HTML)
        
        labels_apygardu_daugiamandat <- sprintf(
          "<strong>%s %s apygarda</strong><br/>%s ",
          nugaletojas_apygardose_daugiamandat$APG_NUM,
          nugaletojas_apygardose_daugiamandat$APG_PAV,
          nugaletojas_apygardose_daugiamandat$Pavadinimas
        ) %>%
          lapply(htmltools::HTML)
        
        
        leafletProxy("map") %>%
          clearGroup(
            c(
              "Apygardos_ribos_by_participant",
              "Apylinkes_by_participant",
              "Apygardos_ribos_winner",
              "Apylinkes_winner",
              "Apygarda_winner"
            )
          ) %>%
          addPolygons(
            data = nugaletojas_apygardose_daugiamandat,
            fillColor = ~ colors(party_color),
            weight = 1,
            layerId = nugaletojas_apygardose_daugiamandat$APG_PAV,
            opacity = 1,
            color = "black",
            highlight = highlightOptions(
              weight = 5,
              color = "white",
              fillOpacity = 1,
              bringToFront = TRUE
            ),
            label = labels_apygardu_daugiamandat,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            ),
            group = "Apygarda_winner"
          ) %>%
          
          addPolygons(
            data = nugaletojas_apygardose_daugiamandat,
            weight = 5,
            opacity = 2,
            color = "black",
            fillColor = "transparent",
            group = "Apygardos_ribos_winner"
          ) %>%
          
          addPolygons(
            fillColor = ~ colors(party_color),
            data = nugaletojas_apylinkese_daugiamandat,
            layerId = nugaletojas_apylinkese_daugiamandat$APL_ID,
            weight = 1,
            opacity = 1,
            color = "black",
            fillOpacity = .1,
            highlight = highlightOptions(
              weight = 5,
              color = "white",
              fillOpacity = .1,
              bringToFront = TRUE
            ),
            label = labels_apylinkiu_daugiamandat,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            ),
            group = "Apylinkes_winner"
          ) %>%
          
          groupOptions("Apylinkes_winner", zoomLevels = 10:20) %>%
          groupOptions("Apygardos_ribos_winner", zoomLevels = 10:20) %>%
          groupOptions("Apygarda_winner", zoomLevels = 1:9)
      }
      
      
      
      
      
      
    }
    
    
  })
  
  
  observe({
    if (input$tipas == "vienmandat") {
      if (input$rez_vienmandat == "kandidatai_vienmandat") {
        req(input$kandidatas_vienmand)
        
        
        kandidato_rez_apylinkese <- zemelapis_apylinkiu_df_vnmd %>%
          filter(Kandidatas == input$kandidatas_vienmand) %>%
          ungroup
        
        
        kandidato_rez_apygardose <- zemelapis_apygardu_df_vnmd %>%
          group_by(APG_NUM) %>%
          filter(Kandidatas == input$kandidatas_vienmand) %>%
          slice(1) %>%
          ungroup
        
        colors <-
          colorNumeric("Blues",
                       kandidato_rez_apylinkese$nuo_galiojanèiø_biuleteniø_apylinkeje)
        
        
        
        labels_apylinkiu <- sprintf(
          "<strong>%s %s apylinkë</strong><br/>%s&#37",
          kandidato_rez_apylinkese$APL_NUM,
          kandidato_rez_apylinkese$APL_PAV,
          kandidato_rez_apylinkese$nuo_galiojanèiø_biuleteniø_apylinkeje
        ) %>%
          lapply(htmltools::HTML)
        
        
        leafletProxy("map") %>%
          clearGroup(
            c(
              "Apygardos_ribos_winner",
              "Apylinkes_winner",
              "Apygarda_winner",
              "Apygardos_ribos_by_participant",
              "Apylinkes_by_participant"
            )
          ) %>%
          addPolygons(
            data = kandidato_rez_apygardose,
            weight = 5,
            layerId = zemelapis_apygardu_df$APG_PAV,
            opacity = 2,
            color = "black",
            fillColor = "transparent",
            group = "Apygardos_ribos_by_participant"
          ) %>%
          
          addPolygons(
            fillColor = ~ colors(nuo_galiojanèiø_biuleteniø_apylinkeje),
            data = kandidato_rez_apylinkese,
            weight = 1,
            opacity = 1,
            color = "black",
            fillOpacity = .3,
            highlight = highlightOptions(
              weight = 5,
              color = "white",
              fillOpacity = .1,
              bringToFront = TRUE
            ),
            label = labels_apylinkiu,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            ),
            group = "Apylinkes_by_participant"
          )
      }
      
      
    }
    
    if (input$tipas == "daugiamandat") {
      if (input$rez_daugiamandat == "kandidatai_daugiamandat") {
        req(input$kandidatas_daugiamandat)
        
        
        partijos_rez_apylinkese <- zemelapis_apylinkiu_df_dgmd %>%
          filter(Pavadinimas == input$kandidatas_daugiamandat &
                   APG_PAV == input$apygarda) %>%
          ungroup
        
        
        partijos_rez_apygardose <- zemelapis_apygardu_df_dgmd %>%
          group_by(APG_NUM) %>%
          filter(Pavadinimas == input$kandidatas_daugiamandat) %>%
          slice(1)
        ungroup
        
        colors <-
          colorNumeric("Blues",
                       partijos_rez_apylinkese$nuo_galiojanèiø_biuleteniø)
        
        
        
        labels_apylinkiu <- sprintf(
          "<strong>%s %s apylinkë</strong><br/>%s&#37",
          partijos_rez_apylinkese$APL_NUM,
          partijos_rez_apylinkese$APL_PAV,
          partijos_rez_apylinkese$nuo_galiojanèiø_biuleteniø
        ) %>%
          lapply(htmltools::HTML)
        
        
        leafletProxy("map") %>%
          clearGroup(
            c(
              "Apygardos_ribos_winner",
              "Apylinkes_winner",
              "Apygarda_winner",
              "Apygardos_ribos_by_participant",
              "Apylinkes_by_participant"
            )
          ) %>%
          addPolygons(
            data = partijos_rez_apygardose,
            layerId = zemelapis_apygardu_df$APG_PAV,
            weight = 5,
            opacity = 2,
            color = "black",
            fillColor = "transparent",
            group = "Apygardos_ribos_by_participant"
          ) %>%
          
          addPolygons(
            fillColor = ~ colors(nuo_galiojanèiø_biuleteniø),
            data = partijos_rez_apylinkese,
            weight = 1,
            opacity = 1,
            color = "black",
            fillOpacity = .3,
            highlight = highlightOptions(
              weight = 5,
              color = "white",
              fillOpacity = .1,
              bringToFront = TRUE
            ),
            label = labels_apylinkiu,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            ),
            group = "Apylinkes_by_participant"
          )
        
        
        
      }
      
      
      
    }
  })
  
  
  coord <- reactive({
    if (input$apygarda == "Visos") {
      zemelapis_apylinkiu_df_vnmd %>%
        select(geometry) %>% st_coordinates
      
    } else {
      zemelapis_apylinkiu_df_vnmd %>%
        subset(APG_PAV == input$apygarda) %>%
        select(geometry) %>% st_coordinates
      
      
    }
    
  })
  
  
  observe({
    if (input$apygarda == "Visos") {
      leafletProxy("map") %>%
        setView(
          lng = mean(coord()[, 1]),
          lat = mean(coord()[, 2]),
          zoom = 7.5
        )
      
    } else {
      leafletProxy("map") %>%
        setView(
          lng = mean(coord()[, 1]),
          lat = mean(coord()[, 2]),
          zoom = ifelse((
            zemelapis_apylinkiu_df %>%
              filter(APG_PAV == input$apygarda) %>%
              as_tibble %>%
              select(Sav_pav,-geometry) %>%
              as.matrix %>%
              str_detect("miesto") %>%
              any
          ),
          13,
          10
          )
        )
      
    }
    
  })
  
  
  observeEvent(input$map_shape_click, {
    # update the location selectInput on map clicks
    click <- input$map_shape_click
    
    print(click$id)
    
    if (is.null(click$id)) {
      return()
    } else {
      apygarda <- zemelapis_apylinkiu_df %>%
        filter(APG_PAV == click$id |
                 APL_ID == click$id) %>% slice(1) %>% pull(APG_PAV)
      
      coord <- zemelapis_apylinkiu_df %>%
        filter(APG_PAV == apygarda) %>%
        select(geometry) %>% st_coordinates
      
      defaultW <- getOption("warn")
      
      options(warn = -1)
      
      leafletProxy("map") %>%
        setView(
          lng = mean(coord[, 1]),
          lat = mean(coord[, 2]),
          zoom = ifelse((
            zemelapis_apylinkiu_df %>%
              filter(APG_PAV == click$id |
                       APL_ID == click$id) %>%
              as_tibble %>%
              select(Sav_pav,-geometry) %>%
              as.matrix %>%
              str_detect("miesto") %>% any
          ),
          13,
          10
          )
        )
      options(warn = defaultW)
      
      updateSelectInput(
        session,
        "apygarda",
        label = "Apygarda",
        choices = c("Visos", vienmandat$Apygarda %>% unique),
        selected = apygarda
      )
      
    }
    
  })
  
  #### Lentelë ####
  
  output$table <- DT::renderDataTable(DT::datatable({
    dat <- bendras %>% select(-3)
    names(dat)[7] <- "mandatø skaièius"
    names(dat) <- names(dat) %>% toTitleCase
    
    if (input$apygard != "Visos:") {
      dat <- daugiamandat %>% select(-party_color) %>%
        group_by(Apygardos_nr) %>%
        arrange(ið_viso_apygardoje %>% rev) %>%
        ungroup %>% filter(Apygarda == input$apygard)
      
      
      if (!is.na(dat$diplomatinëse_atstovybëse) %>% any) {
        dat <- dat  %>%
          select(Pavadinimas, 3:9) %>% unique
        
        names(dat) <-
          str_remove_all(names(dat), "_apygardoje") %>% str_replace_all("_", " ") %>% toTitleCase
        
      } else {
        dat <- dat  %>%
          select(Pavadinimas, 3:7) %>% unique
        
      }
    }
    if (input$apyl != "Visos:") {
      dat <- daugiamandat %>% select(-party_color) %>%
        group_by(Apygardos_nr) %>%
        arrange(ið_viso_apylinkeje %>% rev) %>%
        ungroup %>% filter(apylinke == input$apyl) %>%
        select(Pavadinimas, 10:14)
      
      names(dat) <-
        str_remove_all(names(dat), "_apygardoje") %>% str_replace_all("_", " ") %>% toTitleCase
      
    }
    if (input$partija != "Visos:") {
      dat <- dat %>%  filter(Pavadinimas == input$partija)
    }
    
    dat
    
  }))
  
  
}

shinyApp(ui, server)

