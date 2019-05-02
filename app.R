
library(shiny)
library(shinythemes)
library(leaflet)
library(raster)
library(tidyverse)
library(climatica)

# load data
clim <- list.files("climate", full.names=T) %>%
      stack()
names(clim) <- names(clim) %>%
      gsub("CHELSA_|_1979.2013_V1.2_land|_V1.2_land", "", .) %>%
      gsub("10_", "_", .)


# initalize location
s <- data.frame(lat=37.801064, lon=-122.478557)
coordinates(s) <- c("lon", "lat")
ll <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs(s) <- ll
data <- raster::extract(clim, s) %>%
      as.data.frame() %>%
      gather(var, value) %>%
      separate(var, c("var", "month"), sep="_") %>%
      mutate(month=as.integer(month)) %>%
      arrange(var, month)



# Define UI for application
ui <- navbarPage("shape of the seasons",
                 
                 theme = shinytheme("slate"),
                 
                 tabPanel("tool",
                          
                          fluidRow(
                                column(4,
                                       leafletOutput("map")
                                ),
                                column(4,
                                       plotOutput("cycle")
                                ),
                                column(4,
                                       plotOutput("hydro")
                                )
                          )
                 ),
                 
                 tabPanel("about")
)

# Define server logic
server <- function(input, output) {
      
      site <- reactiveValues(point = s, data = data)
      observeEvent(input$map_click, {
            s <- data.frame(lat=input$map_click$lat, lon=input$map_click$lng)
            coordinates(s) <- c("lon", "lat")
            crs(s) <- ll
            site$point <- s
            site$data <- raster::extract(clim, s) %>%
                  as.data.frame() %>%
                  gather(var, value) %>%
                  separate(var, c("var", "month"), sep="_") %>%
                  mutate(month=as.integer(month)) %>%
                  arrange(var, month)
      })
      
      output$map <- renderLeaflet({
            latlon <- coordinates(site$point)
            #Stamen.TonerBackground
            #Esri.WorldTerrain
            #Esri.WorldGrayCanvas
            leaflet() %>%
                  setView(lng=latlon[1], lat=latlon[2], zoom=2) %>%
                  addProviderTiles(providers$Esri.WorldImagery) %>%
                  addProviderTiles(providers$Stamen.TonerLines) %>%
                  addProviderTiles(providers$Stamen.TonerLabels) %>%
                  addMarkers(lng=latlon[1], lat=latlon[2])
      })
      
      observe({
            click <- input$map_click
            latlon <- coordinates(site$point)
            
            leafletProxy("map") %>%
                  clearMarkers() %>%
                  addMarkers(lng=latlon[1], lat=latlon[2])
      })
      
      output$cycle <- renderPlot({
            x <- input$map_click
            site$data %>%
                  filter(var %in% c("prec", "temp")) %>%
                  spread(var, value) %>%
                  rbind(filter(., month==1)) %>%
                  ggplot(aes(temp/10, prec, color=month)) +
                  geom_path(size=1) +
                  geom_point(size=2) +
                  scale_color_gradientn(colors=c("cyan", "green", "yellow", 
                                                 "red", "magenta", "blue"), 
                                        breaks=1:12) +
                  theme_minimal() +
                  theme(legend.position="top",
                        text=element_text(color="white", size=20, face="bold"),
                        panel.background=element_rect(fill="black"),
                        plot.background=element_rect(fill="black")) +
                  guides(color=guide_colorbar(barwidth=20)) +
                  labs(x="temperature (C)", y="precipitation (mm)")
      })
      
      output$hydro <- renderPlot({
            
            sd <- site$data %>%
                  spread(var, value) %>%
                  mutate_at(vars(temp:tmin), function(x)x/10)
            
            # get S0 values for all 12 months
            S0 <- sapply(1:12, monthly_S0, 
                         latitude=coordinates(site$point)[2])
            
            # monthly water balance variables
            pet <- hargreaves(S0, sd$temp, sd$tmin, sd$tmax) *
                  c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
            pet[sd$temp<0] <- 0 # per Wang et al 2012
            ppt <- sd$prec
            
            months <- 1:12
            for(i in 1:11){
                  i <- c(i, i+1)
                  peti <- pet[i]
                  ppti <- ppt[i]
                  
                  cross <- (peti[1] > ppti[1]) != (peti[2] > ppti[2])
                  if(cross){
                        #(peti[2] - peti[1])*x + peti[1] = (ppti[2] - ppti[1])*x + ppti[1]
                        #(peti[2] - peti[1])*x - (ppti[2] - ppti[1])*x = ppti[1] - peti[1]
                        #x * ((peti[2] - peti[1]) - (ppti[2] - ppti[1])) = ppti[1] - peti[1]
                        x  <- (ppti[1] - peti[1]) / ((peti[2] - peti[1]) - (ppti[2] - ppti[1]))  
                        y <- (peti[2] - peti[1])*x + peti[1]
                        x <- i[1] + x
                        months <- c(months, x)
                        pet <- c(pet, y)
                        ppt <- c(ppt, y)
                  }
            }
            
            aet <- pmin(pet, ppt)
            cmd <- pmax(0, pet - ppt)
            rr <- pmax(0, ppt - pet)
            
            d <- data.frame(month=months, 
                            ppt=ppt, pet=pet, 
                            aet=aet, cwd=cmd, rr=rr) %>%
                  gather(var, value, -month) %>%
                  mutate(var = factor(var, levels=rev(c("aet", "rr", "cwd", 
                                                        "ppt", "pet"))))
            
            colors <- c("forestgreen", "red", "yellow", "cyan", "blue")
            
            ggplot(data=d, aes(month, value, color=var, fill=var)) +
                  geom_area(data=filter(d, ! var %in% c("ppt", "pet"))) +
                  geom_line(data=filter(d, var %in% c("ppt", "pet")),
                            size=1) +
                  geom_point(data=filter(d, var %in% c("ppt", "pet"), 
                                         month %in% 1:12),
                             size=2) +
                  scale_fill_manual(values=colors) +
                  scale_color_manual(values=colors) +
                  scale_x_continuous(breaks=1:12) +
                  theme_minimal() +
                  theme(legend.position="top",
                        text=element_text(color="white", size=20, face="bold"),
                        panel.background=element_rect(fill="black"),
                        plot.background=element_rect(fill="black")) +
                  labs(x="month", y="mm", color=NULL, fill=NULL)
      })
      
      
}

# Run application 
shinyApp(ui = ui, server = server)

