

rm(list=ls())


library(shiny)
library(ggplot2)
library(data.table)
library(rvest)
library(shinythemes) # adding the shinythemese package
library(webshot)

fundamentals_dir <- "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/"

country_cur_lut <- data.table(reg=c("united-states","euro-area","united-kingdom","japan","australia","switzerland","canada","new-zealand"),
                              curr=c("usd","eur","gbp","jpy","aud","chf","cad","nzd"))
indicators = c("consumer-price-index-cpi","interest-rate","gdp-growth-annual","unemployment-rate","balance-of-trade","government-debt-to-gdp","inflation-cpi","non-farm-payrolls")

#-- Save all the fundamentals charts
save_fund_chart<-function()
{

  for(country in country_cur_lut$reg)
    {
  cat(paste0("Fetching fundamentals of ",country,"\n\n") )
for ( indicator in indicators)  
  {
  cat(paste0(indicator,"\n"))
    link = paste0("https://tradingeconomics.com/",country,"/",indicator)
  webshot(link, paste0(fundamentals_dir,"/",country_cur_lut[reg==country ,curr],"/",indicator,".png"), selector = "figure[class=chart-figure]",
          expand = c(0, 50, 0, -30))
cat("finished\n")
}
    cat("####################\n\n")
    }

}  


#-- Returns datatable with main fundamentals
get_fund<-function(country)
{
  
cnts <- c("united-states",
   "euro-area",
  "japan",
  "united-kingdom",
  "canada",
  "australia",
  "switzerland",
  "new-zealand")
if(country %in% cnts)
{
    link = paste0("https://tradingeconomics.com/",country,"/indicators")

  dt = data.table(
    indicator = c("interestRate","gdpGrowth","unemployment","nonfarm","cpi"),
    last = c(0,0,0,0,0),
    previous = c(0,0,0,0,0),
    date = c("","","","","")
  )
  
  
pages_data_label <- html %>% 
  # The '.' indicates the class
  html_nodes('td') %>% 
  # Extract the raw text as a list
  html_text() %>%
  trimws()
cleaned = unlist(lapply(pages_data_label,gsub,pattern="\\s+",replacement=""))



x=as.data.table(t(matrix(cleaned,nrow=7)))
names(x)<-c("indicator","last","reference","previous","range","frequency","delete")
x$delete <- NULL

x<-unique(x)

dt[indicator=="unemployment",last:=x[grepl("^unemploymentrate",indicator,ignore.case = T),last][1]]
dt[indicator=="unemployment",date:=x[grepl("^unemploymentrate",indicator,ignore.case = T),reference][1]]
dt[indicator=="unemployment",previous:=x[grepl("^unemploymentrate",indicator,ignore.case = T),previous][1]]


dt[indicator=="gdpGrowth",last:=x[grepl("gdpannual",indicator,ignore.case = T),last][1]]
dt[indicator=="gdpGrowth",date:=x[grepl("gdpannual",indicator,ignore.case = T),reference][1]]
dt[indicator=="gdpGrowth",previous:=x[grepl("gdpannual",indicator,ignore.case = T),previous][1]]


dt[indicator=="interestRate",last:=x[grepl("^interest",indicator,ignore.case = T),last][1]]
dt[indicator=="interestRate",date:=x[grepl("^interest",indicator,ignore.case = T),reference][1]]
dt[indicator=="interestRate",previous:=x[grepl("^interest",indicator,ignore.case = T),previous][1]]


dt[indicator=="nonfarm",last:=x[grepl("^nonfarm",indicator,ignore.case = T),last][1]]
dt[indicator=="nonfarm",date:=x[grepl("^nonfarm",indicator,ignore.case = T),reference][1]]
dt[indicator=="nonfarm",previous:=x[grepl("^nonfarm",indicator,ignore.case = T),previous][1]]


dt[indicator=="cpi",last:=x[grepl("^ConsumerPriceIndex",indicator,ignore.case = T),last][1]]
dt[indicator=="cpi",date:=x[grepl("^ConsumerPriceIndex",indicator,ignore.case = T),reference][1]]
dt[indicator=="cpi",previous:=x[grepl("^ConsumerPriceIndex",indicator,ignore.case = T),previous][1]]


dt[,change:=100*(as.numeric(last)-as.numeric(previous))/as.numeric(last)]

#x[grepl("cpi",indicator,ignore.case = T)]

return(dt)
}

}  
  #  gsub("\\s+","",trimws(pages_data_label[9]))


#-- Downloads the currency strength chart
download_str<-function()
{
  require(webshot)
  image_dir <- "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/strengths/"
  
  i=1
  N_pers=2
  while(i<(N_pers+1))
  {
    link = paste0("https://finviz.com/forex_performance.ashx?v=",i)
    webshot(link, paste0(image_dir,"strength_",i,".png"), selector = ".vertical-bar-chart",
            expand = c(0, 80, -10, -40))
    
    i=i+1
  }
  
  
  #-- Last Detailed strength
  i=1
  link = paste0("https://finviz.com/forex_performance.ashx?v=",i)
  webshot(link, paste0(image_dir,"detailstrength.png"), selector = "div[id=forex_performance]")
  
  cat("Done! \n")
}

#-- Defines the black theme of ggplot
theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      #panel.margin = unit(0.5, "lines"),  
      panel.spacing = unit(0.5, "lines"),  
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}



server <- function(input, output, session) {
  
  df = data.table(x=c("BUY","SELL"),y=c(20,80))
  
  #-- Buttons
#--- Strengths
  
    #-- On click of the update strengths button
  observeEvent(input$update_strengths, {
    cat("Updating strengths\n\n")
    ###########################
    output$daystrength <- renderImage({
      return(list(
        src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/strengths/strength_1.png",
        contentType = "image/jpeg",
        width = 500,
        height = 200,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$weekstrength <- renderImage({
      return(list(
        src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/strengths/strength_2.png",
        contentType = "image/jpeg",
        width = 500,
        height = 200,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$detailstrength <- renderImage({
      return(list(
        src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/strengths/detailstrength.png",
        contentType = "image/jpeg",
        width = 500,
        height = 200,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    
    
    
    #########################
        
  })
  
  #-- On click of the download strengths button
  observeEvent(input$download_strengths, {
    cat("Downloading strengths\n\n")
    
  download_str()
})

#--- Fundamentals
  #-- On click of the update strengths button
  observeEvent(input$update_fund, {
    cat("Updating fundamentals\n\n")
    ###########################

    #### USD ######
    output$usdinterest_plot <- renderImage({
      return(list(
        src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/usd/interest-rate.png",
        contentType = "image/jpeg",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$usdgdp_plot <- renderImage({
      return(list(
        src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/usd/gdp-growth-annual.png",
        contentType = "image/jpeg",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$usdinflation_plot <- renderImage({
      return(list(
        src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/usd/inflation-cpi.png",
        contentType = "image/png",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    ###### EUR ####
    output$eurinterest_plot <- renderImage({
      return(list(
        src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/eur/interest-rate.png",
        contentType = "image/jpeg",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$eurgdp_plot <- renderImage({
      return(list(
        src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/eur/gdp-growth-annual.png",
        contentType = "image/jpeg",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$eurinflation_plot <- renderImage({
      return(list(
        src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/eur/inflation-cpi.png",
        contentType = "image/png",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    
    ###### GBP #####
    output$gbpinterest_plot <- renderImage({
      return(list(
        src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","gbp","/interest-rate.png"),
        contentType = "image/jpeg",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$gbpgdp_plot <- renderImage({
      return(list(
        src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","gbp","/gdp-growth-annual.png"),
        contentType = "image/jpeg",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$gbpinflation_plot <- renderImage({
      return(list(
        src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","gbp","/inflation-cpi.png"),
        contentType = "image/png",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    ###### JPY #####
    cur="jpy"
    output$jpyinterest_plot <- renderImage({
      return(list(
        src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","jpy","/interest-rate.png"),
        contentType = "image/jpeg",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$jpygdp_plot <- renderImage({
      return(list(
        src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","jpy","/gdp-growth-annual.png"),
        contentType = "image/jpeg",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$jpyinflation_plot <- renderImage({
      return(list(
        src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","jpy","/inflation-cpi.png"),
        contentType = "image/png",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    
    ###### AUD #####
    output$audinterest_plot <- renderImage({
      return(list(
        src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","aud","/interest-rate.png"),
        contentType = "image/jpeg",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$audgdp_plot <- renderImage({
      return(list(
        src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","aud","/gdp-growth-annual.png"),
        contentType = "image/jpeg",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$audinflation_plot <- renderImage({
      return(list(
        src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","aud","/inflation-cpi.png"),
        contentType = "image/png",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    
    ###### NZD #####
    
    output$nzdinterest_plot <- renderImage({
      return(list(
        src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","nzd","/interest-rate.png"),
        contentType = "image/jpeg",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$nzdgdp_plot <- renderImage({
      return(list(
        src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","nzd","/gdp-growth-annual.png"),
        contentType = "image/jpeg",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$nzdinflation_plot <- renderImage({
      return(list(
        src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","nzd","/inflation-cpi.png"),
        contentType = "image/png",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    ###### CAD #####
    
    output$cadinterest_plot <- renderImage({
      return(list(
        src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","cad","/interest-rate.png"),
        contentType = "image/jpeg",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$cadgdp_plot <- renderImage({
      return(list(
        src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","cad","/gdp-growth-annual.png"),
        contentType = "image/jpeg",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$cadinflation_plot <- renderImage({
      return(list(
        src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","cad","/inflation-cpi.png"),
        contentType = "image/png",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    #-- CHF
    output$chfinterest_plot <- renderImage({
      return(list(
        src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/chf/interest-rate.png",
        contentType = "image/jpeg",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$chfgdp_plot <- renderImage({
      return(list(
        src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/chf/gdp-growth-annual.png",
        contentType = "image/jpeg",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    output$chfinflation_plot <- renderImage({
      return(list(
        src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/chf/inflation-cpi.png",
        contentType = "image/png",
        width = 300,
        #  height = 100,
        alt = "Face"
      ))
    }, deleteFile = FALSE)
    
    
    
    
    
    
    #########################
    cat("Done!\n\n")
    
  })
  
  #-- On click of the download strengths button
  observeEvent(input$download_fund, {
    cat("Downloading fundamentals, this will take a while\n\n")
    
    save_fund_chart()
    cat("Finished downloading the fundamental charts, now you can press update fundamentals\n\n")
  
    })
  
  
  #-- Strengths

  output$daystrength <- renderImage({
    return(list(
      src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/strengths/strength_1.png",
      contentType = "image/jpeg",
      width = 500,
      height = 200,
      alt = "Face"
    ))
  }, deleteFile = FALSE)

  output$weekstrength <- renderImage({
    return(list(
      src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/strengths/strength_2.png",
      contentType = "image/jpeg",
      width = 500,
      height = 200,
      alt = "Face"
    ))
  }, deleteFile = FALSE)

  output$detailstrength <- renderImage({
    return(list(
      src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/strengths/detailstrength.png",
      contentType = "image/jpeg",
      width = 500,
      height = 200,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  
  
#-- Model Scores  
  output$ploteu <-renderPlot({
    ggplot(data=df, aes(x=x, y=y)) +
    geom_bar(stat="identity",fill="red")+
    geom_text(aes(label=y), vjust=1.6, color="white", size=3.5)+
    theme_black()+ggtitle("EURUSD") +
      xlab("") + ylab("")
  })
  output$plotgu <-renderPlot({
    ggplot(data=df, aes(x=x, y=y)) +
      geom_bar(stat="identity",fill="red")+
      geom_text(aes(label=y), vjust=1.6, color="white", size=3.5)+
      theme_black()+ ggtitle("GBPUSD") +
      xlab("") + ylab("")
  })
  output$plotuj <-renderPlot({
    ggplot(data=df, aes(x=x, y=y)) +
      geom_bar(stat="identity",fill="red")+
      geom_text(aes(label=y), vjust=1.6, color="white", size=3.5)+
      theme_black()+ ggtitle("USDJPY") +
      xlab("") + ylab("")
  })
  output$plotau <-renderPlot({
    ggplot(data=df, aes(x=x, y=y)) +
      geom_bar(stat="identity",fill="red")+
      geom_text(aes(label=y), vjust=1.6, color="white", size=3.5)+
      theme_black()+ ggtitle("AUDUSD") +
      xlab("") + ylab("")
  })
  output$plotnu <-renderPlot({
    ggplot(data=df, aes(x=x, y=y)) +
      geom_bar(stat="identity",fill="red")+
      geom_text(aes(label=y), vjust=1.6, color="white", size=3.5)+
      theme_black()+ ggtitle("NZDUSD") +
      xlab("") + ylab("")
  })
  output$plotuh <-renderPlot({
    ggplot(data=df, aes(x=x, y=y)) +
      geom_bar(stat="identity",fill="red")+
      geom_text(aes(label=y), vjust=1.6, color="white", size=3.5)+
      theme_black()+ ggtitle("USDCHF") +
      xlab("") + ylab("")
  })
  output$plotuc <-renderPlot({
    ggplot(data=df, aes(x=x, y=y)) +
      geom_bar(stat="identity",fill="red")+
      geom_text(aes(label=y), vjust=1.6, color="white", size=3.5)+
      theme_black()+ ggtitle("USDCAD") +
      xlab("") + ylab("")
  })
  
  #-- Fundamentals
  #### USD ######
  output$usdinterest_plot <- renderImage({
    return(list(
      src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/usd/interest-rate.png",
      contentType = "image/jpeg",
      width = 300,
    #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$usdgdp_plot <- renderImage({
    return(list(
      src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/usd/gdp-growth-annual.png",
      contentType = "image/jpeg",
      width = 300,
    #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)

  output$usdinflation_plot <- renderImage({
    return(list(
      src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/usd/inflation-cpi.png",
      contentType = "image/png",
     width = 300,
    #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)

  ###### EUR ####
  output$eurinterest_plot <- renderImage({
    return(list(
      src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/eur/interest-rate.png",
      contentType = "image/jpeg",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$eurgdp_plot <- renderImage({
    return(list(
      src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/eur/gdp-growth-annual.png",
      contentType = "image/jpeg",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$eurinflation_plot <- renderImage({
    return(list(
      src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/eur/inflation-cpi.png",
      contentType = "image/png",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  
  ###### GBP #####
  output$gbpinterest_plot <- renderImage({
    return(list(
      src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","gbp","/interest-rate.png"),
      contentType = "image/jpeg",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$gbpgdp_plot <- renderImage({
    return(list(
      src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","gbp","/gdp-growth-annual.png"),
      contentType = "image/jpeg",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$gbpinflation_plot <- renderImage({
    return(list(
      src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","gbp","/inflation-cpi.png"),
      contentType = "image/png",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  ###### JPY #####
  cur="jpy"
  output$jpyinterest_plot <- renderImage({
    return(list(
      src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","jpy","/interest-rate.png"),
      contentType = "image/jpeg",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$jpygdp_plot <- renderImage({
    return(list(
      src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","jpy","/gdp-growth-annual.png"),
      contentType = "image/jpeg",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$jpyinflation_plot <- renderImage({
    return(list(
      src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","jpy","/inflation-cpi.png"),
      contentType = "image/png",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  
  ###### AUD #####
  output$audinterest_plot <- renderImage({
    return(list(
      src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","aud","/interest-rate.png"),
      contentType = "image/jpeg",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$audgdp_plot <- renderImage({
    return(list(
      src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","aud","/gdp-growth-annual.png"),
      contentType = "image/jpeg",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$audinflation_plot <- renderImage({
    return(list(
      src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","aud","/inflation-cpi.png"),
      contentType = "image/png",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  
  ###### NZD #####
  
  output$nzdinterest_plot <- renderImage({
    return(list(
      src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","nzd","/interest-rate.png"),
      contentType = "image/jpeg",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$nzdgdp_plot <- renderImage({
    return(list(
      src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","nzd","/gdp-growth-annual.png"),
      contentType = "image/jpeg",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$nzdinflation_plot <- renderImage({
    return(list(
      src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","nzd","/inflation-cpi.png"),
      contentType = "image/png",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  ###### CAD #####
 
  output$cadinterest_plot <- renderImage({
    return(list(
      src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","cad","/interest-rate.png"),
      contentType = "image/jpeg",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$cadgdp_plot <- renderImage({
    return(list(
      src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","cad","/gdp-growth-annual.png"),
      contentType = "image/jpeg",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$cadinflation_plot <- renderImage({
    return(list(
      src = paste0("C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/","cad","/inflation-cpi.png"),
      contentType = "image/png",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  #-- CHF
  output$chfinterest_plot <- renderImage({
    return(list(
      src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/chf/interest-rate.png",
      contentType = "image/jpeg",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$chfgdp_plot <- renderImage({
    return(list(
      src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/chf/gdp-growth-annual.png",
      contentType = "image/jpeg",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  output$chfinflation_plot <- renderImage({
    return(list(
      src = "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/fund/chf/inflation-cpi.png",
      contentType = "image/png",
      width = 300,
      #  height = 100,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
  

  
  }


ui <- fluidPage(#themeSelector(), # displaying the different themes, replace this line when publishing with 
                theme = shinytheme("cyborg"),
                
                titlePanel(strong("Dashboard")), # using strong as a direct tag
                
                sidebarLayout(
                  
                  sidebarPanel(
                   
                    #actionButton("get_news", "Get News"),
                    #actionButton("update_fundamentals", "Update Fundamentals"),
                    #selectInput(inputId = "pair_inp",label="Pairs",choices=c("EURUSD","USDJPY","AUDUSD","NZDUSD","USDCAD","GBPUSD","USDCHF")) ,
                    #submitButton(text = "Update") ,
                    withTags(
                      div(
                        b("News")
                      )), width=2  ),
                  
                  mainPanel(
                    
                    tabsetPanel(

                     #### Strengths tab #####
                      tabPanel("Scores"
                               ,flowLayout( 
                                 plotOutput(outputId = "ploteu")
                               , plotOutput(outputId = "plotgu")
                               , plotOutput(outputId = "plotuj")
                               , plotOutput(outputId = "plotau")
                               , plotOutput(outputId = "plotnu")
                               , plotOutput(outputId = "plotuc")
                               , plotOutput(outputId = "plotuh")  
                               
                               )

                               ), 
                      #### Strengths tab #####
                      tabPanel("Strengths",
                               actionButton("download_strengths", "Download strengths"),
                               actionButton("update_strengths", "Update strengths"),
                               withTags(div(b("Daily strength"))),
                               fluidRow(
                                 column(1,align="center",imageOutput("daystrength"))
                               ),
                               withTags(div(b("Weekly strength"))),
                               fluidRow(
                                 column(1,align="center",
                                        div(style = "font-size: 15px; padding: 0px 0px; margin-top:-15em", imageOutput("weekstrength")))
                               ),
                               withTags(div(b("Detailed strength"))),
                               fluidRow(
                                 column(1,align="center",
                                        div(style = "font-size: 15px; padding: 0px 0px; margin-top:-15em", imageOutput("detailstrength")))
                               )
                               
                               ), 
                      #### Support resistance tab  #####
                      
                      tabPanel("Support resistance"),
                      
                      #### Fundamentals tab #####
                      
                      tabPanel("Fundamentals" ,
                               actionButton("download_fund", "Download fundamentals"),
                               actionButton("update_fund", "Update fundamentals"),
                               fluidRow(
                                 column(width = 1,alignment="center",
                                        "Currency"
                                 ),
                                 column(width = 3, offset = 0,
                                        "Interest rate"
                                 ),
                                 column(width = 3, offset = 0,
                                        "Inflation"
                                 ),
                                 column(width = 3, offset = 0, 
                                        "GDP growth"
                                 )
                                 
                               ) ,
                               fluidRow(
                                 column(width = 1,
  "USD"
                                 ),
                                 column(width = 3, offset = 0,
                                        imageOutput("usdinterest_plot")
                                 ),
                                 column(width = 3, offset = 0,
                                        imageOutput("usdinflation_plot")
                                 ),
                                 column(width = 3, offset = 0,
                                        imageOutput("usdgdp_plot")
                                 )
                                 )
                               
                              #-- EUR 
                               ,
                               fluidRow(
                                 column(width = 1,
                                        div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em","EUR")
                                 ),
                                 column(width = 3, offset = 0,
                                        div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em", imageOutput("eurinterest_plot") )
                                 ),
                                 column(width = 3, offset = 0,
                                        div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em", imageOutput("eurinflation_plot"))
                                 ),
                                 column(width = 3, offset = 0, 
                                        div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",  imageOutput("eurgdp_plot") )
                                 )
                               )
##############################################                               
#-- JPY 
,
fluidRow(
  column(width = 1,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",   "JPY")
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",  imageOutput("jpyinterest_plot"))
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",  imageOutput("jpyinflation_plot"))
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",  imageOutput("jpygdp_plot"))
  )
)
##############################################                               
#-- GBP 
,
fluidRow(
  column(width = 1,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",    "GBP")
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",  imageOutput("gbpinterest_plot"))
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",   imageOutput("gbpinflation_plot"))
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",  imageOutput("gbpgdp_plot"))
  )
)
##############################################                               
#-- AUD 
,
fluidRow(
  column(width = 1,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",   "AUD")
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",   imageOutput("audinterest_plot"))
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",  imageOutput("audinflation_plot"))
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",  imageOutput("audgdp_plot"))
  )
)
##############################################                               
#-- CAD 
,
fluidRow(
  column(width = 1,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",  "CAD")
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",   imageOutput("cadinterest_plot"))
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",  imageOutput("cadinflation_plot"))
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",  imageOutput("cadgdp_plot"))
  )
)
##############################################                               
#-- NZD 
,
fluidRow(
  column(width = 1,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",   "NZD")
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",   imageOutput("nzdinterest_plot"))
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",   imageOutput("nzdinflation_plot"))
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",   imageOutput("nzdgdp_plot"))
  )
)
##############################################                               
#-- CHF 
,
fluidRow(
  column(width = 1,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",  "CHF")
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",  imageOutput("chfinterest_plot"))
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em", imageOutput("chfinflation_plot"))
  ),
  column(width = 3, offset = 0,
         div(style = "font-size: 15px; padding: 0px 0px; margin-top:-17em",  imageOutput("chfgdp_plot"))
  )
)
##############################################                               

                      
                      )
                    )
                  )
                ))

shinyApp(ui = ui, server = server)






if(F)
{
###########################

# webshot


webshot("https://www.r-project.org/", "r.png")

webshot("https://www.r-project.org/", "r-viewport.png", cliprect = "viewport")

webshot("https://www.r-project.org/", "r-sidebar.png", selector = ".sidebar")


image_dir <- "C:/Users/Mohamed Ibrahim/Box Sync/FX_DATASCIENCE/main_fx/03_figs/strengths/"

i=1
N_pers=2
while(i<(N_pers+1))
{
  link = paste0("https://finviz.com/forex_performance.ashx?v=",i)
  webshot(link, paste0(image_dir,"strength_",i,".png"), selector = ".vertical-bar-chart",
          expand = c(0, 80, -10, -40))
  
i=i+1
}


#-- Last hour
i=1
link = paste0("https://finviz.com/forex_performance.ashx?v=",i)
webshot(link, paste0(image_dir,"detailstrength.png"), selector = "div[id=forex_performance]")

#webshot(link, paste0(image_dir,"strength_table_",i,".png"), selector = "div.forex_performance")



webshot(link, "r.png")
webshot(link, "r-viewport.png", cliprect = "viewport")
webshot(link, "r-sidebar.png", selector = ".vertical-bar-chart",
        expand = c(0, 80, -10, -40))





















######################################
library(shiny)

# Define UI with external image call
ui <- fluidPage(
  titlePanel("Look at the image below"),
  
  sidebarLayout(sidebarPanel(),
                
                mainPanel(htmlOutput("picture"))))

# Define server with information needed to hotlink image
server <- function(input, output) {
  output$picture <-
    renderText({
      c(
        '<img src="',
        "http://drive.google.com/uc?export=view&id=0By6SOdXnt-LFaDhpMlg3b3FiTEU",
        '">'
      )
    })
}

shinyApp(ui = ui, server = server)



















}