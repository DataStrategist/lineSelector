
library(dplyr)
library(tidyr)
library(akima)
library(ggplot2)
library(plotly)

shinyServer(function(input, output) {
  ## Read in
  a <- reactive({
    read.csv(input$file$datapath)
    # a <- read.csv("data.csv")
  })
  
  ## do the analysis
  FinalDF <- reactive({
    a <- a()
    a %>%
      select(Entr,Row,Col,KgHa, Cruza) -> b
    
    
    ## Identify controls
    b$Control[b$Entr%%input$num == 0] <- 1
    # b$Control[b$Entr%%10==0] <- 1 ## Control every 10
    b$Control[is.na(b$Control)] <- 0
    
    ## Isolate controls
    b$Yield2[b$Control==1] <- b$KgHa[b$Control==1]
    if(sum(is.na(b$Yield2))!=0){b$Yield2[is.na(b$Yield2)] <- 0}
    #       
    #       ## Plot of best yields
    #       b  %>% filter(KgHa>4000) %>%
    #         ggplot(aes(x=Col,y=Row, z=KgHa, color=KgHa,label=Entr)) + 
    #         # stat_contour(geom="polygon", aes(fill=..level..)) +
    #         geom_point()+ geom_text(nudge_y = 0.5, check_overlap = TRUE) +
    #         scale_colour_gradient(low = "red", high = "green",limits=c(4000, 5000))
    #       # + geom_contour(aes(colour=..level..)) +
    #       
    #       
    ## -------------------- GENERATE MATRIX FOR SURFACE CALCULATION----------------
    ## ALL DATA
    MaxCol <- max(a$Col)
    MaxRow <- max(a$Row)
    s <- matrix(a$KgHa,MaxRow,MaxCol,byrow = T)
    
    ## Matrix of just controls
    Cont <- matrix(b$Yield2,MaxRow,MaxCol,byrow = T)
    
    ## Interpolate empty data ----------------
    ## First get real control values
    idx <- which(Cont > 0, arr.ind=TRUE)
    s.nz <- Cont[idx]
    ControlValues <- data.frame(idx,s.nz)
    
    NewCont <- interp(x=ControlValues$row,y = ControlValues$col,z = ControlValues$s.nz,
                      xo = 1:nrow(Cont),yo = 1:ncol(Cont),extrap = T)[[3]]
    
    # ##Ensure the surface matches controls
    # plot_ly(z=NewCont, type="surface",showscale=FALSE) %>%
    #   add_trace(z=Cont, type="surface", showscale=FALSE, colors = terrain.colors(3))
    
    ## Plot Yields over control surfaces
    plot_ly(z=s, type="surface",showscale=FALSE) %>%
      add_trace(z=NewCont, type="surface", showscale=FALSE, colors = terrain.colors(3) )
    
    ## Generate difference of surfaces
    Difference <- as.numeric(as.character(s-NewCont))
    
    # Spit out a df of best yields (using userinput)
    data.frame(EntryNumber = a$Entr,
               Control = b$Control,
               Row = a$Row,
               Col= a$Col,
               Cruza = a$Cruza, 
               Historial = a$Historia,
               RealYield=a$KgHa,
               NormalizedYield = Difference) -> FinalDF
    FinalDF %>% as.data.frame() -> FinalDF
    FinalDF %>% 
      arrange(desc(NormalizedYield)) %>% 
      filter(NormalizedYield>input$slider1) 
  })
  
  
  
  
  
    
  ## ------------------------------- OUTPUTSSSSSSSSSSSSSSSSS
  output$Plot3 <- renderPlotly({
    a <- a()
    a %>%
      select(Entr,Row,Col,KgHa, Cruza) -> b
    
    
    ## Identify controls
    b$Control[b$Entr%%input$num == 0] <- 1
    # b$Control[b$Entr%%10==0] <- 1 ## Control every 10
    b$Control[is.na(b$Control)] <- 0
    
    ## Isolate controls
    b$Yield2[b$Control==1] <- b$KgHa[b$Control==1]
    if(sum(is.na(b$Yield2))!=0){b$Yield2[is.na(b$Yield2)] <- 0}
    ## ALL DATA
    MaxCol <- max(a$Col)
    MaxRow <- max(a$Row)
    s <- matrix(a$KgHa,MaxRow,MaxCol,byrow = T)
    
    ## Matrix of just controls
    Cont <- matrix(b$Yield2,MaxRow,MaxCol,byrow = T)
    
    ## Interpolate empty data ----------------
    ## First get real control values
    idx <- which(Cont > 0, arr.ind=TRUE)
    s.nz <- Cont[idx]
    ControlValues <- data.frame(idx,s.nz)
    
    NewCont <- interp(x=ControlValues$row,y = ControlValues$col,z = ControlValues$s.nz,
                      xo = 1:nrow(Cont),yo = 1:ncol(Cont),extrap = T)[[3]]
    
    # ##Ensure the surface matches controls
    # plot_ly(z=NewCont, type="surface",showscale=FALSE) %>%
    #   add_trace(z=Cont, type="surface", showscale=FALSE, colors = terrain.colors(3))
    
    ## Plot Yields over control surfaces
    gg <- plot_ly(z=s, type="surface",showscale=FALSE) %>%
      add_trace(z=NewCont, type="surface", showscale=FALSE, colors = terrain.colors(3))
    gg
  })
  
  output$PERC <- renderText({
    a <- a()
    FinalDF <- FinalDF()
    paste("Selection represents ",round(nrow(FinalDF)/nrow(a) * 100,1),"% of the dataset (having removed controls)",sep="")
    # names(a)  
  })
    
  output$fig1 <- renderPlot({
    a <- a()
    FinalDF <- FinalDF()
    # Generate plot of differences above/below normal
    FinalDF %>% 
      ggplot(aes(x=Col,y=Row, z=NormalizedYield, color=NormalizedYield,label=EntryNumber)) + 
      # stat_contour(geom="polygon", aes(fill=..level..)) +
      # + geom_contour(aes(colour=..level..)) +
      geom_point()+ geom_text(nudge_y = 0.5, check_overlap = TRUE) +
      scale_colour_gradient(low = "orange", high = "blue",limits=c(input$slider1, 1500))
  })
  
  output$figControl <- renderPlot({
    FinalDF <- FinalDF()
    # Generate plot of differences above/below normal
    FinalDF %>% 
      filter(Control==1) %>%
      ggplot(aes(x=Col,y=Row, z=NormalizedYield,label=EntryNumber)) + 
      geom_point() + geom_text(nudge_y = 0.5, check_overlap = TRUE)
  })
  
  ## Generate plot of CONTROLS
  output$figControl <- renderPlot({
    a <- a()
    a %>%
      select(Entr,Row,Col,KgHa, Cruza) -> b
    
    b$Control[b$Entr%%input$num==0] <- 1 
    b$Control[is.na(b$Control)] <- 0
    b %>% 
      filter(Control==1) %>%
      ggplot(aes(x=Col,y=Row, z=KgHa,label=Entr)) + 
      geom_point() + geom_text(nudge_y = 0.5, check_overlap = TRUE)
  })
  
  output$table1 <- renderDataTable({
    FinalDF <- FinalDF()
    FinalDF %>% select(EntryNumber,Cruza,Historial,RealYield,NormalizedYield)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('LineSelectorOutput.csv', sep='') },
    content = function(file) {
      write.csv(FinalDF(), file)
      }
    )
    
      
        
        
        
        
        
        # FinalDF %>% View
        # FinalDF %>% write.csv("Output.csv")
        
        #   
        #   
        ## Other stuff
        ## Average of Parents
        # FinalDF %>% group_by(a.Cruza) %>%
        #   summarize(Average=mean(NormalizedYield), Count=n()) %>% 
        #   arrange(desc(Average)) %>% View
        
        ## Could be a plot showing the controls also
})