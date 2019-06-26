#Load packages
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(png)
library(spatstat)
library(raster)

#Load image. Change directory to the lcoation of your image.
ima<-readPNG("Image.PNG")

####UI
ui<-fluidPage(
  # Application title
  titlePanel("Image map"),
  
  #Adding a switch for switching between likes and dislikes
  switchInput("col",value=TRUE,onLabel = "Like",
              offLabel = "Dislike", onStatus = "success", offStatus = "danger"),
  
  #Adding buttons to reset or save the clicked points in to a data.frame
  actionButton("undo", "Reset"),
  useShinyjs(),
  actionButton("updateplot", "Save"),
  
  #Rendering the plots in shiny
  plotOutput("plot1", click = "plot_click",height=dim(ima)[1],width=dim(ima)[2]),
  plotOutput("plot2",height=dim(ima)[1],width=dim(ima)[2])
)

####Server
server=1)
shinyjs::show("updateplot")
}) 

#Creating the logic to store points you have clicked on
val<-reactiveValues(clickx = NULL, clicky = NULL,
                    clickx2 = NULL, clicky2 = NULL)

#Observes the clicks dependent on whether you have selected
#Like or Dislike
observe({
  input$plot_click
  
  if(input$col==TRUE){
    isolate({
      val$clickx = c(val$clickx, input$plot_click$x)
      val$clicky = c(val$clicky, input$plot_click$y)
    })
  }
  else(isolate({
    val$clickx2 = c(val$clickx2, input$plot_click$x)
    val$clicky2 = c(val$clicky2, input$plot_click$y)
  })
  )
  
})

#Setting up the action for the reset up
observeEvent(input$undo, {
  isolate({
    val$clickx = NULL
    val$clicky = NULL
    val$clickx2 = NULL
    val$clicky2 = NULL
  })
})

#First plot. This will be the plot you will click
output$plot1<-renderPlot({
  plot(1,type="n",yaxt="n",xaxt="n",xlab="",ylab="")
  lim1)){
    newDF<-rbind(data.frame(x=val$clickx,y=val$clicky,cat="Like",sys.time=paste(Sys.time())),
                 data.frame(x=val$clickx2,y=val$clicky2,cat="Dislike",sys.time=paste(Sys.time())))
  }
else(newDF<-rbind(data.frame(x=val$clickx,y=val$clicky,cat="Like",sys.time=paste(Sys.time()))))

#Optional. This is to create unique idenitfiers, so you can isolate every unique session
sam<-list()
sam[[1]]<-1:30
sam[[2]]<-letters
sam[[3]]<-LETTERS
sam[[4]]<-c("!", "$", "%", "&", "(", ")", "*")

tmp<-mapply(sample,sam,c(10,10,10,10),TRUE)
newDF$session_id<-paste(sample(tmp,10),collapse="")

#Reading in any existing data and adding it to the current clicks
existing<-read.csv("Heat_map_data.csv")
comb<-rbind(existing,newDF)
write.csv(comb,"Heat_map_data.csv",row.names=F)
})

observeEvent(input$updateplot, {
  output$plot2<-renderPlot({
    plot(1,type="n",yaxt="n",xaxt="n",xlab="",ylab="")
    lim<-par()
    rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
    
    dd<-read.csv("Heat_map_data.csv")
    
    #Using spatstat to map the spatial distribution of points
    coord.like<-with(subset(dd,dd$cat=="Like"),ppp(x,y,c(0,3),c(0,3)))
    coord.dislike<-with(subset(dd,dd$cat=="Dislike"),ppp(x,y,c(0,3),c(0,3)))
    
    zl<-density(coord.like, 0.05)
    zd<-density(coord.dislike, 0.05)
    
    #Setting up the overlay colours
    l_palette<-colorRampPalette(c("transparent","green"))
    d_palette<-colorRampPalette(c("transparent","red"))
    l_opaque<-l_palette(5)
    d_opaque<-d_palette(5)
    
    l_trans<-paste(l_opaque,c("10",rep("80",4)),sep = "")
    l_trans_trans<-rep("transparent",5)
    d_trans<-paste(d_opaque,c("10",rep("80",4)),sep = "")
    d_trans_trans<-rep("transparent",5)
    
    #Plotting the Like and Dislike overlays
    plot(zl, add=T, col = if(nrow(subset(dd,dd$cat=="Like"))==0){l_trans_trans}else{l_trans})
    plot(zd, add=T, col = if(nrow(subset(dd,dd$cat=="Dislike"))==0){d_trans_trans}else{d_trans})
    
    #Adding the highest density points
    zr = raster(zl)
    points(xyFromCell(zr, which.max(zr)),pch=16,cex=2,
           col=ifelse(nrow(subset(dd,dd$cat=="Like"))==0,
                      "transparent","darkgreen"))
    
    zr2 = raster(zd)
    points(xyFromCell(zr2, which.max(zr2)),pch=16,cex=2,
           col=ifelse(nrow(subset(dd,dd$cat=="Dislike"))==0,
                      "transparent","darkred"))
    
  })
})
}

#Run the application
shinyApp(ui = ui, server = server)