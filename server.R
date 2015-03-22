## Run with runApp()

# Should delete this before final commit
proj_path <- paste("~/Desktop/MOOCs/Coursera/Data Science",
                   "9. Developing Data Products/Project/shiny-project", sep='/')
setwd(proj_path)

####

source('./setup.R')


shinyServer(
  function(input, output) {

    output$my.pred <- renderText({
        "temp"
    })

    output$temp1 <- renderText({
        input$addons
    })
    output$temp2 <- renderText({
        input$doors
    })

    output$my.pred <- renderText({
        my.addons <- fix.addons(input$addons)
        newtemp <- data.frame(Mileage=input$mileage,
                              Cylinder=as.numeric(input$cylinders),
                              Doors=as.numeric(input$doors),
                              Cruise=factor(my.addons$cruise, levels=c("No", "Yes")),
                              Sound=factor(my.addons$sound, levels=c("Standard", "Premium")),
                              Leather=factor(my.addons$leather, levels=c("Regular", "Leather")),
                              Brand=factor(input$brand, levels=c("Buick", "Cadillac", "Chevy", "Pontiac", "Saab", "Saturn")),
                              Type=factor(input$cartype, levels=c("convertible", "coupe", "hatchback", "sedan", "wagon")))
        # sprintf("$ %3.2f", predict(my.forest, newdata=newtemp))
        dollar(predict(my.forest, newdata=newtemp))
    })

    output$my.graph <- renderPlot({
        ggplot(data=df, aes(x=Mileage, y=Price, colour=Brand)) +
            geom_point(alpha=0.6) + facet_grid(Doors ~ Cylinder, labeller=plot.labeller) +
            scale_x_continuous(labels = comma) +
            scale_y_continuous(labels = dollar)
    })
  }
)
