
library(shiny)
library(reshape2)
library(ggplot2)
library(scales)
library(dplyr)
library(randomForest)

set.seed(90210)
data(cars, package='caret')

# First must add an index column for melting & join
cars <- cbind(cars, 1:804)
names(cars)[19] <- "index"

car.brand <- cars[, c(8:13, 19)]
car.brand <- melt(car.brand, id.vars='index')
car.brand <- car.brand[car.brand$value == 1, 1:2]


car.type <- cars[, c(14:18, 19)]
car.type <- melt(car.type, id.vars='index')
car.type <- car.type[car.type$value == 1, 1:2]

# df <- cbind(cars[, 1:7], car.brand, car.type)
df <- inner_join(x=cars[, c(1:7, 19)], y=car.brand, by='index')
df <- inner_join(x=df, y=car.type, by='index')
names(df)[9:10] <- c("Brand", "Type")
df <- arrange(df, index)
df$Brand <- relevel(df$Brand, "Chevy")

### If having problems, make sure this step removes the index column.
df <- df[, -8]

df$Type <- factor(df$Type)
df$Brand <- factor(df$Brand)

df$Cruise <- factor(df$Cruise, levels=c(0,1), labels=c("No", "Yes"))
df$Sound <- factor(df$Sound, levels=c(0,1), labels=c("Standard", "Premium"))
df$Leather <- factor(df$Leather, levels=c(0,1), labels=c("Regular", "Leather"))

n <- dim(df)[1]
idx <- sample(x=seq_len(n), size=floor(0.7*n), replace=FALSE)
train <- df[idx, -1]
test <- df[-idx, -1]
y.train <- df[idx, 1]
y.test <- df[-idx, 1]



my.forest <- randomForest(x=train, y=y.train, xtest=test, ytest=y.test,
                          importance=TRUE, keep.forest=TRUE)
# Better (according to caret)
my.forest2 <- randomForest(x=train, y=y.train, xtest=test, ytest=y.test,
                          importance=TRUE, keep.forest=TRUE, mtry=5)


# Test dataframe to predict price
newtemp <- data.frame(Mileage=25000,
                      Cylinder=6,
                      Doors=4,
                      Cruise=factor(1, , levels=c(0,1), labels=c("No", "Yes")),
                      Sound=factor(1, levels=c(0,1), labels=c("Standard", "Premium")),
                      Leather=factor(1, levels=c(0,1), labels=c("Regular", "Leather")),
                      Brand=factor(2, levels=1:6, labels=c("Buick", "Cadillac", "Chevy", "Pontiac", "Saab", "Saturn")),
                      Type=factor(1, levels=1:5, labels=c("convertible", "coupe", "hatchback", "sedan", "wagon")))

temp.addons <- list(Sound=0, Leather=0, Cruise=0)
# temp.addons <- list(Sound="Standard", Leather="Regular", Cruise="Yes")


create.pred.df <- function(mileage, cyl, doors, addons=temp.addons, brand, car.type){
    # A function that will create a data frame that can be passed on to the
    # 'newdata' argument of the predict function

    if (length(addons$cruise) == 0){
        addons$cruise <- "No"
    }
    if (length(addons$sound) == 0){
        addons$sound <- "Standard"
    }
    if (length(addons$leather) == 0){
        addons$leather <- "Regular"
    }


    pred.df <- data.frame(Mileage=mileage,
                          Cylinder=cyl,
                          Doors=doors,
                          Cruise=factor(addons$Cruise, levels=c(0,1), labels=c("No", "Yes")),
                          Sound=factor(addons$Sound, levels=c(0,1), labels=c("Standard", "Premium")),
                          Leather=factor(addons$Leather, levels=c(0,1), labels=c("Regular", "Leather")),
                          Brand=factor(brand, levels=c("Buick", "Cadillac", "Chevy", "Pontiac", "Saab", "Saturn")),
                          Type=factor(car.type, levels=c("convertible", "coupe", "hatchback", "sedan", "wagon"))
        )
}


fix.addons <- function(addons){

    fixed <- list(cruise='No',
                  sound='Standard',
                  leather='Regular')

    if ("Yes" %in% addons){
        fixed$cruise <- 'Yes'
    }
    if ("Premium" %in% addons){
        fixed$sound <- 'Premium'
    }
    if ("Leather" %in% addons){
        fixed$leather <- 'Leather'
    }
    return(fixed)
}

# my.graph <- ggplot(data=df, aes(x=Mileage, y=Price, colour=Brand)) +
#             geom_point(alpha=0.6) + facet_grid(Doors ~ Cylinder)

plot.labeller <- function(var, value){
    # A function that will ensure appropriate facet labels in plot
    value <- as.character(value)
    if (var=="Cylinder") {
        value <- paste(value, 'Cylinders')
    }
    if (var=='Doors'){
        value <- paste(value, 'Doors')
    }

    return(value)
}
