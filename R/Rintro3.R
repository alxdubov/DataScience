##############################################################
##############  R intro part 3          ######################
##############################################################

##############################
##########  dplyr
##############################

library(dplyr)

#### transform a data.frame into a dplyr compatible data table
class(iris)

iris2 <- as_data_frame(iris)   ## function imported to dplyr from the 'tibble' package

class(iris2)

iris
iris2

#### selection of columns
iris2 %>% select(Sepal.Length, Sepal.Width)

### selection of rows using a condition
iris2 %>% filter(Species=="setosa")

### add new columns
iris2 %>% mutate(Sepal.ratio = Sepal.Length/Sepal.Width,
                 Petal.ratio = Petal.Length/Petal.Width)


### summarise (summarize)
iris2 %>% summarise(Sepal_len_mean=mean(Sepal.Length,na.rm=T), 
                    Petal_len_mean=mean(Petal.Length,na.rm=T))

### summarise by groupping
iris2 %>% 
  group_by(Species) %>%
  summarise(Sepal_len_mean=mean(Sepal.Length,na.rm=T), 
                    Petal_len_mean=mean(Petal.Length,na.rm=T))

### joining two datasets
df1 <- iris2 %>% 
  mutate(Sepal.ratio = Sepal.Length/Sepal.Width,
         Petal.ratio = Petal.Length/Petal.Width,
         id = 1:nrow(iris2)) %>%
  select(id, Sepal.ratio, Petal.ratio)

iris2 <- iris2 %>%
  mutate(id = 1:nrow(iris2))

iris3 <- inner_join(iris2, df1, by="id")

iris3

#### order the data by a column
iris3 %>%
  arrange(Sepal.Length)

iris3 %>%
  arrange(desc(Petal.Width))

#### counting
iris3 %>% tally()

iris3 %>% group_by(Species) %>% tally()

iris3 %>% group_by(Species) %>% summarise(cnt = n())

######## complex transformation
### Get the minimum, maximum and average of the height and mass, the count and the 
### number of males, females and those without a defined gender, of the characters of 
### the movie 'starwars' that appeared in the film "Attack of the Clones" 
### and by their homeworld procedence

head(starwars)

starwars$films

mysw <- starwars %>%
  group_by(homeworld) %>% 
  mutate(male = ifelse(gender == "male",1,0),
         female = ifelse(gender == "female", 1,0),
         nogender = ifelse(is.na(gender)==T,1,0),
         attack_of_clones = ifelse("Attack of the Clones" %in% films,1,0)) %>%
  filter(attack_of_clones == 1) %>%
  summarise(height_min=min(height,na.rm=TRUE),
            height_mean=mean(height,na.rm=TRUE),
            height_max=max(height,na.rm=TRUE),
            mass_min=min(mass,na.rm=TRUE),
            mass_mean=mean(mass,na.rm=TRUE),
            mass_max=max(mass,na.rm=TRUE),
            males = sum(male, na.rm=TRUE),
            females = sum(female, na.rm=TRUE),
            nogender = sum(nogender, na.rm=TRUE),
            num_individuals=n()) %>%
  arrange(desc(num_individuals))



###########################################
###  Graphs in R
###########################################

x <- c(1,2,3,4,5,6,7,8)
y <- x + 15

plot(x, y) 

plot(x ~ y)  ## what happen with the axes?
lines(x ~ y)

plot(x, y, type='o') ## points and lines
plot(x, y, type='p') ## points
plot(x, y, type='l') ## lines
plot(x, y, type='b') ## both points and dotted lines
plot(x, y, type='c') ## dotted lines
plot(x, y, type='s') ## steped lines
plot(x, y, type='n') ## no dots or lines

###### adding horizontal and verical lines to a graph
plot(iris$Sepal.Length ~ iris$Sepal.Width)
## lets add to the graph a vertical line at the mean septal lenght
abline(h=mean(iris$Sepal.Length), col="red")
## and an horizontal line at the mean septal with
abline(v=mean(iris$Sepal.Width), col="red")

##############################
###### plot parameters
##############################

### defining the x and y limits
plot(x=NULL, xlim=c(1,10), ylim=c(1,11))

### line width
y <- 1 
for(n in seq(0.5,5,0.5)) {
  abline(h=y, lwd=n)
  y <- y + 1
}
### write the values of the line width 0.2 points over the line
text(x=rep(3,9),y = seq(1.2,10.2,1),labels = paste("lwd=",seq(0.5,5,0.5)))


### line type (6 types)
plot(x=NULL, xlim=c(1,7), ylim=c(1,7), xlab="X", ylab="Y")
y <- 1 
for(n in 1:6) {
  abline(h=y, lty=n)
  y <- y + 1
}
### write the values of the line width 0.2 points over the line
text(x=rep(3,6),y = seq(1.2,6.2,1),labels = paste("lty=",1:6))

### point symbols: there are 35, 25 accessible by numbers (1-25) and 10 accessible by 
### symbols ('*','+','-','.','|','%','#','o','O','0')

plot(x=NULL, xlim=c(1,5), ylim=c(0,6), xlab="X", ylab="Y")
for(x in 1:5) {
  for(y in 1:5) {
    p <- y+(5*(x-1))
    points(x,y,pch=p)
    text(x,y+0.2,labels=paste("pch=",p),cex=0.7)
  }
}


q <- c('*','+','-','.','|','%','#','o','O','0')
plot(x=NULL, xlim=c(0,3), ylim=c(0,6), xlab="X", ylab="Y")
for(x in 1:2) {
  for(y in 1:5) {
    p <- y+(2*(x-1))
    points(x,y,pch=q[p])
    text(x,y+0.2,labels=paste("pch=",q[p]),cex=0.7)
  }
}

#### symbols from 21 to 25 may be drawn in different colors:

cl <- 2:5
gr <- 21:25
plot(x=NULL, xlim=c(1,4), ylim=c(0,5), xlab="X", ylab="Y")
for(x in 1:4) {
  for(y in 1:4) {
    p <- y+(2*(x-1))
    points(x,y,pch=gr[x],bg=cl[y],cex=2)
    text(x,y+0.2,labels=paste("pch=",q[p]),cex=0.7)
  }
}

### barplot
barplot(df$mpg)

### histograms and boxplots
x <- rnorm(400, mean=40, sd=15)
hist(x)
y <- rbinom(1:400, 2, 0.3)
summary(y)
table(y)
boxplot(x ~ y)

t1 <- table(v2)
t1
pie(t1)

scatter.smooth(x)

### adding color to a plot
mycol <- ifelse(x >44,"red","blue")
scatter.smooth(x, col=mycol)

### adding a title and axis labels
scatter.smooth(x, 
               col=mycol, 
               main="Scatter plot", 
               xlab="Individuals", 
               ylab="Frequency", 
               ylim=c(-50,100))

legend(x="bottomright",
       fill=c("red","blue"), 
       col=c("red","blue") ,
       legend=c(">44","<=44"),
       cex=0.5,
       horiz = T)

### ploting many graphics at once
par(mfrow=c(2,2))
cl <- as.numeric(iris$Species)
plot(iris$Sepal.Length, main="Septal Lenght",col=cl)
plot(iris$Sepal.Width, main="Septal Width",col=cl)
plot(iris$Petal.Length, main="Petal Lenght",col=cl)
plot(iris$Petal.Width, main="Petal Width",col=cl)
par(mfrow=c(1,1))

### plotting time-series
j <- JohnsonJohnson
j
plot(j)

##############################
##########  ggplot2
##############################

library(ggplot2)

# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

### a simple graph
plot(iris3$Sepal.Length ~ iris3$Sepal.Width)

####################################
### a ggplot2 graph
####################################
### Main graph: ggplot(data = <mydata>, aes( x= <X_var>, y= <Y_var>) ) +
### geometry       geom_XXXXX()
ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio))

### adding color by Species
ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio, color=Species))

### define the shape of the points by species
ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio, shape=Species, color=Species))

### define the transparency by species
ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio, alpha=Species, color=Species))


### define size by Species 
ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio, size=Species))

### define stroke by Petal.Width (doesn't work with factors)
ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio, stroke=Petal.Length, 
                           color=Species))

### combining many properties in one graph
ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio, 
                           color=Species,shape=Species, 
                           alpha=Species, size=as.numeric(Species)))

########## Separate graphs for each class: Facets

ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio)) +
  facet_wrap(~ Species, nrow = 1) 

ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio, col=Species)) +
  facet_grid(round(Sepal.Length,0) ~ round(Petal.Length,0))

########### Combining two geometric objects into one graph

ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio)) 

ggplot(data=iris3) +
  geom_smooth(mapping = aes(x = Sepal.ratio, y = Petal.ratio)) 

ggplot(data = iris3) + 
  geom_smooth(mapping = aes(Sepal.ratio, y = Petal.ratio, 
                            linetype = Species, color=Species))

ggplot(data = iris3) + 
  geom_smooth(mapping = aes(Sepal.ratio, y = Petal.ratio, 
                            group=Species, color=Species))

### add two different geometries into one graph
ggplot(data = iris3) + 
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio, color=Species)) +
  geom_smooth(mapping = aes(x = Sepal.ratio, y = Petal.ratio, color=Species))

### Positioning the mapping in the initial graph definition
ggplot(data = iris3, mapping = aes(x = Sepal.ratio, y = Petal.ratio, color=Species)) + 
  geom_point() +
  geom_smooth()


### setting the color of the points by species
ggplot(data = iris3, mapping = aes(x = Sepal.ratio, y = Petal.ratio)) + 
  geom_point(mapping = aes(color = Species)) + 
  geom_smooth()

### filtering the second geometry data to show all out of setosa
ggplot(data = iris3, mapping = aes(x = Sepal.ratio, y = Petal.ratio)) + 
  geom_point(mapping = aes(color = Species)) + 
  geom_smooth(data = filter(iris3, Species != "setosa"))

######### Other Geometric objects 
ggplot(data = iris3) + 
  geom_histogram(mapping = aes(x=Sepal.Length))

ggplot(data = iris3,mapping = aes(x=Sepal.Length)) + 
  geom_histogram(bins = 30)

### Bar graph
iris3 <- iris3 %>%
  mutate(Sepal.Length.cat=factor(round(Sepal.Length,0)),
         Petal.Length.cat=factor(round(Petal.Length,0)))

ggplot(data = iris3) + 
  geom_bar(mapping = aes(x=Sepal.Length.cat))

#### Colored bars
ggplot(data = iris3) + 
  geom_bar(mapping = aes(x=Sepal.Length.cat, fill=Species))

#### stacked bars
ggplot(data = iris3) + 
  geom_bar(mapping = aes(x=Sepal.Length.cat, fill=Species), position = "fill" )

### Dodge
ggplot(data = iris3) + 
  geom_bar(mapping = aes(x=Sepal.Length.cat, fill=Species), position = "dodge" )

####### Boxplots
ggplot(data = iris3) + 
  geom_boxplot(mapping = aes(x=Species,y=Sepal.ratio))

ggplot(data = iris3) + 
  geom_boxplot(mapping = aes(x=Species,y=Sepal.ratio)) +
  coord_flip()

##################################
#####  Extended layered grammar
##################################
# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(
#   mapping = aes(<MAPPINGS>),
#   stat = <STAT>, 
#   position = <POSITION>
#   ) +
#   <COORDINATE_FUNCTION> +
#   <FACET_FUNCTION>
##################################

ggplot(data = iris3) + 
  geom_bar(mapping = aes(x = Sepal.Length.cat, fill=Species)) 

ggplot(data = iris3) + 
  geom_bar(mapping = aes(x = Sepal.Length.cat, fill=Species)) +
  coord_polar() 

####################
#summary(mysw)
#table(mtcars$cyl)
# mpg - wt - cyl

#ggplot(data = mtcars) +
#  geom_point(mapping=aes(x = mpg, y = wt, color=factor(cyl))) +
#  facet_grid(~cyl) +
#  coord_flip()



#########################################################
#####   Import / Export data
#########################################################

###### CSV (comma separated text)

write.csv(iris, file="iris.csv")
write.csv(iris, file="iris.csv",row.names = F)

mydata <- read.csv(file="iris.csv")
class(mydata)
str(mydata)

####### Excel
library(xlsx)
df1 <- read.xlsx("excel-example.xlsx",sheetIndex = 1)
df2 <- read.xlsx("excel-example.xlsx",sheetIndex = 2)

### Write a data.frame to an excel file
write.xlsx(df1, "one-sheet-example.xlsx", sheetName="Data Frame")


### generate some worksheets and add them to a workbook, then save it into an excel file
library(dplyr)

countries <- df1 %>% 
  group_by(Country) %>% 
  summarise(age_mean=mean(Age,na.rm=T),
            income_mean=mean(Income, na.rm=T),
            income_min=min(Income, na.rm=T),
            income_max=max(Income, na.rm=T),
            count=n())

calories <- df2 %>% 
  group_by(Diet) %>% 
  summarise(caliries_mean=mean(Calories_per_day,na.rm=T),
            caliries_min=min(Calories_per_day,na.rm=T),
            caliries_max=max(Calories_per_day,na.rm=T),
            count=n())

#### pass the tables to an excel file

file <- "excel-example2.xlsx"

wb <- createWorkbook()
sheet1 <- createSheet(wb, sheetName="Countries")
sheet2 <- createSheet(wb, sheetName="Calories")
sheet3 <- createSheet(wb, sheetName="Diet")
sheet4 <- createSheet(wb, sheetName="Income")

addDataFrame(countries,sheet1)
addDataFrame(calories,sheet2)
addDataFrame(df1,sheet3)
addDataFrame(df2,sheet4)

saveWorkbook(wb, file)


################################
##### SPSS / SAS / STATA
###############################

#### SPSS
library(foreign)
sav <- read.spss(file="c:/mydata.sav", to.data.frame=TRUE) 

#### SAS
library(Hmisc)
sap <- sasxport.get("c:/mydata.xpt")

#### STATA
library(foreign)
stata <- read.dta("c:/mydata.dta")

##########################
##### HTML / XML
##########################

###### HTML

library(XML)

url <- "http://www.google.com/search?q=introduction+to+r"
doc <- htmlParse(url)
links <- xpathSApply(doc, "//a/@href")
free(doc)

length(links)

links[[1]]
links[[20]]

###### XML

myxml <- "<foo>
  <bar>text <baz id = 'a' /></bar>
  <bar>2</bar>
  <baz id = 'b' />
</foo>"

xmldoc <- xmlParse(myxml)
rootNode <- xmlRoot(xmldoc)
rootNode[1]
rootNode[2]

########################
#### JSON files
########################
library(jsonlite)

json_file <- "http://api.worldbank.org/country?per_page=10&region=OED&lendingtype=LNX&format=json"
json_data <- fromJSON(json_file)

json_data[[1]]$per_page

json_df <- as.data.frame(json_data)


########################
#### DATABASE
########################

#### To be able to query the database we need to create a new ODBC DNS on the windows computer


library(DBI)
con <- dbConnect(odbc::odbc(), "tcds")
sql <- "SELECT * FROM acs2015_country_data"
acs <- dbGetQuery(con, sql)
dbDisconnect(con)

