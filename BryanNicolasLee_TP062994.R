
#Bryan Nicolas Lee
#TP062994

#Library used

library(ggplot2)
library(corrplot)
library(car)
library(janitor)
library(CGPfunctions)
library(viridis)
library(ggpubr)
library(lessR)
library(plyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(tidyr)
library(forcats)
library(janitor)


#Improting and viewing the Data
data = read.csv("C:\\Users\\BNL\\Downloads\\House_Rent_Dataset.csv", header = TRUE)
View(data)

#Data Exploration

summary(data)
head(data)
dim(data)
nrow(data)
ncol(data)

View(data %>% summarise_all(n_distinct))
#Extra Feature - summarise_all() – Summarise all columns/variables
#Extra Feature – n_distinct - counts the number of unique values in a vector or variable



#Check for any missing values
sum(is.na(data))


#Data Preprocessing



class(data$Posted.On) 
datedf <- as.Date(data$Posted.On,"%m/%d/%Y")
#Extra Feature: as.Date() - convert character data to dates 
changedf <- data.frame(date = datedf,
                       year = as.numeric(format(datedf, format = "%Y")),
                       month = as.numeric(format(datedf, format = "%m")),
                       day = as.numeric(format(datedf, format = "%d")))
data <- mutate(data,Day=changedf$day)
data <- mutate(data,Month=changedf$month)
data <- mutate(data,Year=changedf$year)
View(data)

month.abb[unique(data$Month)]
#Extra Feature: month.abb() - three-letter abbreviations for the English month names

data <- mutate(data,Month=month.abb[data$Month])
head(data)


View(data)



#Seperation for floors

unique(data$Floor)

data <- data %>% 
  separate(Floor, into=c("FirstFloor", "TotalFloor")," out of ")
#Extra Feature: seperate() - separate a single data frame or table column into multiple columns

# Removal of NA total floors
data <- data[complete.cases(data[,6]),] # No Total Floor (2323,2638,4213,4282)
#Extra Feature: complete.cases() - used to return a logical vector with cases which are complete

View(rdata)
unique(data$`FirstFloor`)
unique(data$`TotalFloor`)
#Making Ground, Upper, Lower Basement into numerical
unique(data$`FirstFloor`)
data[data == 'Ground'] <- 0
data[data == 'Upper Basement'] <- -1
data[data == 'Lower Basement'] <- -2

#Data Transformation

#transform outliers
transform_outliers <- function(data, column, low, high){
  Q <- quantile(data[,column], probs = c(low, high), na.rm = FALSE)
  iqr<- IQR(data[,column])
  up <- Q[2] + 1.5*iqr
  down <- Q[1] - 1.5*iqr
  new_data <- subset(data, data[,column]> down & data[,column] < up)
  outlier <- subset(data, data[,column] <down)
  outlier <- rbind(outlier, subset(data, data[,column] > up))
  outlier[,column] = as.integer(mean(new_data[, column]))
  new_data <- rbind(new_data,outlier)
  return (new_data)
}
#Extra Feature: quantile() - to create sample quantiles within a data set with probability[0, 1]
#Extra Feature: na.rm() -  the logical parameter that tells the function whether or not to remove NA values from the calculation
#Extra Feature: IQR() - to compute the interquartile range of a given object of numerical values
#Extra Feature: subset() - create subsets of a Data frame

sdata <- transform_outliers(data,"Size",0.01,0.999)
rdata <- transform_outliers(sdata,"Rent",0.01,0.90)


rdata <- transform(rdata, FirstFloor = as.numeric(FirstFloor))
rdata <- transform(rdata, TotalFloor = as.numeric(TotalFloor))



#Data Analysis

#Q1 Looking at the numbered values, is there any correlation between them?

#Analysis 1.1: Find any Correlation between all the values
cor_mtrix <- cor(data[,c(2,3,4,12)], )
#Extra Feature: cor() - used to measure the correlation coefficient value between two vectors
round(cor_mtrix,
      digits = 2 # rounded to 2 decimals
)
#Analysis 1.2: Which have the least correlation with each other?
heatmap(cor_mtrix)
#corrplot ordered by Coefficience value
#Analysis 1.3: Which have the Highest correlation with each other? 
corrplot(cor_mtrix, methods="circle", order = "hclust", addrect = 2 ) 
#Extra Feature: Correlation Plot – To visualize correlation between continuous variables.




#By using Cor plot we can see high correlation between BHK,SIZE & Bathroom

#Q2 Following the Correlation Diagram, How does each column affect the column for BHK


bhk <- data$BHK
size <- data$Size
bathroom <- data$Bathroom


#Analysis 2.1: Find the relationship between BHK and Bathroom
PlotXTabs(rdata,BHK,Bathroom,"stack")
#Extra Feature: PlotxTabs – conducts a crosstabulation of two or more variables

#Analysis 2.2: Find relationship between BHK and Size

  #Scatter Graph
dev.off()
q1corrsize <- plot(bhk,size,main = "Correlation of BHK and Size",xlab= "BHK Values", ylab = "House Size",
      frame = FALSE)
abline(lm(size ~ bhk, data = data), col = "red", lwd=2.0) #BEST FIT LINE
lines(lowess(bhk,size), col = "blue", lwd=2.0) #LOESS FIT


#Analysis 2.3: Find how much does a change in BHK affect the House Size?

q1estimate <- lm(size ~ bhk, data = data) # Estimate linear regression model
q1coef <- coef(q1estimate) # Extract coefficients of model
q1_equation <- paste("y =",coef(q1estimate)[[1]], "+",coef(q1estimate)[[2]],"x")
q1_equation
#Extra Features: lm() - used to fit linear models to data frames
#               coef() - generic function which extracts model coefficients from objects returned by modeling functions


# Question 3: Following the Correlation Diagram, how much Bathroom affects the fields with numbered data?
#Bathroom and BHK

#Analysis 3.1: Find the relationship of Bathroom towards BHK
PlotXTabs(data,Bathroom,BHK,"stack")  #Crosstab Data

#Analysis 3.2: How much exactly is the correlation of Bathroom and BHK?
tabyl(data, Bathroom,BHK) #BHK count by Bathroom
#Extra Feature: tabyl - produces formatted two-way crosstabs that display count for the interaction of two categorical variables

#Analysis 3.3: Find the relationship of Bathroom towards Size
plot(bathroom,size,main = "Correlation of Bathroom and Size",xlab= "House Size", ylab = "No of Bathrooms",
                   frame = FALSE)
abline(lm(size ~ bathroom, data = data), col = "red", lwd=2.0) #BEST FIT LINE
lines(lowess(bathroom,size), col = "blue", lwd=2.0) #LOESS FIT

scatterplot(size ~ bathroom, data = data,xlab= "No of Bathrooms", ylab = "House Size")

#Analysis 3.4: Find how much does a change in Bathroom affect the House Size?

q1estimate <- lm(size ~ bathroom, data = data) # Estimate linear regression model
q1coef <- coef(q1estimate) # Extract coefficients of model
q1_equation <- paste("y =",coef(q1estimate)[[1]], "+",coef(q1estimate)[[2]],"x")
q1_equation

#Q4  Following the Correlation Diagram, how much Size affects the fields with numbered data?
#Size & BHK

#Analysis 4.1: Find the relationship of Size towards BHK
  
plot(size,bhk,main = "Correlation of BHK and Size",xlab= "House Size", ylab = "BHK", pch = 19, frame = FALSE)
  abline(lm(bhk ~ size, data = data), col = "red") #BEST FIT LINE
  lines(lowess(size,bhk), col = "blue") #LOESS FIT
  
  
  q3scat2 <- scatterplot(bhk ~ size, data = data,xlab= "House Size", ylab = "BHK")
  #Extra Feature: scatterplot() - Makes enhanced scatterplots, with boxplots in the margins, a nonparametric regression smooth, smoothed conditional spread, outlier identification, and a regression line
 
#Analysis 4.2: Find how much does a change in House Size affect the BHK?
  q1estimate <- lm(bhk ~ size, data = data) # Estimate linear regression model
  q1coef <- coef(q1estimate) # Extract coefficients of model
  q1_equation <- paste("y =",coef(q1estimate)[[1]], "+",coef(q1estimate)[[2]],"x")
  q1_equation

#Analysis 4.3: Find the relationship of Size towards Bathroom
  
  q3scat <- plot(size,bathroom,main = "Correlation of BHK and Size",xlab= "No of Bathrooms", ylab = "House Size",
                 pch = 19, frame = FALSE)
  abline(lm(bathroom ~ size, data = data), col = "red") #BEST FIT LINE
  lines(lowess(size,bathroom), col = "blue") #LOESS FIT
  
  
  q3scat2 <- scatterplot(bathroom ~ size, data = data,xlab= "No of Bathrooms", ylab = "House Size")
  
#Analysis 4.4: Find how much does a change in House Size affect the Bathroom?
  
  q1estimate <- lm(bathroom ~ size, data = data) # Estimate linear regression model
  q1coef <- coef(q1estimate) # Extract coefficients of model
  q1_equation <- paste("y =",coef(q1estimate)[[1]], "+",coef(q1estimate)[[2]],"x")
  q1_equation

#Q5 To what extent does Furnishing Status affect other columns?
  
  #Analysis 5.1: Find which Furnishing Status is the most frequent as well as least frequent
    ggplot(rdata,aes(x=Furnishing.Status, fill =..count..),stat = "count") + 
    geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")
  
  #Analysis 5.2: Find the effect that Furnishing status has on the BHK of Housing Rentals
  PlotXTabs(rdata,BHK,Furnishing.Status,"stack")
  
  #Analysis 5.3: Find the effect that Furnishing status has on the Rent of Housing Rentals
  ggplot(rdata, aes(x=Furnishing.Status, y=Rent, fill=Furnishing.Status)) + 
    geom_violin()+
    geom_boxplot(width=0.1, color="black", alpha=0.2)
  #Extra Feature: geom_violin() - used to produce a violin plot displaying a continuous distribution
  
  #Analysis 5.4: Find the effect that Furnishing status has on the Sizes of Housing Rentals
  ggplot(rdata, aes(x=Furnishing.Status, y=Size, fill=Furnishing.Status)) + 
    geom_violin()+
    geom_boxplot(width=0.1, color="black", alpha=0.2)
  
  #Analysis 5.5: Find the effect that Furnishing status has on the Area Type of Housing Rentals
  PlotXTabs(rdata,Area.Type,Furnishing.Status,"stack")
  
  #Analysis 5.6: Find the effect that Furnishing status has on the tenants preferred for Housing Rentals
  PlotXTabs(rdata,Tenant.Preferred,Furnishing.Status,"stack")
  
  
#Q6 To what extent does Area Type affect the other features of the Rental Housings?
  
  #Analysis 6.1: Find which Area Type is the most frequent as well as least frequent
  ggplot(rdata,aes(x=Area.Type, fill =..count..),stat = "count") + 
    geom_bar() + 
    geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")
  
  #Analysis 6.2: Find the effect that Area Type has on the BHK of Housing Rentals
  PlotXTabs(rdata,BHK,Area.Type,"stack")
  
  #Analysis 6.3: Find the effect that Area Type has on the BHK of Housing Rentals
  ggplot(rdata, aes(x=Area.Type, y=Rent, fill=Area.Type)) + 
    geom_violin()+
    geom_boxplot(width=0.1, color="black", alpha=0.2)
  
  # Analysis 6.4: Find the effect that Area Type has on the Sizes of Housing Rentals
  
  ggplot(rdata, aes(x=Area.Type, y=Size, fill=Area.Type)) + 
    geom_violin()+
    geom_boxplot(width=0.1, color="black", alpha=0.2)
  
  #Analysis 6.5: Find the effect that Area Type has on the Furnishing statuses of Housing Rentals
  PlotXTabs(rdata,Furnishing.Status,Area.Type,"stack")
  
  #Analysis 6.6: Find the effect that Area Type has on the tenants preferred for Housing Rentals
  PlotXTabs(rdata,Tenant.Preferred,Area.Type,"stack")
  
#Q7 Which City is the most affordable for tenants looking to rent on a budget? 
View(rdata)

max(rdata$Rent)

  #Analysis 7.1: Find the affordability of the city Kolkata
kolkata <- rdata$City == "Kolkata"
kolkatag<-ggplot(rdata[kolkata,], aes(x=Rent))+ 
  geom_histogram(aes(y=..density..), colour="blue", fill = "gray") + 
  geom_density(wd = 1, colour = 4,fill = 4, alpha = 0.25)   
  #Extra Feature: geom_density() - Computes and draws kernel density estimate
  labs(y="Density", x="Rent Kolkota")
kolkatag
  #Analysis 7.2: Find the affordability of the city Mumbai
mumbai <- rdata$City == "Mumbai"
mumbaig<-ggplot(rdata[mumbai,], aes(x=Rent))+ 
  geom_histogram(aes(y=..density..), colour="blue", fill = "gray")+ 
  geom_density(wd = 1, colour = 4,fill = 4, alpha = 0.25)  + 
  labs(y="Density", x="Rent Mumbai")
mumbaig
  #Analysis 7.3: Find the affordability of the city Bangalore
bangalore <- rdata$City == "Bangalore"
bangaloreg <-ggplot(rdata[bangalore,], aes(x=Rent))+ 
  geom_histogram(aes(y=..density..), colour="blue", fill = "gray")+ 
  geom_density(wd = 1, colour = 4,fill = 4, alpha = 0.25)  + 
  labs(y="Density", x="Rent Bangalore")

  #Analysis 7.4: Find the affordability of the city Delhi
delhi <- rdata$City == "Delhi"
delhig <- ggplot(rdata[delhi,], aes(x=Rent))+ 
  geom_histogram(aes(y=..density..), colour="blue", fill = "gray")+ 
  geom_density(wd = 1, colour = 4,fill = 4, alpha = 0.25)  + 
  labs(y="Density", x="Rent Delhi")
delhig

  #Analysis 7.5: Find the affordability of the city Chennai
chennai <- rdata$City == "Chennai"
chennaig <-ggplot(rdata[chennai,], aes(x=Rent))+ 
  geom_histogram(aes(y=..density..), colour="blue", fill = "gray")+
  geom_density(wd = 1, colour = 4,fill = 4, alpha = 0.25)  + 
  labs(y="Density", x="Rent Chennai")
chennaig

  #Analysis 7.6: Find the affordability of the city Hyderabad
hyderabad <- rdata$City == "Hyderabad"
hyderabadg <- ggplot(rdata[hyderabad,], aes(x=Rent))+ 
  geom_histogram(aes(y=..density..),colour="blue", fill = "gray")+ 
  geom_density(wd = 1, colour = 4,fill = 4, alpha = 0.25) + 
labs(y="Density", x="Rent Hyderabad")
hyderabadg

  #Analysis 7.7: Find a pattern comparing the rent distributions of each city
 ggarrange(kolkatag,mumbaig,bangaloreg,delhig,chennaig,hyderabadg,
                    labels = c("K","M", "B", "D", "C", "H"),
                    ncol = 2, nrow = 3)
 #Extra Feature: ggarange() - to arrange multiple ggplots over multiple pages

  #Analysis 7.8: To confirm the distribution analysis, what is the Average rent for each city?

mean_rent_table <- rdata %>% group_by(City) %>%
  summarise(Mean_Rent = mean(Rent),
            .groups = 'drop')
mean_rent_df <- mean_rent_table %>% as.data.frame(order(Mean_Rent))
sorted_meanrent_df <- mean_rent_df[order(-mean_rent_df$Mean_Rent),]
#Extra Feature: order() – To give order to the given numbers
sorted_meanrent_df

  #Bar chart for mean of rent


 ggplot(sorted_meanrent_df,aes(x = fct_reorder(City,-Mean_Rent),Mean_Rent)) + 
   geom_bar(stat = "identity",color = "blue", fill = "light blue") + 
   geom_text(aes(label = signif(Mean_Rent,5), vjust = 1.5), colour = "black")

rent_bar

#Q8 Which Areas are the most affordable in each city?

# Area locality 
unique(rdata$Area.Locality) #1233 different entries

#Taking the most frequent area localities only for each city

#Most Frequent area localities for city function
filt_areas_city <- function(cityloc){
locdata <- filter(rdata,City == cityloc)
loccount <- tabyl(locdata,Area.Locality)
locsorted <- loccount %>% arrange(desc(n))
#Extra Feature: arrange() - to reorder the rows of a data frame
toploc <- head(locsorted,5)
toploc
}
#Gather Mean Rent of Area Locality and sort by rent Function
filt_areas_rent <- function(filt){
  filtloc <- filt %>% group_by(Area.Locality) %>%
    summarise(Mean_Rent = mean(Rent),.groups = 'drop')
  locdf <- filtloc %>% as.data.frame(order(Mean_Rent))
  sorted_meanrent_df <- locdf[order(locdf$Mean_Rent),]
  sorted_meanrent_df
}
#Analysis 8.1: What areas do most housing rentals in Kolkata reside in? 
filt_areas_city("Kolkata")
kolfilt <- filter(rdata, Area.Locality == "Salt Lake City Sector 2" | 
                    Area.Locality == "Behala" |
                    Area.Locality == "Salt Lake City Sector 1" |
                    Area.Locality == "Kasba" |
                    Area.Locality == "Salt Lake City Sector 5")
unique(kolfilt$Area.Locality)
#Analysis 8.2: What areas do most housing rentals in Mumbai reside in? 
filt_areas_city("Mumbai") 
mumfilt <- filter(rdata, Area.Locality == "Bandra West" | 
                    Area.Locality == "Chembur" |
                    Area.Locality == "Andheri West" |
                    Area.Locality == "Goregaon West" |
                    Area.Locality == "Khar West")
unique(mumfilt$Area.Locality)
#Analysis 8.3: What areas do most housing rentals in Bangalore reside in? 
filt_areas_city("Bangalore")
banfilt <- filter(rdata, Area.Locality == "Electronic City" | 
                    Area.Locality == "K R Puram" |
                    Area.Locality == "Murugeshpalya, Airport Road" |
                    Area.Locality == "Mahadevapura" |
                    Area.Locality == "Hebbal")
unique(banfilt$Area.Locality)
#Analysis 8.4: What areas do most housing rentals in Delhi reside in? 
filt_areas_city("Delhi")
delfilt <- filter(rdata, Area.Locality == "Laxmi Nagar" | 
                    Area.Locality == "Chhattarpur" |
                    Area.Locality == "kst chattarpur Apartments" |
                    Area.Locality == "Saket" |
                    Area.Locality == "Vasant Kunj")
unique(delfilt$Area.Locality)
#Analysis 8.5: What areas do most housing rentals in Chennai reside in? 
filt_areas_city("Chennai")
chefilt <- filter(rdata, Area.Locality == "Velachery" | 
                    Area.Locality == "Chhattarpur" |
                    Area.Locality == "Iyyappanthangal" |
                    Area.Locality == "Medavakkam" |
                    Area.Locality == "Sholinganallur")
unique(chefilt$Area.Locality)
#Analysis 8.6: What areas do most housing rentals in Hyderabad reside in? 
filt_areas_city("Hyderabad")
hydfilt <- filter(rdata, Area.Locality == "Gachibowli" | 
                    Area.Locality == "Miyapur, NH 9" |
                    Area.Locality == "Kondapur" |
                    Area.Locality == "Banjara Hills, NH 9" |
                    Area.Locality == "Attapur")
unique(hydfilt$Area.Locality)


#Analysis 8.7: What areas of the housing rentals in Kolkata are the costliest to rent on? 
mrentkolloc <- filt_areas_rent(kolfilt)
ggplot(mrentkolloc ,aes(x=Area.Locality, y=Mean_Rent, fill = Area.Locality)) + 
  geom_bar(stat='Identity') + 
  labs(title='Average Rent by Area Locality in Kolkata', x='Area Locality', y='Average Rent') +
  geom_text(aes(label=signif(Mean_Rent)), vjust=2, color = "white")

#Analysis 8.8: What areas of the housing rentals in Mumbai are the costliest to rent on? 
mrentmumloc <- filt_areas_rent(mumfilt)
ggplot(mrentmumloc ,aes(x=Area.Locality, y=Mean_Rent, fill = Area.Locality)) +  
  geom_bar(stat='Identity')+ 
  labs(title='Average Rent by Area Locality in Mumbai', x='Area Locality', y='Average Rent') +
  geom_text(aes(label=signif(Mean_Rent)), vjust=2, color = "white")

#Analysis 8.9: What areas of the housing rentals in Bangalore are the costliest to rent on? 
mrentbanloc <- filt_areas_rent(banfilt)
ggplot(mrentbanloc ,aes(x=Area.Locality, y=Mean_Rent, fill = Area.Locality)) +  
  geom_bar(stat='Identity')+ 
  labs(title='Average Rent by Area Locality in Bangalore', x='Area Locality', y='Average Rent')+
  geom_text(aes(label=signif(Mean_Rent)), vjust=2, color = "white")

#Analysis 8.10: What areas of the housing rentals in Delhi are the costliest to rent on? 
mrentdelloc <- filt_areas_rent(delfilt)
ggplot(mrentdelloc ,aes(x=Area.Locality, y=Mean_Rent, fill = Area.Locality)) +  geom_bar(stat='Identity')+ 
  labs(title='Average Rent by Area Locality in Delhi', x='Area Locality', y='Average Rent')+
  geom_text(aes(label=signif(Mean_Rent)), vjust=2, color = "white")

#Analysis 8.11: What areas of the housing rentals in Chennai are the costliest to rent on? 
mrentcheloc <- filt_areas_rent(chefilt)
ggplot(mrentcheloc ,aes(x=Area.Locality, y=Mean_Rent, fill = Area.Locality)) +  
  geom_bar(stat='Identity')+ 
  labs(title='Average Rent by Area Locality in Chennai', x='Area Locality', y='Average Rent')+
  geom_text(aes(label=signif(Mean_Rent)), vjust=2, color = "white")

#Analysis 8.12: What areas of the housing rentals in Hyderabad are the costliest to rent on? 
mrenthydloc <- filt_areas_rent(hydfilt)
ggplot(mrenthydloc ,aes(x=Area.Locality, y=Mean_Rent, fill = Area.Locality)) +  
  geom_bar(stat='Identity')+ 
  labs(title='Average Rent by Area Locality in Hyderabad', x='Area Locality', y='Average Rent')+
  geom_text(aes(label=signif(Mean_Rent)), vjust=2, color = "white")



#Q9.1 What are the most average housing rentals for Bachelors 
#(This is to get what an average housing criteria values(control values) are for bachelors)

  #Analysis 9.1.1: How many Housing Rentals prefer bachelor’s as tenants? 

ggplot(rdata,aes(x=Tenant.Preferred, fill =..count..),stat = "count") + 
  geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")

#830

  #Analysis 9.1.2: Find average Rent for Bachelor only Rental Housing 
bachdata <- filter(rdata,Tenant.Preferred == "Bachelors")
mean(bachdata$Rent)

  #Analysis 9.1.3: Find how many Bachelor only Rental Housing falls behind the Mean Rent and above it 

highbachrent <- bachdata$Rent > 26622.11
bachrenttable <- table(highbachrent)
bachrenttable
truefalsedata <- c(553,277)
bachrentpiepercent <- round(100*truefalsedata/sum(truefalsedata), 1)
pie(bachrenttable,labels = bachrentpiepercent, main = "Pie chart for Bachelor Rent > 26622",
    col = rainbow(length(bachrenttable)))
legend("topright",c("False","True"),cex = 0.8,fill = rainbow(length(bachrenttable))) 
#Extra Feature: rainbow() – Generates color palettes which can be used to quickly generate color vectors of desired length taken as the parameter.

#Analysis 9.1.4: Find Bachelors only housing rent range and Bachelors only housing rentals that fall into this range
bachrentsd <- signif(sd(bachdata$Rent),7)
#Extra Feature: sd() <- Finding the Standard Deviation of given values in R.
bachrentsd #20839.88
rentbachrent <- filter(bachdata,Rent < (mean(bachdata$Rent) + 0.5*bachrentsd) & 
                         Rent > (mean(bachdata$Rent) - 0.5*bachrentsd) )  
View(rentbachrent)

#Analysis 9.1.5: Find average Size for Bachelor only Rental Housing 

bachsizesd <- signif(sd(rentbachrent$Size),3) #780
signif(mean(rentbachrent$Size),4) #1180
highbachsize <- rentbachrent$Size > 1180
bachsizetable <- table(highbachsize)
truefalsedata <- c(163,125)
bachrentpiepercent <- round(100*truefalsedata/sum(truefalsedata), 1)
#Extra Feature: round() - round off values to a specific number of decimal value
pie(bachsizetable,labels = bachrentpiepercent, main = "Pie chart for Size > Mean Data",
    col = rainbow(length(bachsizetable)))
legend("topright",c("False","True"),cex = 0.8,fill = rainbow(length(bachsizetable)))

#Analysis 9.1.6: Find Bachelors only housing sizes range and Bachelors preferred rental housings that fall into this range
bachsizesd <- signif(sd(rentbachrent$Size),3)
sizebach <- filter(rentbachrent, Size < (mean(rentbachrent$Size) + 0.5*bachsizesd) & 
                     Size > (mean(rentbachrent$Size) - (0.5*bachsizesd)))
bachsizesd

#Analysis 9.1.7: Find Bachelors only Housing Rentals most preferred Area Type
area_type_bachelors <- sizebach$Area.Type
df <- data.frame(area_type_bachelors)
areatypebach_bar <- ggplot(df,aes(area_type_bachelors, fill =..count..), stat = "count") + 
  geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")
areatypebach_bar #Carpet Area

areatypebach <- filter(sizebach, Area.Type == "Carpet Area")
View(areatypebach)

#Analysis 9.1.8: Find Bachelors only Housing Rentals most preferred Furnishing Status
furnishing_status_bachelors <- areatypebach$Furnishing.Status
df <- data.frame(furnishing_status_bachelors)
furnishingstatusbach_bar <- ggplot(df,aes(furnishing_status_bachelors, fill =..count..), stat = "count") +
  geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")
furnishingstatusbach #Semi Furnished
furnishingstatusbach <- filter(areatypebach, Furnishing.Status == "Semi-Furnished")
View(furnishingstatusbach)

#Analysis 9.1.9: Find Bachelors only Housing Rentals most preferred BHK
bhkbachelors <- furnishingstatusbach$BHK
df <-data.frame(bhkbachelors)
bhkbach_bar <- ggplot(df,aes(bhkbachelors, fill =..count..), stat = "count") + 
  geom_bar(width = 0.5) + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")
bhkbach_bar #2
bhkbach <- filter(furnishingstatusbach, BHK == 2)
View(bhkbach)

#Analysis 9.1.10: Find Bachelors only Housing Rentals most preferred Bathroom Numbers

bathroombachelors <- bhkbach$Bathroom
df <- data.frame(bathroombachelors)
bathroom_bar <-  ggplot(df,aes(bathroombachelors, fill =..count..), stat = "count") + 
  geom_bar(width = 0.5) + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")
bathroom_bar
bathroombach <- filter(bhkbach, Bathroom == 2)
View(bathroombach)

#Analysis 9.1.11: Find Bachelors only preferred Housing Rentals most resided in City

citybachelors <- bathroombach$City
df <- data.frame(citybachelors)
city_bar <-  ggplot(df,aes(citybachelors, fill =..count..), stat = "count") + geom_bar(width = 0.5) + 
  geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")
city_bar
citybach <- filter(bathroombach, City == "Bangalore")
View(citybach)

#Rental Houses with the most popular House Properties for Bachelor

famousbach <- citybach
View(famousbach)

#FAMOUS PROPERTIES
# Rent = 16202.17 < Rent < 37042.05
# Size = 790 < Size < 1570
# Area Type = Carpet Area
# Furnishing Status = Semi Furnished
# BHK = 2
# Bathrooms = 2
# City = Bangalore

#9.2 What is the most average housing rentals for Family
#(This is to get what an average housing criteria values(control values) are for Family)

#Analysis 9.2.1: How many Housing Rentals prefer Family as tenants? 
ggplot(rdata,aes(x=Tenant.Preferred, fill =..count..),stat = "count") + 
  geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")
#471

##Analysis 9.2.2: Find average Rent for Family only Rental Housing 
View(rdata)
famdata <- filter(rdata,Tenant.Preferred == "Family")
mean(famdata$Rent) #28731.55

#Analysis 9.2.3: Find how many Family only Rental Housing falls behind the Mean Rent and above it 
famrentsd <- signif(sd(famdata$Rent),7) #20765.71
highfamrent <-famdata$Rent > 28731.55
famrenttable <- table(highfamrent)
truefalsedata <- c(312,159)
famrentpiepercent <- round(100*truefalsedata/sum(truefalsedata), 1)
pie(famrenttable,labels = famrentpiepercent, main = "Pie chart for Rent > Mean Data",col = rainbow(length(famrenttable)))
legend("topright",c("False","True"),cex = 0.8,fill = rainbow(length(famrenttable))) 

#Analysis 9.2.4: Find Family only housing rent range and Family only housing rentals that fall into this range
famrentsd <- signif(sd(famdata$Rent),7)
rentfamrent <- filter(famdata,Rent < (mean(famdata$Rent) + 0.5*famrentsd) &
                        Rent > (mean(famdata$Rent) - 0.5*famrentsd) )  
View(rentfamrent)

#Analysis 9.2.5: Find average Size for Family only Rental Housing 
max(famdata$Size)
famsizesd <- signif(sd(rentfamrent$Size),3) #792
signif(mean(rentfamrent$Size),4) #1348
highfamsize <- rentfamrent$Size > 1348
famsizetable <- table(highfamsize)
famsizetable
truefalsedata <- c(109,89)
famsizepiepercent <- round(100*truefalsedata/sum(truefalsedata), 1)
pie(famsizetable,labels = famsizepiepercent, main = "Pie chart for Size > Mean Data",col = rainbow(length(famsizetable)))
legend("topright",c("False","True"),cex = 0.8,fill = rainbow(length(famsizetable)))

#Analysis 9.2.6: Find Family only housing sizes range and Family preferred rental housings that fall into this range
famsizesd <- signif(sd(rentfamrent$Size),3)
sizefam <- filter(rentfamrent, Size < (mean(rentfamrent$Size) + 0.5*famsizesd) & Size > (mean(rentfamrent$Size) - (0.5*famsizesd)))
View(sizefam)

#Analysis 9.2.7: Find Family only Housing Rentals most preferred Area Type
area_type_family <- sizefam$Area.Type
df <- data.frame(area_type_family)
tabyl(df,area_type_family)
ggplot(df,aes(area_type_family, fill =..count..), stat = "count") +
  geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")

areatypefam <- filter(sizefam, Area.Type == "Carpet Area")
View(areatypefam)

#Analysis 9.2.8: Find Family only Housing Rentals most preferred Furnishing Status
furnishing_status_family <- areatypefam$Furnishing.Status
df <- data.frame(furnishing_status_family)
furnishingstatusfam_bar <- ggplot(df,aes(furnishing_status_family, fill =..count..), stat = "count") +
  geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")
furnishingstatusfam_bar #Semi Furnished
furnishingstatusfam <- filter(areatypefam, Furnishing.Status == "Semi-Furnished")
View(furnishingstatusfam)

#Analysis 9.2.9: Find Family only Housing Rentals most preferred BHK
bhkfamily <- furnishingstatusfam$BHK
df <-data.frame(bhkfamily)
bhkfam_bar <- ggplot(df,aes(bhkfamily, fill =..count..), stat = "count") + 
  geom_bar(width = 0.5) + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")
bhkfam_bar #2
bhkfam <- filter(furnishingstatusfam, BHK == 3)
View(bhkfam)

#Analysis 9.2.10: Find Family only Housing Rentals most preferred Bathroom Numbers

bathroomfamily <- bhkfam$Bathroom
df <- data.frame(bathroomfamily)
bathroom_bar <-  ggplot(df,aes(bathroomfamily, fill =..count..), stat = "count") + 
  geom_bar(width = 0.5) + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white") + 
  labs(title='Family Bathroom Count', x='Bathroom', y='Count')
bathroom_bar
bathroomfam <- filter(bhkfam, Bathroom == 3)
View(bathroomfam)

#Analysis 9.2.11: Find Family only preferred Housing Rentals most resided in City

cityfamily <- bathroomfam$City
df <- data.frame(cityfamily)
city_bar <-  ggplot(df,aes(cityfamily, fill =..count..), stat = "count") + 
  geom_bar(width = 0.5) + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")
city_bar
cityfam <- filter(bathroomfam, City == "Hyderabad")
View(cityfam)

#Rental Houses with the most popular House Properties for Bachelor

famousfam <- cityfam
View(famousfam)

#FAMOUS PROPERTIES
# Rent = 18349 < Rent < 39114
# Size = 952 < Size < 1744
# Area Type = Carpet Area
# Furnishing Status = Semi Furnished
# BHK = 3
# Bathrooms = 3
# City = Hyderabad

#9.3 What is the most average housing rentals for Family/Bachelors?
#(This is to get what an average housing criteria values(control values) are for Family/Bachelors)

#Analysis 9.3.1: How many Housing Rentals prefer Bachelors/Family as tenants? 
View(data)
fambachcount <- ggplot(data,aes(x=Tenant.Preferred, fill =..count..),stat = "count") + 
  geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")
fambachcount #3444

#Analysis 9.3.2: Find average Rent for Bachelors/Family Rental Housing 
fambachdata <- filter(rdata,Tenant.Preferred == "Bachelors/Family")
mean(fambachdata$Rent) #20990.76

#Analysis 9.3.3: Find how many Bachelors/Family Rental Housing falls behind the Mean Rent and above it 
fambachrentsd <- signif(sd(fambachdata$Rent),7) #18440.2
highfambachrent <- fambachdata$Rent > 20990.76
fambachrenttable <- table(highfambachrent)
truefalsedata <- c(2273,1168)
fambachrentpiepercent <- round(100*truefalsedata/sum(truefalsedata), 1)
pie(fambachrenttable,labels = fambachrentpiepercent, 
    main = "Pie chart for Rent > Mean Data",col = rainbow(length(fambachrenttable)))
legend("topright",c("False","True"),cex = 0.8,fill = rainbow(length(fambachrenttable))) 

#Analysis 9.3.4: Find Bachelors/Family housing rent range and Bachelors/Family housing rentals that fall into this range
rentfambachrent <- filter(fambachdata,Rent < (mean(fambachdata$Rent) + 0.5*fambachrentsd) & 
                            Rent > (mean(fambachdata$Rent) - 0.5*fambachrentsd) )  
View(rentfambachrent)

#Analysis 9.3.5: Find average Size for Bachelors/Family Rental Housing 
max(fambachdata$Size)
fambachsizesd <- signif(sd(rentfambachrent$Size),3) #611
signif(mean(rentfambachrent$Size),4) #1069
highfambachsize <- rentfambachrent$Size > 1069
fambachsizetable <- table(highfambachsize)
fambachsizetable
truefalsedata <- c(928,724)
famsizebachpiepercent <- round(100*truefalsedata/sum(truefalsedata), 1)
pie(fambachsizetable,labels = famsizebachpiepercent, main = "Pie chart for Size > Mean Data",
    col = rainbow(length(fambachsizetable)))
legend("topright",c("False","True"),cex = 0.8,fill = rainbow(length(famsizebachpiepercent)))

#Analysis 9.3.6: Find Bachelors/Family housing sizes range and Bachelors/Family preferred rental housings that fall into this range
fambachsizesd <- signif(sd(rentfambachrent$Size),3)
sizefambach <- filter(rentfambachrent, Size < (mean(rentfambachrent$Size) + 0.5*fambachsizesd) & 
                          Size > (mean(rentfambachrent$Size) - (0.5*fambachsizesd)))
View(sizefambach)

#Analysis 9.3.7: Find Bachelors/Family Housing Rentals most preferred Area Type
area_type_fambachelors <- sizefambach$Area.Type
df <- data.frame(area_type_fambachelors)
areatypefambach_bar <- ggplot(df,aes(area_type_fambachelors,fill =..count..), stat = ..count..) +
  geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")
areatypefambach_bar #Super Area

areatypefambach <- filter(sizefambach, Area.Type == "Super Area")
View(areatypefambach)

#Analysis 9.3.8: Find Bachelors/FamilyHousing Rentals most preferred Furnishing Status
furnishing_status_fambachelors <- areatypefambach$Furnishing.Status
df <- data.frame(furnishing_status_fambachelors)
furnishingstatusfambach_bar <- ggplot(df,aes(furnishing_status_fambachelors, fill =..count..), stat = "count") + 
  geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")
furnishingstatusfambach_bar #Semi Furnished
furnishingstatusfambach <- filter(areatypefambach, Furnishing.Status == "Semi-Furnished")
View(furnishingstatusfambach)

#Analysis 9.3.9: Find Bachelors/FamilyHousing Rentals most preferred BHK
bhkfamilybach <- furnishingstatusfambach$BHK
df <-data.frame(bhkfamilybach)
bhkfambach_bar <- ggplot(df,aes(bhkfamilybach, fill =..count..), stat = "count") + 
  geom_bar(width = 0.5) + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")
bhkfambach_bar #2
bhkfambach <- filter(furnishingstatusfambach, BHK == 2)
View(bhkfambach)

#Analysis 9.3.10: Find Bachelors/Family Housing Rentals most preferred Bathroom Numbers

bathroomfamilybach <- bhkfambach$Bathroom
df <- data.frame(bathroomfamilybach)
bathroom_bar <-  ggplot(df,aes(bathroomfamilybach, fill =..count..), stat = "count") + 
  geom_bar(width = 0.5) + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white") + 
  labs(title='Family Bathroom Count', x='Bathroom', y='Count')
bathroom_bar
bathroomfambach <- filter(bhkfambach, Bathroom == 2)
View(bathroomfambach)

#Analysis 9.3.11: Find Bachelors/Family preferred Housing Rentals most resided in City

cityfamilybach <- bathroomfambach$City
df <- data.frame(cityfamilybach)
city_bar <-  ggplot(df,aes(cityfamilybach, fill =..count..), stat = "count") + 
  geom_bar(width = 0.5) + geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")
city_bar
cityfambach <- filter(bathroomfambach, City == "Chennai")
View(cityfambach)

#Rental Houses with the most popular House Properties for Bachelor

famousfambach <- cityfambach
  View(famousfambach)

#FAMOUS PROPERTIES
# Rent = 11771 < Rent < 30211
# Size = 763 < Size < 1374
# Area Type = Super Area
# Furnishing Status = Semi Furnished
# BHK = 2
# Bathrooms = 2
# City = Chennai


#10 How much rent changes for each feature wanted in a house

#Analysis 10.1: Find average Rent that is paid to be in each city
ggplot(rdata,aes(x=City, y=Rent, fill = City)) +  geom_boxplot()
avgrentcity <- rdata%>%group_by(City)%>%summarise(avgrent = mean(Rent))%>%arrange(desc(avgrent))
#Extra Feature: summarise() - to summarize the data frame into just one value or vector
avgrentcity
ggplot(avgrentcity,aes(x=City, y=avgrent, fill = avgrent)) +  geom_bar(stat='Identity')+
  labs(title='Average Rent by City', x='City', y='Average Rent')+
  geom_text(stat='identity', aes(label=signif(avgrent)), vjust=2, color = "white")

#Analysis 10.2: Find average Rent that is paid to be in each area type wanted
avgrentarea <- rdata%>%group_by(Area.Type)%>%summarise(avgrent = mean(Rent))%>%arrange(desc(avgrent))
avgrentarea
ggplot(avgrentarea,aes(x=Area.Type, y=avgrent, fill = avgrent)) +  geom_bar(stat='Identity')+ 
  labs(title='Average Rent by Area', x='Area', y='Average Rent')+
  geom_text(stat='identity', aes(label=signif(avgrent)), vjust=2, color = "white")


#Analysis 10.3: Find average Rent that is paid to be in each BHK  wanted
avgrentbhk <- rdata%>%group_by(BHK)%>%summarise(avgrent = mean(Rent))%>%arrange(desc(avgrent))
avgrentbhk 
ggplot(avgrentbhk,aes(x=BHK, y=avgrent, fill = avgrent)) +  geom_bar(stat='Identity')+ 
  labs(title='Average Rent by Area', x='BHK', y='Average Rent')+
  geom_text(stat='identity', aes(label=signif(avgrent)), vjust=2, color = "white")

#Analysis 10.4: Find average Rent that is paid to for every Bathroom wanted
avgrentbath <- data%>%group_by(Bathroom)%>%summarise(avgrent = mean(Rent))%>%arrange(desc(avgrent))
avgrentbath
ggplot(avgrentbath,aes(x=Bathroom, y=avgrent, fill = avgrent)) +  geom_bar(stat='Identity')+ 
  labs(title='Average Rent by Area', x='Bathroom', y='Average Rent')+
  geom_text(stat='identity', aes(label=signif(avgrent)), vjust=2, color = "white")


#11 What is more cost effective in housing Rentals when comparing contacting the owner, an agent or a builder?

#Analysis 11.1: Find how much does rent defer when contacting owner, agent or builder
ggplot(rdata,aes(x=Point.of.Contact, y=Rent,fill = Point.of.Contact)) +  geom_boxplot()
avgrentpoc <- rdata %>% group_by(Point.of.Contact) %>%
  summarise(Mean_Rent = mean(Rent),
            .groups = 'drop')
avgrentpoc

#Analysis 11.2: Find how much does rent defer to Size when contacting owner, agent or builder

ggplot(rdata, aes(x=Rent, y=Size)) + geom_point(aes(color = factor(Point.of.Contact))) + facet_wrap(~Point.of.Contact)
avgsizepoc <- rdata %>% group_by(Point.of.Contact) %>%
  summarise(Mean_Size = mean(Size),Mean_Rent = mean(Rent),
            .groups = 'drop')
avgsizepoc

#Analysis 11.3: Find how much does rent defer with BHK when contacting owner, agent or builder     
p1 <- ggplot(rdata, aes(x=BHK, y=Rent, fill=Point.of.Contact)) + 
  geom_boxplot() +
  facet_wrap(~BHK, scale="free")

avgbhkpoc <- rdata %>% group_by(Point.of.Contact) %>%
  summarise(Mean_BHK = signif(mean(BHK), digits = 1),Mean_Rent = mean(Rent),
            .groups = 'drop')
avgbhkpoc

#Analysis 11.4: Find how much does rent defer with Bathroom number when contacting owner, agent or builder
p2 <- ggplot(rdata, aes(x=Bathroom, y=Rent, fill=Point.of.Contact)) + 
  geom_violin() +
  facet_wrap(~Bathroom, scale="free")
avgbathpoc <- rdata %>% group_by(Point.of.Contact) %>%
  summarise(Mean_bath = signif(mean(Bathroom), digits = 1),Mean_Rent = mean(Rent),
            .groups = 'drop')
avgbathpoc

#Analysis 11.5: Find how much does rent defer with Bathroom number when contacting owner, agent or builder
ggplot(rdata, aes(x=Furnishing.Status, y=Rent, fill=Point.of.Contact)) + 
  geom_boxplot() +
  facet_wrap(~Furnishing.Status, scale="free")


#12 The best date for consumers to have a look for rental houses

#Analysis 12.1: Find the best month for having more options

ggplot(rdata,aes(x=Month, fill =..count..),stat = "count") + 
  geom_bar()+ geom_text(stat='count', aes(label=..count..), vjust=2, color = "white")

#Analysis 12.2: Find the best month according to rent
avgrentmon <- rdata %>% group_by(Month) %>%
  summarise(Mean_Rent = mean(Rent),
            .groups = 'drop')
ggplot(avgrentmon,aes(x=Month, y=Mean_Rent, fill=Month)) + geom_bar(stat="identity") +
  geom_text(stat='identity', aes(label=signif(Mean_Rent)), vjust=2, color = "white")

#Analysis 12.3: Find the best month to search for a big house
avgsizemon <- rdata %>% group_by(Month) %>%
  summarise(Mean_Size = mean(Size),
            .groups = 'drop')
ggplot(avgsizemon,aes(x=Month, y=Mean_Size, fill=Month)) + geom_bar(stat="identity") +
  geom_text(stat='identity', aes(label=signif(Mean_Size)), vjust=2, color = "white")
#Best month to look for a large house
sizesorted<- avgsizemon %>% arrange(desc(Mean_Size))
head(sizesorted) #July 1125
ggplot(avgsizemon,aes(x=Month, y=Mean_Size, fill = Mean_Size)) + geom_bar(stat='identity')+ geom_text(stat='identity', aes(label=signif(Mean_Size,digits=5)), vjust=2, color = "white")

# Analysis 12.4: Find the best month for each city tenant might be looking for
rdata%>%group_by(City)%>%summarise(modemonth = getmode(Month))%>%arrange(desc(modemonth))


#13 Is there an effect to having different level of floors to other factors of Rentals?
View(rdata)

#Analysis 13.1: Which Floors have the highest amount of frequency in the dataset?
detach(package:dplyr,unload=TRUE)
library(plyr)
View(rdata)
unique(rdata$FirstFloor)
countff<-count(rdata,'FirstFloor')
library(dplyr)
sortedcountff<- countff  %>% arrange(desc(freq))
topff <- head(sortedcountff) #1 , 2 , 0 , 3, 4 ,5
ggplot(topff, aes(x=FirstFloor,y=freq,fill=freq))+geom_bar(stat = "identity", color="black")+ 
  geom_text( aes(label=freq), vjust=2, color = "white")

#Analysis 13.2: Find if Floors affect the Rent of the Rental House
#Highest
highavgrentfloor <- rdata%>%group_by(FirstFloor)%>%summarise(avgrent = mean(Rent))%>%arrange(desc(avgrent))
onlyhighavg <- head(highavgrentfloor,n=10) #Highest
onlyhighavg #29, 41, 49, 48
ggplot(onlyhighavg,aes(x=FirstFloor,y=avgrent))+ geom_bar(stat="identity",aes(fill=avgrent))+ 
  labs(title='Highest AVG rent for every floor', x='Floor', y='Average Rent')+
  geom_text(aes(label=FirstFloor, vjust=2, color = "black"))

#Lowest
lowavgrentfloor <- rdata%>%group_by(FirstFloor)%>%summarise(avgrent = mean(Rent))%>%arrange(desc(-avgrent))
onlylowavg <- head(lowavgrentfloor,n=10) #lowest
onlylowavg #0, 1, 2, -2, 3
ggplot(onlylowavg,aes(x=FirstFloor,y=avgrent))+ geom_bar(stat="identity",aes(fill=avgrent))+ 
  labs(title='Lowest AVG rent for every floor', x='Floor', y='Average Rent')+
  geom_text(aes(label=FirstFloor, vjust=2, color = "black"))

#Total Floor to rent
#Highest
highavgrenttfloor <- rdata%>%group_by(TotalFloor)%>%summarise(avgrent = mean(Rent))%>%arrange(desc(avgrent))
onlyhighavg <- head(highavgrenttfloor,n=10) #Highest
onlyhighavg #71, 68, 54, 44
ggplot(onlyhighavg,aes(x=TotalFloor,y=avgrent))+ geom_bar(stat="identity",aes(fill=avgrent))+ 
  labs(title='Highest AVG rent for every Total floor', x='Total Floor', y='Average Rent')+
  geom_text(aes(label=TotalFloor, vjust=2, color = "black"))

#Lowest
lowavgrenttfloor <- rdata%>%group_by(TotalFloor)%>%summarise(avgrent = mean(Rent))%>%arrange(desc(-avgrent))
onlylowavg <- head(lowavgrenttfloor,n=10) #lowest
onlylowavg #2, 1, 3, 4, 5
ggplot(onlylowavg,aes(x=TotalFloor,y=avgrent))+ geom_bar(stat="identity",aes(fill=avgrent))+ 
  labs(title='Lowest AVG rent for every Total floor', x='Total Floor', y='Average Rent')+
  geom_text(aes(label=TotalFloor, vjust=2, color = "black"))


#Analysis 13.3: Find if Floors affect the Sizes of the Rental House
#Highest
highavgsizefloor <- rdata%>%group_by(FirstFloor)%>%summarise(avgsize = mean(Size))%>%arrange(desc(avgsize))
onlyhighavg <- head(highavgsizefloor,n=10) #Highest
onlyhighavg #39, 50, 24, 47
ggplot(onlyhighavg,aes(x=FirstFloor,y=avgsize))+ geom_bar(stat="identity",aes(fill=avgsize))+
  labs(title='Highest AVG Size for every floor', x='Floor', y='Average Size')+
  geom_text(aes(label=FirstFloor, vjust=2, color = "black"))

#Lowest
lowavgsizefloor <- rdata%>%group_by(FirstFloor)%>%summarise(avgsize = mean(Size))%>%arrange(desc(-avgsize))
onlylowavg <- head(lowavgsizefloor,n=10) #lowest
onlylowavg #-1, 0, 2, 1, 3
ggplot(onlylowavg,aes(x=FirstFloor,y=avgsize))+ geom_bar(stat="identity",aes(fill=avgsize))+ 
  labs(title='Lowest AVG Size for every floor', x='Floor', y='Average Size')+
  geom_text(aes(label=FirstFloor, vjust=2, color = "black"))

#Total Floor to Size
#Highest
highavgsizetfloor <- rdata%>%group_by(TotalFloor)%>%summarise(avgsize = mean(Size))%>%arrange(desc(avgsize))
onlyhighavg <- head(highavgsizetfloor,n=10) #Highest
onlyhighavg #81, 89, 60, 75
ggplot(onlyhighavg,aes(x=TotalFloor,y=avgsize))+ geom_bar(stat="identity",aes(fill=avgsize))+ 
  labs(title='Highest AVG Size for every Total floor', x='Total Floor', y='Average Size')+
  geom_text(aes(label=TotalFloor, vjust=2, color = "black"))

#Lowest
lowavgsizetfloor <- rdata%>%group_by(TotalFloor)%>%summarise(avgsize = mean(Size))%>%arrange(desc(-avgsize))
onlylowavg <- head(lowavgsizetfloor,n=10) #lowest
onlylowavg #2, 3, 7, 8
ggplot(onlylowavg,aes(x=TotalFloor,y=avgsize))+ geom_bar(stat="identity",aes(fill=avgsize))+ 
  labs(title='Lowest AVG Size for every Total floor', x='Total Floor', y='Average Size')+
  geom_text(aes(label=TotalFloor, vjust=2, color = "black"))


# Analysis 13.4: Find if Floors affect the Bathrooms of the Rental House


avgbathffloor<- rdata%>%group_by(Bathroom)%>%summarise(avgfloor = mean(FirstFloor))%>%arrange(desc(-avgfloor))
ggplot(avgbathffloor,aes(x=Bathroom,y=avgfloor))+ geom_bar(stat="identity",aes(fill=avgfloor)) + 
  labs(title='AVG First Floor for each Bathroom', x='Bathroom', y='First Floor')+ 
geom_text( aes(label=signif(avgfloor,2)), vjust=2, color = "white")


# Total Floor with Bathroom 

avgbathtfloor<- rdata%>%group_by(Bathroom)%>%summarise(avgtfloor = mean(TotalFloor))%>%arrange(desc(-avgtfloor))
ggplot(avgbathtfloor,aes(x=Bathroom,y=avgtfloor))+ geom_bar(stat="identity",aes(fill=avgtfloor)) + 
  labs(title='AVG Total Floor for each Bathroom', x='Bathroom', y='Total Floor')+ 
  geom_text( aes(label=signif(avgtfloor,2)), vjust=2, color = "white")


# Analysis 13.5: Find if Floors affect the BHK of the Rental House

avgbhkffloor<- rdata%>%group_by(BHK)%>%summarise(avgfloor = mean(FirstFloor))%>%arrange(desc(-avgfloor))
ggplot(avgbhkffloor,aes(x=BHK,y=avgfloor))+ geom_bar(stat="identity",aes(fill=avgfloor)) + 
  labs(title='AVG First Floor for each BHK', x='BHK', y='First Floor')+ 
  geom_text( aes(label=signif(avgfloor,2)), vjust=2, color = "white")

# Total Floor with BHK
avgbhktfloor<- rdata%>%group_by(BHK)%>%summarise(avgtfloor = mean(TotalFloor))%>%arrange(desc(-avgtfloor))
ggplot(avgbhktfloor,aes(x=BHK,y=avgtfloor))+ geom_bar(stat="identity",aes(fill=avgtfloor)) + 
  labs(title='AVG Total Floor for each BHK', x='BHK', y='Total Floor')+ 
  geom_text( aes(label=signif(avgtfloor,2)), vjust=2, color = "white")


# Analysis 13.6: Find if Floors affect the Area Type of the Rental House
avgareafloor <- rdata%>%group_by(Area.Type)%>%summarise(avgfloor = mean(FirstFloor))%>%arrange(desc(-avgfloor))
ggplot(avgareafloor,aes(x=Area.Type,y=avgfloor))+ geom_bar(stat="identity",aes(fill=avgfloor)) + 
  labs(title='AVG First Floor for Area Type', x='Area Type', y='First Floor')+
   geom_text( aes(label=signif(avgfloor,1)), vjust=2, color = "white")

# Area Type with Total Floor
avgareatfloor <- rdata%>%group_by(Area.Type)%>%summarise(avgtfloor = mean(TotalFloor))%>%arrange(desc(-avgtfloor))
ggplot(avgareatfloor,aes(x=Area.Type,y=avgtfloor))+ geom_bar(stat="identity",aes(fill=avgtfloor)) +
  labs(title='AVG Total Floor for Area Type', x='Area Type', y='Total Floor')+ 
  geom_text( aes(label=signif(avgtfloor,2)), vjust=2, color = "white")

# Analysis 13.7: Find if Floors affect the Furnishing Status  of the Rental House
avgfsfloor <- rdata%>%group_by(Furnishing.Status)%>%summarise(avgfloor = mean(FirstFloor))%>%arrange(desc(-avgfloor))
ggplot(avgfsfloor,aes(x=Furnishing.Status,y=avgfloor))+ geom_bar(stat="identity",aes(fill=avgfloor)) + 
  labs(title='AVG First Floor for Furnishing Status', x='Furnishing Status', y='First Floor')+ 
  geom_text( aes(label=signif(avgfloor,2)), vjust=2, color = "white")

# Furnishing Status with Total Floor
avgfstfloor <- rdata%>%group_by(Furnishing.Status)%>%summarise(avgtfloor = mean(TotalFloor))%>%arrange(desc(-avgtfloor))
ggplot(avgfstfloor,aes(x=Furnishing.Status,y=avgtfloor))+ geom_bar(stat="identity",aes(fill=avgtfloor)) + 
  labs(title='AVG Total Floor for Furnishing Status', x='Furnishing Status', y='Total Floor')+ 
  geom_text( aes(label=signif(avgtfloor,2)), vjust=2, color = "white")

# Analysis 13.8: Find if Floors affect the Point of Contact of the Rental House
avgpocfloor <- rdata%>%group_by(Point.of.Contact)%>%summarise(avgfloor = mean(FirstFloor))%>%arrange(desc(-avgfloor))
ggplot(avgpocfloor,aes(x=Point.of.Contact,y=avgfloor))+ geom_bar(stat="identity",aes(fill=avgfloor)) + 
  labs(title='AVG First Floor for Point of Contact', x='Point of Contact', y='First Floor')+ 
  geom_text( aes(label=signif(avgfloor,2)), vjust=2, color = "white")
# Point of Contact with Total Floor
avgpoctfloor <- rdata%>%group_by(Point.of.Contact)%>%summarise(avgtfloor = mean(TotalFloor))%>%arrange(desc(-avgtfloor))
ggplot(avgpoctfloor,aes(x=Point.of.Contact,y=avgtfloor))+ geom_bar(stat="identity",aes(fill=avgtfloor)) + 
  labs(title='AVG Total Floor for Point of Contacts', x='Point of Contact', y='Total Floor')+ 
  geom_text( aes(label=signif(avgtfloor,2)), vjust=2, color = "white")




