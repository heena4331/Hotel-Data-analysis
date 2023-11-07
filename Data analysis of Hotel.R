

library(tidyverse)
library(readxl)
hotel_data <- read_excel('C:\\Users\\Computer\\Downloads\\archive\\HotelFinalDataset.xlsx')

hotel_data 

glimpse(hotel_data)

view(hotel_data)

head(hotel_data)
view(clean_hotel_data)

#Cleaning data 

hotel_data%>%map(~sum(is.na(.)))

# Clean price column
# Remove currency symbol, commas, spaces, and double quotes
hotel_data$Price <- gsub("₹|,|\\s|\"", "", hotel_data$Price)


#remove non-breaking space characters (\u00A0) in the 'Price
hotel_data$Price <- gsub("\u00A0", "", hotel_data$Price)


# Convert to numeric
hotel_data$Price <- as.numeric(hotel_data$Price)
hotel_data



clean_hotel_data <- hotel_data%>%drop_na(Rating,ReviewsCount)

clean_hotel_data

##unique value
lapply(clean_hotel_data,unique)


#################################################################################

##EDA 
#Rating vs price

ggplot(clean_hotel_data,aes(x= Rating, Price,color = City))+
         geom_point(size =3)+ggtitle('Price vs Rating')


## add trendline in scatter plot

ggplot(clean_hotel_data, aes(x = Rating, y = Price)) +
  geom_point(size = 3, color = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, color = 'red') +
  ggtitle('Price vs Rating')


## mean of rating group by city
Rating_mean <- clean_hotel_data%>%group_by(City)%>%summarise(mean=mean(Rating))

Rating_mean



# average rating of hotel in each city
average_rating <- clean_hotel_data%>%group_by(City)%>%summarise(aveg_rating=mean(Rating))%>%
                   arrange(desc(aveg_rating))


average_rating

ggplot(average_rating, aes(x = reorder(City, -aveg_rating), y = aveg_rating, fill = aveg_rating)) +
  geom_bar(stat = 'identity') +
  ggtitle('Average Rating of Hotels in Each City')+theme(axis.text = element_text(angle = 90,
                                                          hjust = 1                        ))

top_Rating_cities <- head(average_rating,5)

top_Rating_cities

ggplot(top_Rating_cities, aes(x = reorder(City, -aveg_rating), y = aveg_rating, fill = aveg_rating)) +
  geom_bar(stat = 'identity') +
  ggtitle('Top 5 City highest rating')
                                       

lowest_rating <-     clean_hotel_data%>%group_by(City)%>%summarise(aveg_rating=mean(Rating))%>%
  arrange(aveg_rating)%>%slice(1:10)
lowest_rating

lowest_rating_cities <- head(lowest_rating,5)
lowest_rating_cities


ggplot(lowest_rating_cities, aes(x = reorder(City, -aveg_rating), y = aveg_rating, fill = aveg_rating)) +
  geom_bar(stat = 'identity') +
  ggtitle('5 lowest rating Cities ')

##count average price
average_price <- clean_hotel_data%>%group_by(City)%>%summarise(avg_price= mean(Price))
average_price

joined_table <- merge(average_rating,average_price,by = 'City')
joined_table


#average price by city graph

ggplot(joined_table, aes(x = reorder(City, -avg_price), y = avg_price, fill = aveg_rating)) +
  geom_bar(stat = 'identity', color = 'black', alpha = 0.8) +
  labs(
    x = "City",
    y = "Average Price",
    title = "Average Price by City",
    fill = "Average Rating"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )


##color change by rating 
library(ggplot2)

ggplot(joined_table, aes(x = reorder(City, -aveg_rating), y = avg_price, fill = aveg_rating)) +
  geom_bar(stat = 'identity', color = 'black', alpha = 0.8) +
  labs(
    x = "City",
    y = "Average Price",
    title = "Average Price by City",
    fill = "Average Rating"
  ) +
  scale_fill_gradient(low = "red",   high = "green") +  # Change color gradient
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

view(clean_hotel_data)

##average price and rating for all rooms
average_tp <- clean_hotel_data%>% group_by(Type)%>%summarise(price= mean(Price))
average_tp

average_tr <- clean_hotel_data%>% group_by(Type)%>%summarise(rating= mean(Rating))%>%slice(1:10)
average_tr

average_trp <- merge(average_tp, average_tr,by = 'Type')

##descendind order rating 
#top rated rooms with price
avg_trp_desc <- average_trp%>%arrange(desc(average_trp$rating))
avg_trp_desc

top_rooms <- head(avg_trp_desc,5)
top_rooms

ggplot(top_rooms,aes(x= Type , y= price,fill = rating))+
  geom_bar(stat = 'identity',color = 'black',alpha= 0.7)+
  labs(x= 'Type odf rooms',y= 'average price',
       title = 'Top 5 rooms',fill = 'average rating')+
  theme_minimal()


##acsending order rating
#low rated rooms with price

avg_trp_acs <- average_trp%>%arrange(average_trp$rating)
avg_trp_acs

low_romms <- head(avg_trp_acs,5)
low_romms

ggplot(low_romms,aes(x= Type , y= price , fill= rating))+
  geom_bar(stat = 'identity',color = 'black', alpha= 0.7)+
  labs(x = 'Type of Rooms', y= 'average price', title = '5 lowest rating rooms with price ')+
  theme_minimal()



##normalize data price column and rating column

## with new column 
##install caret
install.packages('caret')

library(caret)

# Suppose your data is stored in a data frame called 'clean_hotel_data'

# Define the names of the new columns
new_price_column <- 'Scaled_Price'
new_rating_column <- 'Scaled_Rating'

# Scale the 'Price' column and store the scaled values in the new column
clean_hotel_data[new_price_column] <- scale(clean_hotel_data$Price)

# Scale the 'Rating' column and store the scaled values in the new column
clean_hotel_data[new_rating_column] <- scale(clean_hotel_data$Rating)

view(clean_hotel_data)


library(caret)
#Select the columns you want to cluster on
cluster_data <- clean_hotel_data%>%select(Scaled_Price,Scaled_Rating)
cluster_data


# Choose the number of clusters (replace 'k' with your chosen value)
k <- 3

#Perform K-Means clustering
kmeans_result <- kmeans(cluster_data, centers = 3,nstart = 10  )


# Cluster assignments for each data point
cluster_assignments <- kmeans_result$cluster


# Cluster centers
cluster_centers <- kmeans_result$centers

#Create a data frame with cluster assignments
clustered_data <- data.frame(cluster = as.factor(cluster_assignments), cluster_data)

# Plot the clustered data
ggplot(clustered_data, aes(x = Scaled_Price, y = Scaled_Rating, color = cluster)) +
  geom_point() +
  ggtitle('K-Means Clustering') +
  labs(color = 'Cluster') +
  theme_minimal()









