# Quiz1: Exploring the data
setwd("C:/yelp_dataset")
list.files()
library(jsonlite)

business <- stream_in(file(list.files()[2]))
checkin <- stream_in(file(list.files()[3]))
review <- stream_in(file(list.files()[4]))
tip <- stream_in(file(list.files()[5]))
user <- stream_in(file(list.files()[6]))

## Q3. The number of lines of text in the reviews file  
nrow(review) #1,569,264

## Q4. The text of 100th review in the reviews file   
review[100,"text"] 

## Q5. The percentage of 5 stars
table(review$stars)/nrow(review) #36.9%

## Q6. The number of lines in the business file
nrow(business)

## Q7. The percentage of businesses having free wi-fi
table(business$attributes$`Wi-Fi`)/sum(table(business$attributes$`Wi-Fi`))

## Q8. The number of lines in tip file
nrow(tip) #495,107

##Q9. The 1,000th lein in the tip file
tip[1000,'user_id']

## Q10. THe nmae of the user with over 10,000 compliment votes of type "funny"
temp <- user[user$votes$funny >10^4, "name"]
c('Ira','Roger','Brian','Jeff') %in% temp

## Q11. 
temp <- data.frame(funny = user$votes$funny, fans = user$fans)
temp[temp$funny >= 1, "funny"] <- 1
temp[temp$fans >= 1, "fans"] <- 1
fisher.test(table(temp))


#Exploratory Modeling

#Try to predict how a user rate a business

#First of all, let's see whether a business's review is different according to the categories
#First of all, let's constrain the area of business which makes homogeneous

names(business)
names(user)
names(review)
names(tip)
names(checkin)

# The ultimate goal of this business is connecting customers to appropriate business
# Then let's classify the businesses and customers then connect to each others

# First, let's classify the businesses

# What attributes would be most differentiated in business?
# --> it seems categories and city, perhaps attributes
# --> then the output of stars, 

#Let's extract the features which relate to stars 
#hours, address, hours, open

# 1) Category classification
#: category would be important, but there are many categories.
#: and I'd like to reduce the number of categories and reamin one category for a business

# First, make a list of categories with frequency
cat_list <- data.frame(table(unlist(business$categories)))
cat_list <- cat_list[order(-cat_list$Freq),]
head(cat_list, 50)

# Then leave the most frequent category for each business
# But, before let's see whether the number of categories affects stars
num_cat <- sapply(business$categories, FUN = length)
cor(num_cat, business$stars) #0.0306 --> weak correlation

# Therefore we can reduce the number of categories into one

tail(cat_list,30)

cat_extract <- function(list, cat_list)
{
  
}

c <- cat_list[1,"Var1"] %in% unlist(business$categories) 

x <- business[cat_list[1,"Var1"] business$categories, ]

length(business$categories)

match(cat_list[1,'Var1'],business$categories)

length(unlist(business$categories))

class(business[5,"categories"])

cat_list[cat_list$Var == m[1],"Freq"]

x <- data.frame(sapply(business, class))

m <- unlist(x[x$sapply.business..class. == "numeric",])

m

table(business$open)

str(business$attributes)

x <- aggregate(stars~open, data = business, FUN = mean)

cor(business$latitude, business$stars)

names(business$attributes)

table(business$attributes$Music$dj)

table(business$attributes$`Accepts Insurance`)

apply(business$attributes, 2, class)

business[1:20,"categories"]

apply(business, 2, class)

#Is stars different from cities

length(table(business$city)) #There are 378 cities
city_list <- data.frame(table(business$city))
city_list <- city_list[order(-city_list$Freq),]

head(city_list)

temp <- aggregate(stars ~ city, data = business, FUN = mean)
temp2 <- aggregate(review_count ~ city, data = business, FUN = sum)
head(temp2)
hist(temp$stars)
sd(temp$stars)

plot(business$stars, business$review_count)

names(business)

head(business$hours)

nrow(temp)

temp2 <- aggregate(stars ~ categories, data = business, FUN = mean)
hist(temp2$stars)
names(business)
head(temp)



mean(cat_list$Freq)
hist(log(cat_list$Freq))

names(business)


mean(business$review_count)
nrow(business[business$review_count>20, ]) # there are 15761 businesses which have more than 20 reviews

length(cat_list) # There are 783 categories 

mean(user$fans)
max(user$fans)

names(user)
names(tip)
names(review)

nrow(user[user$review_count > 0, ]) #There are 366,708 users
nrow(user[user$review_count > 10, ]) #more than 10, 140,882 users

review[review$user_id == user[3,"user_id"], "stars"]

sd(review[review$user_id == user[3,"user_id"], "stars"])

sd(review[review$user_id == user[100,"user_id"], "stars"])

str(user$review_count)
str(user$user_id)
