#'---
#'title: "Kpop group BTS' songs and MV analysis"
#'author: "Rodrigo Santana Aguiar"
#'date: "October 26th, 2021"
#'---

# This project's goal is to treat, manipulate and analyze songs and MV's (music video) data from Korean boy-band BTS. 
# Ultimately, machine learning models will be used to predict some trends in data.
# Data were collected by the author using external sources, such as Youtube, Spotify, Wikipedia and Musicboard.
# The data dictionary is available on my GitHub directory:

# Set the working directory and loading the packages
setwd("C:/Users/Rodrigo/Desktop/BigDataAzure/MiniProjetos/Kpop")

# This line silences warnings throughout the script execution
options(warn=-1)

#
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(corrplot)
library(tidyr)

# Load and verify data
data <- read.csv("data.csv", sep = ";")
str(data)

#### Data cleaning and formatting ####
# * convert "release" and "mv_release_date" to date
# * convert "album", "language", "single" and "unit" to factor
# * convert "jn", "jk", "th", "nj", "jh", "sg", "jm" to numeric
# * delete the last column

# Converting the dates
df <- data                                                    # Creating a copy so that the original dataset wont change
df$release <- parse_date_time(data$release, "dmy")
df$mv_release_date <- parse_date_time(data$mv_release_date,"dmy")

# Converting categorical variables
columns <- c("album", "language", "single", "mv", "unit")
df[, columns] <- lapply(df[, columns], factor)

# Converting numerical values
members <- c("jn", "jk", "th", "nj", "jh", "sg", "jm")
df[, members] <- lapply(data[, members], as.double)

# Deleting the last column
df <- subset(df, select = -X)
str(df)
View(df)

#### Data transformation ####

df$total_sung <- rowSums(df[, members], na.rm = TRUE)         # Total singing time

new_columns <- paste(members, "perc", sep = "_") 
df[, new_columns] <- df[, members] / df$total_sung            # Singing time percentage

rap = c("nj", "jh", "sg")
vocal = c("jn", "jk", "th", "jm")
df$rap <- rowSums(df[, rap], na.rm = TRUE)                    # Rap line singing time
df$vocal <- rowSums(df[, vocal], na.rm = TRUE)                # Vocal line singing time

# Calculating ratios 
df$likes_dislikes <- df$likes/df$dislikes                     # Likes / dislikes
df$likes_views <- df$likes/df$views                           # Likes / MV's views
df$dislikes_views <- df$dislikes/df$views                     # Dislikes / MV's views
df$mv_song <- df$mv_duration / df$length                      # MV's duration / song duration

df <- df %>% 
  mutate(rap_vocal = case_when(vocal > 0 ~ rap/vocal,         # Rap line / vocal line singing time ratio
                               vocal == 0 & rap > 0 ~ 1))

# Creating a new dataset with each member singing 
# percentage for each song
# Needs optimization
a <- df %>% select(release, jn_perc)
names(a)[2] <- "perc"
a$member <- "jn"
b <- df %>% select(release, jk_perc)
b$member <- "jk"
names(b)[2] <- "perc"
c <- df %>% select(release, th_perc)
c$member <- "th"
names(c)[2] <- "perc"
d <- df %>% select(release, nj_perc)
d$member <- "nj"
names(d)[2] <- "perc"
e <- df %>% select(release, jh_perc)
e$member <- "jh"
names(e)[2] <- "perc"
f <- df %>% select(release, sg_perc)
f$member <- "sg"
names(f)[2] <- "perc"
g <- df %>% select(release, jm_perc)
g$member <- "jm"
names(g)[2] <- "perc"

df_vocal <- bind_rows(a,b,c,g)                                # New vocal line df
df_rap <- bind_rows(d,e,f)                                    # New rap line df

#### Exploratory analysis ####

# Overall analysis
columns2 <- c("length", "language", "single", "mv",           # Some columns to take a look at
              "rating", "rap_vocal", "mv_song")
summary(df[columns2])

df %>% group_by(unit) %>%
  summarise(rating = mean(rating, na.rm = TRUE),
            like_dis = mean(likes_dislikes, na.rm = TRUE),
            song_dur = mean(length, na.rm = TRUE))

# Solo songs have higher scores
# Vocal line seems to please the audience more than rap line
# OT7 songs have the lowest rating -> most of the songs
# Rap line songs are longer than vocal line songs

# Correlations
columns <- c("length", "views", "dislikes", "likes", "mv_duration", 
             "rating", "total_sung", "jn", "jk", "th", "nj", "jh", "sg", "jm", 
             "mv_song", "likes_views", "likes_dislikes", "dislikes_views", 
             "rap_vocal")

df_cor <- df %>% select(columns) %>% drop_na()   #creates a new dataset with 
                                                 #numeric values
df_cor <- cor(df_cor)    #calculates the correlation matrix
corrplot(df_cor)         #displays the correlation matrix

# Insights: 
# 1. Vocal line singing time is related
# 2. The more the vocal line sings, the less the rap line does
# 3. Hits tend to have a lower rating
# 4. The more the vocal line sings, the more likes the mv receives
# 5. The longer the mv, the fewer people dislike it. The inverse is also true

# Ratings distribution
ggplot(data = df, aes(x = rating)) + geom_histogram() + ylab("Frequency") + 
  xlab("Musicboard rating") + ggtitle("BTS songs ratings")

# Rating distribution is not normal and certainly biased by fans

# Views
g1 <- ggplot(data = df, aes(x = mv_release_date, y = views)) + 
  geom_point() + ylab("Views") + xlab("Date") + geom_smooth(method = "lm") +
  ggtitle("MVs' views vs time") + theme_classic()

g2 <- ggplot(data = df, aes(x = mv_release_date, y = likes_dislikes)) + 
  geom_point() + ylab("Likes / Dislikes ratio") + geom_smooth(method = "lm") +
  xlab("Date") + ggtitle("Likes-dislikes ratio vs time") + theme_classic()

g3 <- ggplot(data = df, aes(x = mv_release_date, y = likes_views)) + 
  geom_point() + ylab("Likes / Views ratio") + geom_smooth(method = "lm") +
  xlab("Date") + ggtitle("Likes-views ratio vs time") + theme_classic()

g4 <- ggplot(data = df, aes(x = mv_release_date, y = dislikes_views)) + 
  geom_point() + ylab("Dislikes / Views ratio") + geom_smooth(method = "lm") +
  xlab("Date") + ggtitle("Dislikes-views ratio vs time") + theme_classic()

grid.arrange(g1, g2, g3, g4, ncol = 2)

# relationships

g1 <- ggplot(data = df, aes(x = mv_release_date, y = rap_vocal)) + 
  geom_point() + xlab("Date") +
  ylab("Ratio") + ggtitle("Rap / vocal ratio (MVs) vs time") + ylim(0, 1.6) +
  theme_classic() + geom_smooth(method = "lm")

g2 <- df %>% group_by(album, release) %>% 
  summarise(rap_vocal = mean(rap_vocal, na.rm = TRUE)) %>%
  ggplot(aes(x = release, y = rap_vocal)) + geom_point() + xlab("Date") +
  ylab("Ratio") + ggtitle("Rap / vocal ratio (songs) vs time") + ylim(0, 1.6) +
  theme_classic() + geom_smooth(method = "lm")

grid.arrange(g1, g2, ncol = 2)

g1 <- ggplot(data = df, aes(x = mv_release_date, y = mv_duration)) + 
  geom_point() + xlab("Date") + ylab("Duration") + theme_classic() +
  ggtitle("MV duration vs time") + geom_smooth(method = "lm", se = FALSE)

g2 <- ggplot(data = df, aes(x = mv_release_date, y = mv_song)) + 
  geom_point() + xlab("Date") + ylab("Ratio") + ggtitle("MV-song ratio vs time") + 
  theme_classic() + geom_smooth(method = "lm", se = FALSE)

grid.arrange(g1, g2, ncol = 2)

g1 <- df_vocal %>% group_by(release, member) %>% summarise(perc = mean(perc, na.rm = TRUE)) %>%
  ggplot(aes(x = release, y = perc)) + xlab("Date") +
  ylab("Sung percentage") + geom_point(aes(color = member, shape = member)) + 
  ggtitle("Vocal line distribution vs time") + theme_classic() + 
  geom_smooth(method = "lm", se = FALSE, aes(color = member))

g2 <- df_rap %>% group_by(release, member) %>% summarise(perc = mean(perc, na.rm = TRUE)) %>%
  ggplot(aes(x = release, y = perc)) + xlab("Date") +
  ylab("Sung percentage") + geom_point(aes(color = member, shape = member)) + 
  ggtitle("Rap line distribution vs time") + theme_classic() +
  geom_smooth(method = "lm", se = FALSE, aes(color = member))

grid.arrange(g1, g2, ncol = 2)

# Language
df %>% group_by(language) %>%
  summarise(mean_view = mean(views, na.rm = TRUE), mean_rating = mean(rating, na.rm = TRUE),
            mean_likes_view = mean(likes_views, na.rm = TRUE))

# Songs in Korean have the highest rating
# MVs in English have a higher mean for views
# Songs in Japanese have a higher like-view ratio

# Album
df %>% group_by(album) %>% summarise(views = mean(views, na.rm = TRUE), rating = mean(rating, na.rm = TRUE),
                                     likes_view = mean(likes_views, na.rm = TRUE), 
                                     rap_vocal = mean(rap_vocal, na.rm = TRUE )) %>%
  arrange(desc(rap_vocal), .by_group = TRUE)

# The album TMBMIL pt 1 has the highest rap-vocal ratio, whilst Map of the soul 7 has the lowest

#### Regression analysis ####

# Goal: create a ML model to predict the 

