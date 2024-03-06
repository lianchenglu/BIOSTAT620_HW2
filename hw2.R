setwd("/home/chenggg/BIOSTAT620/BIOSTAT620_HW2")
rm(list = ls())
gc()
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(systemfit)
df <- read_excel("ScreenTime_chenggg.xlsx")
df <- df[c(1:13), ]
df$Pickup.1st_EST <- format(as.POSIXct(df$Pickup.1st_PST, format = "%H:%M", tz = "America/Los_Angeles"), "%H:%M", tz = "America/New_York")
df <- df %>% select(-"Pickup.1st_PST")
convert_to_minutes <- function(time) {
  if (!grepl("h", time)) {
    return(as.numeric(sub("m", "", time)))
  }
  if (!grepl("m", time)) {
    return(60*as.numeric(sub("h", "", time)))
  }
  parts <- strsplit(time, "h|m")[[1]]
  as.numeric(parts[1]) * 60 + as.numeric(parts[2])
}

df$Total.ST.min <- sapply(df$Total.ST, convert_to_minutes)
df$Social.ST.min <- sapply(df$Social.ST, convert_to_minutes)
df$is_weekday <- ifelse(wday(df$Date) %in% 2:6, 1, 0)
df$IsBeforeJan17 <- ifelse(df$Date < as.Date('2024-01-17'), 1, 0)

df$Total.ST.min_lag <- c(NA, acf(df[[3]], plot = F)$acf) # total ST
df$Social.ST.min_lag <- c(NA, acf(df[[5]], plot = F)$acf) # total social ST

eq1 <- Total.ST.min ~ Total.ST.min_lag + is_weekday + IsBeforeJan17
eq2 <- Social.ST.min ~ Social.ST.min_lag + is_weekday + IsBeforeJan17

fit <- systemfit(list(eq1=eq1, eq2=eq2), data=df, method="SUR")
summary(fit)
