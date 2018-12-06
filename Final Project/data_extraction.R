library("dplyr")
library("ggplot2")

movies <- read.csv(file = "movies.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

## selecting name, total grossing, opening week, and year
movies <- movies %>%
  select(year, movies_name, total_gross, opening_week, opening_date)

movies$year <- gsub("https://www.boxofficemojo.com/yearly/chart/\\?yr=", "", movies$year)
movies$year <- gsub("&p=.htm", "", movies$year)
movies$total_gross <- gsub("\\$", "", movies$total_gross)
movies$opening_week <- gsub("\\$", "", movies$opening_week)

library("lubridate")
movies$opening_date <- paste0(movies$opening_date,"-", movies$year)
movies$opening_date <- dmy(movies$opening_date)


movies <- movies %>%
          group_by(year) %>%
          arrange(year, opening_date)

for (row in 1 : nrow(movies)) {
  curr_row <- movies[row, ]
  if (is.na(curr_row$opening_date)) {
    movies <- movies[-c(row), ]
  }
}

movies <- movies[-c(99), ]

movies_2 <- movies %>%
            group_by(quarter = floor_date(opening_date, "3 months")) 

movies_2$total_gross <- movies_2$total_gross %>%
  lapply(function(y) gsub(",", "", y)) %>%
  as.numeric()

movies_2$opening_week <- movies_2$opening_week %>%
  lapply(function(y) gsub(",", "", y)) %>%
  as.numeric()


movies_2$quarter <- month(movies_2$quarter)
movies_2$quarter[movies_2$quarter == "4"] <- "2"
movies_2$quarter[movies_2$quarter == "7"] <- "3"
movies_2$quarter[movies_2$quarter == "10"] <- "4"

movies_2 <- movies_2 %>%
            group_by(year, quarter) %>%
            mutate(quarter_total = sum(total_gross)) %>%
            mutate(quarter_mean = mean(total_gross)) %>%
            mutate(quarter_median = median(total_gross)) 

movies_2 <- movies_2 %>%
            group_by(year) %>%
            mutate(quarter_over_year_gross = quarter_total / sum(total_gross))

movies_2 <- movies_2 %>%
            group_by(year, quarter) %>%
            mutate(quarter_variance = var(total_gross)) %>%
            mutate(quarter_st_dev = sd(total_gross))


            

quarter_data <- movies_2 %>%
                select(year, quarter, quarter_total, quarter_mean, quarter_median, quarter_over_year_gross, 
                       quarter_st_dev) %>%
                distinct() %>%
                mutate(num_of_movies = quarter_total / quarter_mean) %>%
                mutate(standard_error = quarter_st_dev / sqrt(num_of_movies)) %>%
                mutate(confidence_low_95 = quarter_mean - qnorm(0.95)*  standard_error) %>%
                mutate(confidence_high_95 = quarter_mean + qnorm(0.95)*  standard_error)

quarter_data <- quarter_data %>%
                group_by(quarter) %>%
                mutate(quarter_performance = mean(quarter_over_year_gross)) %>%
                mutate(quarter_over_year_sd = sd(quarter_over_year_gross)) 

year_data <- quarter_data %>%
             select(quarter_performance, quarter_over_year_sd) %>%
             distinct()
  

quarter_data_summarized <- quarter_data %>% 
  select(year, quarter, quarter_total) %>%
  group_by(year, quarter)

grossing_by_year <- data.frame(Year = 0, 
                               Winter = 0, 
                               Spring = 0, 
                               Summer = 0, 
                               Fall = 0) 


row <- 1
while (row < nrow(quarter_data_summarized)) {
  curr_row <- quarter_data_summarized[row, ]
  year_data <- as.numeric(curr_row$year)
  winter_data <- as.numeric(quarter_data_summarized[row , ]$quarter_total)
  spring_data <- as.numeric(quarter_data_summarized[row + 1, ]$quarter_total)
  summer_data <- as.numeric(quarter_data_summarized[row + 2, ]$quarter_total)
  fall_data <- as.numeric(quarter_data_summarized[row + 3, ]$quarter_total)
  new_row <- data.frame(Year = year_data, 
                        Winter = winter_data, 
                        Spring = spring_data, 
                        Summer = summer_data, 
                        Fall =fall_data)
  grossing_by_year <- rbind(grossing_by_year, new_row)
  row <- row + 4
}

grossing_by_year <- grossing_by_year[2:nrow(grossing_by_year), ]


quarter_data_summarized <- quarter_data_summarized %>%
  group_by(quarter) %>%
  mutate(Mean = mean(quarter_total)) %>%
  mutate(Standard_Deviation = sd(quarter_total)) %>%
  mutate(Variance = var(quarter_total)) %>%
  mutate(Standard_error = Standard_Deviation / sqrt(38)) %>%
  mutate(Confidence_Lower_Bound_95 = Mean - qnorm(0.95) * Standard_error) %>%
  mutate(Confidence_Higher_Bound_95 = Mean + qnorm(0.95) * Standard_error) %>%
  select(Mean, Standard_Deviation, Variance, Confidence_Lower_Bound_95, Confidence_Higher_Bound_95) %>%
  rename(Quarter = quarter) %>%
  unique()

quarter_data_summarized[quarter_data_summarized == 1] <- "Winter"
quarter_data_summarized[quarter_data_summarized == 2] <- "Spring"
quarter_data_summarized[quarter_data_summarized == 3] <- "Summer"
quarter_data_summarized[quarter_data_summarized == 4] <- "Fall"

write.csv(grossing_by_year, file="grossing_by_year.csv", row.names=FALSE)
write.csv(quarter_data_summarized, file="quarter_data_summarized.csv", row.names=FALSE)

for (i in 1 : 4) {
  data_name <- paste0("quarter_", i) 
  quarter <- quarter_data %>%
    filter(quarter == toString(i))
  write.csv(quarter, file=paste0(data_name,".csv"), row.names= FALSE)
}

write.csv(movies_2, file="all_year.csv", row.names=FALSE)
write.csv(quarter_data, file="quarter_data.csv", row.names=FALSE)

# this function writes a file into an output folder if existed. If it does not then it
# creates that folder in the current working directory
save_to_folder<- function(data_name, file_name, output_folder) {
  currwd <- getwd()
  if (file.exists(output_folder)) {
    setwd(output_folder)
  } else {
    dir.create(file.path(currwd, output_folder), showWarnings = FALSE)
  }
  write.csv(data_name, file = file_name, row.names = FALSE)
  setwd(currwd) 
}

year_to_csv <- function(year_name) {
  ##stripping Year away
  movies$opening_date <- gsub(paste0(year_name, "-"), "", movies$opening_date)
  year_data <- subset(movies, year == year_name)
  file_name <- paste0(year_name, "_movie_data.csv")
  save_to_folder(year_data, file_name, "output")
}

lapply(unique(movies$year), "year_to_csv")


plot_hist <- function(df, q) {
  quarter_data <- df %>%
    filter(quarter == q)
  map <- ggplot(quarter_data, aes(x = quarter_over_year_gross)) + 
    geom_histogram(binwidth = 0.01,
                   color="black", 
                   fill="white") +
    xlim(c(0 , 0.45)) + 
    ylim(c(0, 8))

  #ggsave(paste0("historam_map_quarter_", q, ".jpg"))
  return(map)
}

plot_hist_interleaved <- function(df) {
  map <- ggplot(df, aes(x = quarter_over_year_gross, color = quarter)) + 
    geom_histogram(binwidth = 0.01, 
                   fill = "white", 
                   position = "dodge") + 
    theme(legend.position = "top")
  return(map)
}

## Fall, Spring, Summer, Winter
color <- c("sienna1", "thistle1", "indianred1", "lightcyan1")
plot_box <- function(df) {
  map <- ggplot(df, aes(x = factor(quarter, levels = unique(quarter)), 
                        y = quarter_total,
                        fill = quarter)) +
    scale_fill_manual(values = color) + 
    geom_boxplot() + 
    labs(title = "Distribution of each quarter's total grossing",
         x = "Quarter",
         y = "Total Grossing (Dollars)") + 
    theme_bw()
}

## This functon calculates critical value given the correct numbers
## Parameters:
## mean : sample mean
## hypo_val : hypothesized value
## std_dev : population standard deviation 
## sample_size : sample size 
## alpha : significance level
## return the test statistic z value.
test_stat <- function(mean, hypo_val, std_dev, sample_size, alpha) {
  #test statistic
  z <- (mean - hypo_val)/(std_dev/sqrt(sample_size)) 
  return(z)
}
