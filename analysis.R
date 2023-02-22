library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

# Load in the data
spl_df <- read.csv("/Users/aaliyahviloria/Desktop/INFO201/a3-spl-checkouts-aviloria13/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)


#### SUMMARY INFORMATION ####
# Create list for summary information
sum_info <- list()

# Calculate total number of checkouts in 2022
spl_df_2022 <- spl_df %>% filter(CheckoutYear == "2022")
sum_info$earliest_date <- spl_df %>% filter(date == min(date)) %>% distinct(date) %>% pull(date)
sum_info$latest_date <- spl_df %>% filter(date == max(date)) %>% distinct(date) %>% pull(date)
sum_info$total_checkouts <- spl_df %>% summarize(sum = sum(Checkouts)) %>% pull(sum)
sum_info$total_checkouts_2022 <- spl_df_2022 %>% summarize(sum = sum(Checkouts)) %>% pull(sum)

# Filter for Dr.Seuss books
Seuss_df <- spl_df %>% filter(str_detect(Creator, "Seuss, Dr.") & CheckoutYear == "2022")

# Calculate total number of checkouts for Dr.Seuss' books in 2022
sum_info$Seuss_total <- Seuss_df %>% summarize(total = sum(Checkouts)) %>% pull(total)

# Calculate the average number of checkouts for Dr.Seuss' books, by month
Seuss_sum <- Seuss_df %>% group_by(CheckoutMonth) %>% summarize(sum = sum(Checkouts, na.rm = TRUE))
Seuss_avg <- Seuss_sum %>% summarize(avg = mean(sum, na.rm = TRUE)) %>% pull(avg)
sum_info$Seuss_avg <- round(Seuss_avg)

# Calculate the month with the least checkouts (and number of checkouts) for Dr.Seuss' books
Seuss_least <- Seuss_sum %>% filter(sum == min(sum))
sum_info$Seuss_least_month <- Seuss_least %>% pull(CheckoutMonth)
sum_info$Seuss_least_books <- Seuss_least %>% pull(sum)

# Calculate the month with the most checkouts (and number of checkouts) for Dr.Seuss' books
Seuss_most <- Seuss_sum %>% filter(sum == max(sum))
sum_info$Seuss_most_month <- Seuss_most %>% pull(CheckoutMonth)
sum_info$Seuss_most_books <- Seuss_most %>% pull(sum)

# Filter for Stephen King books
Stephen_df <- spl_df %>% filter(str_detect(Creator, "Stephen King") & CheckoutYear == "2022")

# Calculate total number of checkouts for Stephen King's books in 2022
sum_info$Stephen_total <- Stephen_df %>% summarize(total = sum(Checkouts)) %>% pull(total)

# Calculate the average number of checkouts for Stephen King's books, by month
Stephen_sum <- Stephen_df %>% group_by(CheckoutMonth) %>% summarize(sum = sum(Checkouts, na.rm = TRUE))
Stephen_avg <- Stephen_sum %>% summarize(avg = mean(sum, na.rm = TRUE)) %>% pull(avg)
sum_info$Stephen_avg <- round(Stephen_avg)

# Calculate the month with the least checkouts (and number of checkouts) for Stephen King's books
Stephen_least <- Stephen_sum %>% filter(sum == min(sum))
sum_info$Stephen_least_month <- Stephen_least %>% pull(CheckoutMonth)
sum_info$Stephen_least_books <- Stephen_least %>% pull(sum)

# Calculate the month with the most checkouts (and number of checkouts) for Stephen King's books
Stephen_most <- Stephen_sum %>% filter(sum == max(sum))
sum_info$Stephen_most_month <- Stephen_most %>% pull(CheckoutMonth)
sum_info$Stephen_most_books <- Stephen_most %>% pull(sum)


#### CHART 1 ####
Seuss_sum <- Seuss_sum %>% mutate(avg = rep(c(sum_info$Seuss_avg), each=12))
Seuss_chart <- ggplot(Seuss_sum) + 
  geom_point(mapping = aes(x = CheckoutMonth, y = sum, color = "Sum Checkouts")) + 
  geom_line(mapping = aes(x = CheckoutMonth, y = sum, color = "Sum Checkouts")) +
  geom_point(mapping = aes(x = CheckoutMonth, y = avg, color = "Average Checkouts")) + 
  geom_line(mapping = aes(x = CheckoutMonth, y = avg, color = "Average Checkouts")) +
  labs(title = "Dr.Seuss Book Checkouts per Month in 2022", x = "Month", y = "Number of Checkouts", color = "Type of Checkouts") +
  scale_x_continuous(breaks = seq(1, 12, 1))


#### CHART 2 ####
Stephen_sum <- Stephen_sum %>% mutate(avg = rep(c(sum_info$Stephen_avg), each=12))
Stephen_chart <- ggplot(Stephen_sum) + 
  geom_point(mapping = aes(x = CheckoutMonth, y = sum, color = "Sum Checkouts")) + 
  geom_line(mapping = aes(x = CheckoutMonth, y = sum, color = "Sum Checkouts")) +
  geom_point(mapping = aes(x = CheckoutMonth, y = avg, color = "Average Checkouts")) + 
  geom_line(mapping = aes(x = CheckoutMonth, y = avg, color = "Average Checkouts")) +
  labs(title = "Stephen King Book Checkouts per Month in 2022", x = "Month", y = "Number of Checkouts", color = "Type of Checkouts") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(breaks = seq(1000, 1800, 100))


#### CHART 3 ####
author_combo <- left_join(Seuss_sum, Stephen_sum, by = "CheckoutMonth")
author_combo_chart <- ggplot(author_combo) + 
  geom_point(mapping = aes(x = CheckoutMonth, y = sum.x, color = "Dr.Seuss")) + 
  geom_line(mapping = aes(x = CheckoutMonth, y = sum.x, color = "Dr.Seuss")) +
  geom_point(mapping = aes(x = CheckoutMonth, y = sum.y, color = "Stephen King")) + 
  geom_line(mapping = aes(x = CheckoutMonth, y = sum.y, color = "Stephen King")) +
  labs(title = "Dr.Seuss vs. Stephen King Checkouts in 2022", x = "Month", y = "Number of Checkouts", color = "Author") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(breaks = seq(500, 1800, 100))

