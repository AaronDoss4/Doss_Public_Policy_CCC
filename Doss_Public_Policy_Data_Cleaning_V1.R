library(tidyverse)

data_source <- read_tsv("37070-0001-Data.tsv")

analysis_data <- data_source %>%
  select(DN1W2, DN6W2, DE6, SCHTREAT)

dim(analysis_data)
colSums(is.na(analysis_data))

missing_codes <- c(-55, -66, -77, -88, -95, -96, -97, -98, -99)

analysis_data$DN1W2[analysis_data$DN1W2 %in% missing_codes] <- NA

analysis_data$DN6W2[analysis_data$DN6W2 %in% missing_codes] <- NA

analysis_data$DE6[analysis_data$DE6 %in% missing_codes] <- NA

analysis_data$SCHTREAT[analysis_data$SCHTREAT %in% missing_codes] <- NA

dim(analysis_data)
colSums(is.na(analysis_data))
sapply(analysis_data, class)

sample_80pct <- round(0.80 * nrow(data_source))

analysis_sample <- analysis_data[1:sample_80pct, ]

dim(analysis_sample)
colSums(is.na(analysis_sample))

analysis_sample <- na.omit(analysis_sample)

dim(analysis_sample)
colSums(is.na(analysis_sample))

pdf("Observed_PeerConflict_Histogram.pdf", width = 8, height = 5.5)

ggplot(analysis_sample, aes(x = DN1W2)) +
  geom_histogram(
    binwidth = 1,
    boundary = -0.5,
    fill = "steelblue",
    color = "black",
    linewidth = 0.8
  ) +
  scale_x_continuous(
    breaks = 0:4,
    labels = c(
      "Never",
      "1–2 times/month",
      "About 1 time/week",
      "2–3 times/week",
      "Every day"
    )
  ) +
  labs(
    title = "How Often Students Observe Peer Conflict in Schools",
    subtitle = "Student Responses from Schools With and Without Intervention",
    x = "Frequency of seeing students being picked on",
    y = "Number of students"
  ) +
  theme_minimal()

dev.off()