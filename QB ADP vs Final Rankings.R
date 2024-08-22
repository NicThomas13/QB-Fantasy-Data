```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
# 2023
library(MASS)
library(caret)
library(glmnet)
library(bayesplot)
library(loo)
library(dplyr)
library(stringr)
QBADP2023 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\2023_QB_ADP_Rankings.csv")
QBFinal2023 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\Fantasy_Football_Statistics_QB_2023.csv")

str(QBFinal2023$YDS)
QBADP2023
QBFinal2023

FinalReason <- glm(formula = QBFinal2023$Rank ~ QBFinal2023$CMP + QBFinal2023$ATT + QBFinal2023$PCT + QBFinal2023$YDS + QBFinal2023$Y.A + QBFinal2023$TD + QBFinal2023$INT + QBFinal2023$SACKS + QBFinal2023$ATT.1 + QBFinal2023$YDS + QBFinal2023$TD + QBFinal2023$FL + QBFinal2023$G + QBFinal2023$FPTS + QBFinal2023$FPTS.G + QBFinal2023$ROST)
FinalReason
summary(FinalReason)
anova(FinalReason)

library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(gtExtras)
library(webshot2)

QBFinal2023 <- QBFinal2023 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

QBADP2023
QBFinal2023
FinalFix2023 <- QBFinal2023 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

# Ensure ADP and Final are numeric
QBADP2023 <- QBADP2023 %>%
  mutate(AVG = as.numeric(AVG))

FinalFix2023 <- FinalFix2023 %>%
  mutate(Rank = as.numeric(Rank))

# Add Games (G) to the Final dataset if it exists
if ("G" %in% colnames(FinalFix2023)) {
  FinalFix2023 <- FinalFix2023 %>%
    filter(G > 0)
}

# Only QBs with pass attempts are recorded
if ("ATT" %in% colnames(FinalFix2023)) {
  FinalFix2023 <- FinalFix2023 %>%
    filter (ATT > 0)
}

# Only QBs with pass attempts are recorded
if ("FPTS/G" %in% colnames(FinalFix2023)) {
  FinalFix2023 <- FinalFix2023 %>%
    filter (FPTS/G > 0)
}

# Only QBs with pass attempts are recorded
if ("ROST" %in% colnames(FinalFix2023)) {
  FinalFix2023 <- FinalFix2023 %>%
    filter (ROST > 3)
}

FinalFix2023
QBADP2023

# Select relevant columns
QBADP2023 <- QBADP2023 %>% dplyr::select(Player, QB, AVG)
FinalFix2023 <- FinalFix2023 %>% dplyr::select(Player, Rank)

# Merge the data on Player
QB_data2023 <- inner_join(QBADP2023, FinalFix2023, by = "Player")

QBADP2023
FinalFix2023
QB_data2023

# Calculate the difference
QB_data2023 <- QB_data2023 %>%
  mutate(Difference_2023 = AVG - Rank)

# Compute the variance of the differences
variance_diff2023 <- var(QB_data2023$Difference_2023)
variance_diff2023

# Print the summary table
QBsummary_table2023 <- QB_data2023 %>%
  dplyr::select(Player, AVG, Rank, Difference_2023)

print(QBsummary_table2023)

# # Output the variance
# print(paste("Variance of Differences:", variance_diff))

# Making gt table
QB_gt2023 <- QB_data2023 %>% 
  mutate(AVG = round(AVG, 1),
         Rank = round(Rank, 1),
         diff = AVG - Rank) |> 
  dplyr::select(Player, AVG, Rank, diff) |> 
  arrange(-diff) |> 
  gt() |>
  cols_align(align = "center") |> 
  cols_label(Player = "Quarterback",
             AVG = "Average Draft Position",
             Rank = "Final Rank",
             diff = "Difference_2023") |> 
  tab_header(
    title = md("**Quarterback ADP vs. Final Rank 2023**"),
    subtitle = md("**Comparing the Pre-Season and Post-Season Rankings**")
  ) %>%
  gtExtras::gt_theme_espn()

#save the gt table
gtsave(QB_gt2023, "qb_adp_vs_rank_2023.png")
summary(QB_data2023)

# Calculate mean of difference, create variance scale of distance from mean
QB_data2023

# Histogram Distribution 2023
ggplot(QB_data2023, aes(x = Difference_2023)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "QB ADP vs Finish 2023", x = "Difference", y = "Frequency")

# Boxplot Distribution 2023
ggplot(QB_data2023, aes(x = AVG, y = Rank)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "ADP vs Rank 2023", x = "AVG", y = "Rank")

# Scatterplot Distribution 2023
ggplot(QB_data2023, aes(x = AVG, y = Difference_2023)) +
  geom_point(color = "blue") +
  labs(title = "ADP vs Rank 2023", x = "AVG", y = "Rank")

# Histogram of the differences
ggplot(QB_data2023, aes(x = Difference_2023)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Differences Between ADP and Final Rank (2023)",
       x = "Difference (ADP - Final Rank)",
       y = "Frequency") +
  theme_minimal()


# Load the gridExtra package
library(gridExtra)

# Create the individual plots
scatter_plot <- ggplot(QB_data2023, aes(x = AVG, y = Rank)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatterplot of QB ADP vs Final Rank (2023)",
       x = "Average Draft Position (ADP)",
       y = "Final Rank") +
  theme_minimal()

hist_plot <- ggplot(QB_data2023, aes(x = Difference_2023)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Differences Between ADP and Final Rank (2023)",
       x = "Difference (ADP - Final Rank)",
       y = "Frequency") +
  theme_minimal()

# Scatterplot of ADP vs Final Rank with player names
ggplot(QB_data2023, aes(x = AVG, y = Rank)) +
  geom_text(aes(label = Player), color = "blue", size = 3, vjust = -0.5) +  # Adds player names as text
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Adds a linear trend line
  labs(title = "Scatterplot of QB ADP vs Final Rank (2023)",
       x = "Average Draft Position (ADP)",
       y = "Final Rank") +
  theme_minimal()
```

```{r}
#2022
library(MASS)
library(caret)
library(glmnet)
library(bayesplot)
library(loo)
QBADP2022 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\2022_QB_ADP_Rankings.csv")
QBFinal2022 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\Fantasy_Football_Statistics_QB_2022.csv")

str(QBFinal2022$YDS)
QBADP2022
QBFinal2022

FinalReason2022 <- glm(formula = QBFinal2022$Rank ~ QBFinal2022$CMP + QBFinal2022$ATT + QBFinal2022$PCT + QBFinal2022$YDS + QBFinal2022$Y.A + QBFinal2022$TD + QBFinal2022$INT + QBFinal2022$SACKS + QBFinal2022$ATT.1 + QBFinal2022$YDS + QBFinal2022$TD + QBFinal2022$FL + QBFinal2022$G + QBFinal2022$FPTS + QBFinal2022$FPTS.G + QBFinal2022$ROST)
FinalReason2022
summary(FinalReason2022)
anova(FinalReason2022)


QBFinal2022 <- QBFinal2022 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(gtExtras)
library(webshot2)
QBADP2022
QBFinal2022
FinalFix2022 <- QBFinal2022 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

# Ensure ADP and Final are numeric
QBADP2022 <- QBADP2022 %>%
  mutate(AVG = as.numeric(AVG))

FinalFix2022 <- FinalFix2022 %>%
  mutate(Rank = as.numeric(Rank))

# Add Games (G) to the Final dataset if it exists
if ("G" %in% colnames(FinalFix2022)) {
  FinalFix2022 <- FinalFix2022 %>%
    filter(G > 0)
}

# Only QBs with pass attempts are recorded
if ("ATT" %in% colnames(FinalFix2022)) {
  FinalFix2022 <- FinalFix2022 %>%
    filter (ATT > 0)
}

# Only QBs with pass attempts are recorded
if ("FPTS/G" %in% colnames(FinalFix2022)) {
  FinalFix2022 <- FinalFix2022 %>%
    filter (FPTS/G > 5)
}

QBADP2022 <- QBADP2022 %>% 
  filter(Player != "Taysom Hill")

FinalFix2022
QBADP2022

# Select relevant columns
QBADP2022 <- QBADP2022 %>% dplyr::select(Player, QB, AVG)
FinalFix2022 <- FinalFix2022 %>% dplyr::select(Player, Rank)

# Merge the data on Player
QB_data2022 <- inner_join(QBADP2022, FinalFix2022, by = "Player")

QBADP2022
FinalFix2022
QB_data2022

# Calculate the difference
QB_data2022 <- QB_data2022 %>%
  mutate(Difference_2022 = AVG - Rank)

# Compute the variance of the differences
variance_diff2022 <- var(QB_data2022$Difference_2022)
variance_diff2022

# Print the summary table
QBsummary_table2022 <- QB_data2022 %>%
  dplyr::select(Player, AVG, Rank, Difference_2022)

print(QBsummary_table2022)

# # Output the variance
# print(paste("Variance of Differences:", variance_diff))

# Making gt table
QB_gt2022 <- QB_data2022 %>% 
  mutate(AVG = round(AVG, 1),
         Rank = round(Rank, 1),
         diff = AVG - Rank) |> 
  dplyr::select(Player, AVG, Rank, diff) |> 
  arrange(-diff) |> 
  gt() |>
  cols_align(align = "center") |> 
  cols_label(Player = "Quarterback",
             AVG = "Average Draft Position",
             Rank = "Final Rank",
             diff = "Difference_2022") |> 
  tab_header(
    title = md("**Quarterback ADP vs. Final Rank 2022**"),
    subtitle = md("**Comparing the Pre-Season and Post-Season Rankings**")
  ) %>%
  gtExtras::gt_theme_espn()

#save the gt table
gtsave(QB_gt2022, "qb_adp_vs_rank_2022.png")
summary(QB_data2022)

# Calculate mean of difference, create variance scale of distance from mean
QB_data2022

# Histogram Distribution 2022
ggplot(QB_data2022, aes(x = Difference_2022)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "QB ADP vs Finish 2022", x = "Difference", y = "Frequency")

# Boxplot Distribution 2022
ggplot(QB_data2022, aes(x = AVG, y = Rank)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "ADP vs Rank 2022", x = "AVG", y = "Rank")

# Scatterplot Distribution 2022
ggplot(QB_data2022, aes(x = AVG, y = Rank)) +
  geom_point(color = "blue") +
  labs(title = "ADP vs Rank 2022", x = "AVG", y = "Rank")

# Scatterplot of ADP vs Final Rank with player names
ggplot(QB_data2022, aes(x = AVG, y = Rank)) +
  geom_text(aes(label = Player), color = "blue", size = 3, vjust = -0.5) +  # Adds player names as text
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Adds a linear trend line
  labs(title = "Scatterplot of QB ADP vs Final Rank (2022)",
       x = "Average Draft Position (ADP)",
       y = "Final Rank") +
  theme_minimal()
```

```{r}
# 2021
library(MASS)
library(caret)
library(glmnet)
library(bayesplot)
library(loo)
QBADP2021 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\2021_QB_ADP_Rankings.csv")
QBFinal2021 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\Fantasy_Football_Statistics_QB_2021.csv")

str(QBFinal2021$YDS)
QBADP2021
QBFinal2021

FinalReason2021 <- glm(formula = QBFinal2021$Rank ~ QBFinal2021$CMP + QBFinal2021$ATT + QBFinal2021$PCT + QBFinal2021$YDS + QBFinal2021$Y.A + QBFinal2021$TD + QBFinal2021$INT + QBFinal2021$SACKS + QBFinal2021$ATT.1 + QBFinal2021$YDS + QBFinal2021$TD + QBFinal2021$FL + QBFinal2021$G + QBFinal2021$FPTS + QBFinal2021$FPTS.G + QBFinal2021$ROST)
FinalReason2021
summary(FinalReason2021)
anova(FinalReason2021)


QBFinal2021 <- QBFinal2021 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(gtExtras)
library(webshot2)
QBADP2021
QBFinal2021
FinalFix2021 <- QBFinal2021 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

# Ensure ADP and Final are numeric
QBADP2021 <- QBADP2021 %>%
  mutate(AVG = as.numeric(AVG))

FinalFix2021 <- FinalFix2021 %>%
  mutate(Rank = as.numeric(Rank))

# Add Games (G) to the Final dataset if it exists
if ("G" %in% colnames(FinalFix2021)) {
  FinalFix2021 <- FinalFix2021 %>%
    filter(G > 0)
}

# Only QBs with pass attempts are recorded
if ("ATT" %in% colnames(FinalFix2021)) {
  FinalFix2021 <- FinalFix2021 %>%
    filter (ATT > 0)
}

# Only QBs with pass attempts are recorded
if ("FPTS/G" %in% colnames(FinalFix2021)) {
  FinalFix2021 <- FinalFix2021 %>%
    filter (FPTS/G > 5)
}

FinalFix2021
QBADP2021

# Select relevant columns
QBADP2021 <- QBADP2021 %>% dplyr::select(Player, QB, AVG)
FinalFix2021 <- FinalFix2021 %>% dplyr::select(Player, Rank)

# Merge the data on Player
QB_data2021 <- inner_join(QBADP2021, FinalFix2021, by = "Player")

QBADP2021
FinalFix2021
QB_data2021

# Calculate the difference
QB_data2021 <- QB_data2021 %>%
  mutate(Difference_2021 = AVG - Rank)

# Compute the variance of the differences
variance_diff2021 <- var(QB_data2021$Difference_2021)
variance_diff2021

# Print the summary table
QBsummary_table2021 <- QB_data2021 %>%
  dplyr::select(Player, AVG, Rank, Difference_2021)

print(QBsummary_table2021)

# # Output the variance
# print(paste("Variance of Differences:", variance_diff))

# Making gt table
QB_gt2021 <- QB_data2021 %>% 
  mutate(AVG = round(AVG, 1),
         Rank = round(Rank, 1),
         diff = AVG - Rank) |> 
  dplyr::select(Player, AVG, Rank, diff) |> 
  arrange(-diff) |> 
  gt() |>
  cols_align(align = "center") |> 
  cols_label(Player = "Quarterback",
             AVG = "Average Draft Position",
             Rank = "Final Rank",
             diff = "Difference_2021") |> 
  tab_header(
    title = md("**Quarterback ADP vs. Final Rank 2021**"),
    subtitle = md("**Comparing the Pre-Season and Post-Season Rankings**")
  ) %>%
  gtExtras::gt_theme_espn()

#save the gt table
gtsave(QB_gt2021, "qb_adp_vs_rank_2021.png")
summary(QB_data2021)

# Calculate mean of difference, create variance scale of distance from mean
QB_data2021

# Histogram Distribution 2021
ggplot(QB_data2021, aes(x = Difference_2021)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "QB ADP vs Finish 2021", x = "Difference", y = "Frequency")

# Boxplot Distribution 2021
ggplot(QB_data2021, aes(x = AVG, y = Rank)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "ADP vs Rank 2021", x = "AVG", y = "Rank")

# Scatterplot Distribution 2021
ggplot(QB_data2021, aes(x = AVG, y = Rank)) +
  geom_point(color = "blue") +
  labs(title = "ADP vs Rank 2021", x = "AVG", y = "Rank")

# Scatterplot of ADP vs Final Rank with player names
ggplot(QB_data2021, aes(x = AVG, y = Rank)) +
  geom_text(aes(label = Player), color = "blue", size = 3, vjust = -0.5) +  # Adds player names as text
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Adds a linear trend line
  labs(title = "Scatterplot of QB ADP vs Final Rank (2021)",
       x = "Average Draft Position (ADP)",
       y = "Final Rank") +
  theme_minimal()
```

```{r}
# 2020
library(MASS)
library(caret)
library(glmnet)
library(bayesplot)
library(loo)
QBADP2020 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\2020_QB_ADP_Rankings.csv")
QBFinal2020 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\Fantasy_Football_Statistics_QB_2020.csv")

str(QBFinal2020$YDS)
QBADP2020
QBFinal2020

FinalReason2020 <- glm(formula = QBFinal2020$Rank ~ QBFinal2020$CMP + QBFinal2020$ATT + QBFinal2020$PCT + QBFinal2020$YDS + QBFinal2020$Y.A + QBFinal2020$TD + QBFinal2020$INT + QBFinal2020$SACKS + QBFinal2020$ATT.1 + QBFinal2020$YDS + QBFinal2020$TD + QBFinal2020$FL + QBFinal2020$G + QBFinal2020$FPTS + QBFinal2020$FPTS.G + QBFinal2020$ROST)
FinalReason2020
summary(FinalReason2020)
anova(FinalReason2020)


QBFinal2020 <- QBFinal2020 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(gtExtras)
library(webshot2)
QBADP2020
QBFinal2020
FinalFix2020 <- QBFinal2020 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

# Ensure ADP and Final are numeric
QBADP2020 <- QBADP2020 %>%
  mutate(AVG = as.numeric(AVG))

FinalFix2020 <- FinalFix2020 %>%
  mutate(Rank = as.numeric(Rank))

# Add Games (G) to the Final dataset if it exists
if ("G" %in% colnames(FinalFix2020)) {
  FinalFix2020 <- FinalFix2020 %>%
    filter(G > 0)
}

# Only QBs with pass attempts are recorded
if ("ATT" %in% colnames(FinalFix2020)) {
  FinalFix2020 <- FinalFix2020 %>%
    filter (ATT > 0)
}

# Only QBs with pass attempts are recorded
if ("FPTS/G" %in% colnames(FinalFix2020)) {
  FinalFix2020 <- FinalFix2020 %>%
    filter (FPTS/G > 5)
}

FinalFix2020
QBADP2020

# Select relevant columns
QBADP2020 <- QBADP2020 %>% dplyr::select(Player, QB, AVG)
FinalFix2020 <- FinalFix2020 %>% dplyr::select(Player, Rank)

# Merge the data on Player
QB_data2020 <- inner_join(QBADP2020, FinalFix2020, by = "Player")

QBADP2020
FinalFix2020
QB_data2020

# Calculate the difference
QB_data2020 <- QB_data2020 %>%
  mutate(Difference_2020 = AVG - Rank)

# Compute the variance of the differences
variance_diff2020 <- var(QB_data2020$Difference_2020)
variance_diff2020

# Print the summary table
QBsummary_table2020 <- QB_data2020 %>%
  dplyr::select(Player, AVG, Rank, Difference_2020)

print(QBsummary_table2020)

# # Output the variance
# print(paste("Variance of Differences:", variance_diff))

# Making gt table
QB_gt2020 <- QB_data2020 %>% 
  mutate(AVG = round(AVG, 1),
         Rank = round(Rank, 1),
         diff = AVG - Rank) |> 
  dplyr::select(Player, AVG, Rank, diff) |> 
  arrange(-diff) |> 
  gt() |>
  cols_align(align = "center") |> 
  cols_label(Player = "Quarterback",
             AVG = "Average Draft Position",
             Rank = "Final Rank",
             diff = "Difference_2020") |> 
  tab_header(
    title = md("**Quarterback ADP vs. Final Rank 2020**"),
    subtitle = md("**Comparing the Pre-Season and Post-Season Rankings**")
  ) %>%
  gtExtras::gt_theme_espn()

#save the gt table
gtsave(QB_gt2020, "qb_adp_vs_rank_2020.png")
summary(QB_data2020)

# Calculate mean of difference, create variance scale of distance from mean
QB_data2020

# Histogram Distribution 2020
ggplot(QB_data2020, aes(x = Difference_2020)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "QB ADP vs Finish 2020", x = "Difference", y = "Frequency")

# Boxplot Distribution 2020
ggplot(QB_data2020, aes(x = AVG, y = Rank)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "ADP vs Rank 2020", x = "AVG", y = "Rank")

# Scatterplot Distribution
ggplot(QB_data2020, aes(x = AVG, y = Rank)) +
  geom_point(color = "blue") +
  labs(title = "ADP vs Rank 2020", x = "AVG", y = "Rank")

# Scatterplot of ADP vs Final Rank with player names
ggplot(QB_data2020, aes(x = AVG, y = Rank)) +
  geom_text(aes(label = Player), color = "blue", size = 3, vjust = -0.5) +  # Adds player names as text
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Adds a linear trend line
  labs(title = "Scatterplot of QB ADP vs Final Rank (2020)",
       x = "Average Draft Position (ADP)",
       y = "Final Rank") +
  theme_minimal()
```


```{r}
# 2019
library(MASS)
library(caret)
library(glmnet)
library(bayesplot)
library(loo)
QBADP2019 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\2019_QB_ADP_Rankings.csv")
QBFinal2019 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\Fantasy_Football_Statistics_QB_2019.csv")

str(QBFinal2019$YDS)
QBADP2019
QBFinal2019

FinalReason2019 <- glm(formula = QBFinal2019$Rank ~ QBFinal2019$CMP + QBFinal2019$ATT + QBFinal2019$PCT + QBFinal2019$YDS + QBFinal2019$Y.A + QBFinal2019$TD + QBFinal2019$INT + QBFinal2019$SACKS + QBFinal2019$ATT.1 + QBFinal2019$YDS + QBFinal2019$TD + QBFinal2019$FL + QBFinal2019$G + QBFinal2019$FPTS + QBFinal2019$FPTS.G + QBFinal2019$ROST)
FinalReason2019
summary(FinalReason2019)
anova(FinalReason2019)


QBFinal2019 <- QBFinal2019 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(gtExtras)
library(webshot2)
QBADP2019
QBFinal2019
FinalFix2019 <- QBFinal2019 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

# Ensure ADP and Final are numeric
QBADP2019 <- QBADP2019 %>%
  mutate(AVG = as.numeric(AVG))

FinalFix2019 <- FinalFix2019 %>%
  mutate(Rank = as.numeric(Rank))

# Add Games (G) to the Final dataset if it exists
if ("G" %in% colnames(FinalFix2019)) {
  FinalFix2019 <- FinalFix2019 %>%
    filter(G > 0)
}

# Only QBs with pass attempts are recorded
if ("ATT" %in% colnames(FinalFix2019)) {
  FinalFix2019 <- FinalFix2019 %>%
    filter (ATT > 0)
}

# Only QBs with pass attempts are recorded
if ("FPTS/G" %in% colnames(FinalFix2019)) {
  FinalFix2019 <- FinalFix2019 %>%
    filter (FPTS/G > 5)
}

QBADP2019 <- QBADP2019 %>% 
  filter(Player != "Nick Mullens")

FinalFix2019
QBADP2019

# Select relevant columns
QBADP2019 <- QBADP2019 %>% dplyr::select(Player, QB, AVG)
FinalFix2019 <- FinalFix2019 %>% dplyr::select(Player, Rank)

# Merge the data on Player
QB_data2019 <- inner_join(QBADP2019, FinalFix2019, by = "Player")

QBADP2019
FinalFix2019
QB_data2019

# Calculate the difference
QB_data2019 <- QB_data2019 %>%
  mutate(Difference_2019 = AVG - Rank)

# Compute the variance of the differences
variance_diff2019 <- var(QB_data2019$Difference_2019)
variance_diff2019

# Print the summary table
QBsummary_table2019 <- QB_data2019 %>%
  dplyr::select(Player, AVG, Rank, Difference_2019)

print(QBsummary_table2019)

# # Output the variance
# print(paste("Variance of Differences:", variance_diff))

# Making gt table
QB_gt2019 <- QB_data2019 %>% 
  mutate(AVG = round(AVG, 1),
         Rank = round(Rank, 1),
         diff = AVG - Rank) |> 
  dplyr::select(Player, AVG, Rank, diff) |> 
  arrange(-diff) |> 
  gt() |>
  cols_align(align = "center") |> 
  cols_label(Player = "Quarterback",
             AVG = "Average Draft Position",
             Rank = "Final Rank",
             diff = "Difference_2019") |> 
  tab_header(
    title = md("**Quarterback ADP vs. Final Rank 2019**"),
    subtitle = md("**Comparing the Pre-Season and Post-Season Rankings**")
  ) %>%
  gtExtras::gt_theme_espn()

#save the gt table
gtsave(QB_gt2019, "qb_adp_vs_rank_2019.png")
summary(QB_data2019)

# Calculate mean of difference, create variance scale of distance from mean
QB_data2019

# Histogram Distribution 2019
ggplot(QB_data2019, aes(x = Difference_2019)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "QB ADP vs Finish 2019", x = "Difference", y = "Frequency")

# Boxplot Distribution 2019
ggplot(QB_data2019, aes(x = AVG, y = Rank)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "ADP vs Rank 2019", x = "AVG", y = "Rank")

# Scatterplot Distribution
ggplot(QB_data2019, aes(x = AVG, y = Rank)) +
  geom_point(color = "blue") +
  labs(title = "ADP vs Rank 2019", x = "AVG", y = "Rank")

# Scatterplot of ADP vs Final Rank with player names
ggplot(QB_data2019, aes(x = AVG, y = Rank)) +
  geom_text(aes(label = Player), color = "blue", size = 3, vjust = -0.5) +  # Adds player names as text
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Adds a linear trend line
  labs(title = "Scatterplot of QB ADP vs Final Rank (2019)",
       x = "Average Draft Position (ADP)",
       y = "Final Rank") +
  theme_minimal()
```

```{r}
# 2018
library(MASS)
library(caret)
library(glmnet)
library(bayesplot)
library(loo)
QBADP2018 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\2018_QB_ADP_Rankings.csv")
QBFinal2018 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\Fantasy_Football_Statistics_QB_2018.csv")

str(QBFinal2018$YDS)
QBADP2018
QBFinal2018

FinalReason2018 <- glm(formula = QBFinal2018$Rank ~ QBFinal2018$CMP + QBFinal2018$ATT + QBFinal2018$PCT + QBFinal2018$YDS + QBFinal2018$Y.A + QBFinal2018$TD + QBFinal2018$INT + QBFinal2018$SACKS + QBFinal2018$ATT.1 + QBFinal2018$YDS + QBFinal2018$TD + QBFinal2018$FL + QBFinal2018$G + QBFinal2018$FPTS + QBFinal2018$FPTS.G + QBFinal2018$ROST)
FinalReason2018
summary(FinalReason2018)
anova(FinalReason2018)


QBFinal2018 <- QBFinal2018 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(gtExtras)
library(webshot2)
QBADP2018
QBFinal2018
FinalFix2018 <- QBFinal2018 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

# Ensure ADP and Final are numeric
QBADP2018 <- QBADP2018 %>%
  mutate(AVG = as.numeric(AVG))

FinalFix2018 <- FinalFix2018 %>%
  mutate(Rank = as.numeric(Rank))

# Add Games (G) to the Final dataset if it exists
if ("G" %in% colnames(FinalFix2018)) {
  FinalFix2018 <- FinalFix2018 %>%
    filter(G > 0)
}

# Only QBs with pass attempts are recorded
if ("ATT" %in% colnames(FinalFix2018)) {
  FinalFix2018 <- FinalFix2018 %>%
    filter (ATT > 0)
}

# Only QBs with pass attempts are recorded
if ("FPTS/G" %in% colnames(FinalFix2018)) {
  FinalFix2018 <- FinalFix2018 %>%
    filter (FPTS/G > 5)
}

FinalFix2018
QBADP2018

# Select relevant columns
QBADP2018 <- QBADP2018 %>% dplyr::select(Player, QB, AVG)
FinalFix2018 <- FinalFix2018 %>% dplyr::select(Player, Rank)

# Merge the data on Player
QB_data2018 <- inner_join(QBADP2018, FinalFix2018, by = "Player")

QBADP2018
FinalFix2018
QB_data2018

# Calculate the difference
QB_data2018 <- QB_data2018 %>%
  mutate(Difference_2018 = AVG - Rank)

# Compute the variance of the differences
variance_diff2018 <- var(QB_data2018$Difference_2018)
variance_diff2018

# Print the summary table
QBsummary_table2018 <- QB_data2018 %>%
  dplyr::select(Player, AVG, Rank, Difference_2018)

print(QBsummary_table2018)

# # Output the variance
# print(paste("Variance of Differences:", variance_diff))

# Making gt table
QB_gt2018 <- QB_data2018 %>% 
  mutate(AVG = round(AVG, 1),
         Rank = round(Rank, 1),
         diff = AVG - Rank) |> 
  dplyr::select(Player, AVG, Rank, diff) |> 
  arrange(-diff) |> 
  gt() |>
  cols_align(align = "center") |> 
  cols_label(Player = "Quarterback",
             AVG = "Average Draft Position",
             Rank = "Final Rank",
             diff = "Difference_2018") |> 
  tab_header(
    title = md("**Quarterback ADP vs. Final Rank 2018**"),
    subtitle = md("**Comparing the Pre-Season and Post-Season Rankings**")
  ) %>%
  gtExtras::gt_theme_espn()

#save the gt table
gtsave(QB_gt2018, "qb_adp_vs_rank_2018.png")
summary(QB_data2018)

# Calculate mean of difference, create variance scale of distance from mean
QB_data2018

# Histogram Distribution 2018
ggplot(QB_data2018, aes(x = Difference_2018)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "QB ADP vs Finish 2018", x = "Difference", y = "Frequency")

# Boxplot Distribution 2018
ggplot(QB_data2018, aes(x = AVG, y = Rank)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "ADP vs Rank 2018", x = "AVG", y = "Rank")

# Scatterplot Distribution 2018
ggplot(QB_data2018, aes(x = AVG, y = Rank)) +
  geom_point(color = "blue") +
  labs(title = "ADP vs Rank 2018", x = "AVG", y = "Rank")

# Scatterplot of ADP vs Final Rank with player names
ggplot(QB_data2018, aes(x = AVG, y = Rank)) +
  geom_text(aes(label = Player), color = "blue", size = 3, vjust = -0.5) +  # Adds player names as text
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Adds a linear trend line
  labs(title = "Scatterplot of QB ADP vs Final Rank (2018)",
       x = "Average Draft Position (ADP)",
       y = "Final Rank") +
  theme_minimal()
```

```{r}
# 2017
library(MASS)
library(caret)
library(glmnet)
library(bayesplot)
library(loo)
QBADP2017 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\2017_QB_ADP_Rankings.csv")
QBFinal2017 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\Fantasy_Football_Statistics_QB_2017.csv")

str(QBFinal2017$YDS)
QBADP2017
QBFinal2017

FinalReason2017 <- glm(formula = QBFinal2017$Rank ~ QBFinal2017$CMP + QBFinal2017$ATT + QBFinal2017$PCT + QBFinal2017$YDS + QBFinal2017$Y.A + QBFinal2017$TD + QBFinal2017$INT + QBFinal2017$SACKS + QBFinal2017$ATT.1 + QBFinal2017$YDS + QBFinal2017$TD + QBFinal2017$FL + QBFinal2017$G + QBFinal2017$FPTS + QBFinal2017$FPTS.G + QBFinal2017$ROST)
FinalReason2017
summary(FinalReason2017)
anova(FinalReason2017)


QBFinal2017 <- QBFinal2017 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(gtExtras)
library(webshot2)
QBADP2017
QBFinal2017
FinalFix2017 <- QBFinal2017 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

# Ensure ADP and Final are numeric
QBADP2017 <- QBADP2017 %>%
  mutate(AVG = as.numeric(AVG))

FinalFix2017 <- FinalFix2017 %>%
  mutate(Rank = as.numeric(Rank))

# Add Games (G) to the Final dataset if it exists
if ("G" %in% colnames(FinalFix2017)) {
  FinalFix2017 <- FinalFix2017 %>%
    filter(G > 0)
}

# Only QBs with pass attempts are recorded
if ("ATT" %in% colnames(FinalFix2017)) {
  FinalFix2017 <- FinalFix2017 %>%
    filter (ATT > 0)
}

# Only QBs with pass attempts are recorded
if ("FPTS/G" %in% colnames(FinalFix2017)) {
  FinalFix2017 <- FinalFix2017 %>%
    filter (FPTS/G > 5)
}

FinalFix2017
QBADP2017

# Select relevant columns
QBADP2017 <- QBADP2017 %>% dplyr::select(Player, QB, AVG)
FinalFix2017 <- FinalFix2017 %>% dplyr::select(Player, Rank)

# Merge the data on Player
QB_data2017 <- inner_join(QBADP2017, FinalFix2017, by = "Player")

QBADP2017
FinalFix2017
QB_data2017

# Calculate the difference
QB_data2017 <- QB_data2017 %>%
  mutate(Difference_2017 = AVG - Rank)

# Compute the variance of the differences
variance_diff2017 <- var(QB_data2017$Difference_2017)
variance_diff2017

# Print the summary table
QBsummary_table2017 <- QB_data2017 %>%
  dplyr::select(Player, AVG, Rank, Difference_2017)

print(QBsummary_table2017)

# # Output the variance
# print(paste("Variance of Differences:", variance_diff))

# Making gt table
QB_gt2017 <- QB_data2017 %>% 
  mutate(AVG = round(AVG, 1),
         Rank = round(Rank, 1),
         diff = AVG - Rank) |> 
  dplyr::select(Player, AVG, Rank, diff) |> 
  arrange(-diff) |> 
  gt() |>
  cols_align(align = "center") |> 
  cols_label(Player = "Quarterback",
             AVG = "Average Draft Position",
             Rank = "Final Rank",
             diff = "Difference_2017") |> 
  tab_header(
    title = md("**Quarterback ADP vs. Final Rank 2017**"),
    subtitle = md("**Comparing the Pre-Season and Post-Season Rankings**")
  ) %>%
  gtExtras::gt_theme_espn()

#save the gt table
gtsave(QB_gt2017, "qb_adp_vs_rank_2017.png")
summary(QB_data2017)

# Calculate mean of difference, create variance scale of distance from mean
QB_data2017

# Histogram Distribution 2017
ggplot(QB_data2017, aes(x = Difference_2017)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "QB ADP vs Finish 2017", x = "Difference", y = "Frequency")

# Boxplot Distribution 2017
ggplot(QB_data2017, aes(x = AVG, y = Rank)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "ADP vs Rank 2017", x = "AVG", y = "Rank")

# Scatterplot Distribution 2017
ggplot(QB_data2017, aes(x = AVG, y = Rank)) +
  geom_point(color = "blue") +
  labs(title = "ADP vs Rank 2017", x = "AVG", y = "Rank")

# Scatterplot of ADP vs Final Rank with player names
ggplot(QB_data2017, aes(x = AVG, y = Rank)) +
  geom_text(aes(label = Player), color = "blue", size = 3, vjust = -0.5) +  # Adds player names as text
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Adds a linear trend line
  labs(title = "Scatterplot of QB ADP vs Final Rank (2017)",
       x = "Average Draft Position (ADP)",
       y = "Final Rank") +
  theme_minimal()
```

```{r}
# 2016
library(MASS)
library(caret)
library(glmnet)
library(bayesplot)
library(loo)
QBADP2016 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\2016_QB_ADP_Rankings.csv")
QBFinal2016 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\Fantasy_Football_Statistics_QB_2016.csv")

str(QBFinal2016$YDS)
QBADP2016
QBFinal2016

FinalReason2016 <- glm(formula = QBFinal2016$Rank ~ QBFinal2016$CMP + QBFinal2016$ATT + QBFinal2016$PCT + QBFinal2016$YDS + QBFinal2016$Y.A + QBFinal2016$TD + QBFinal2016$INT + QBFinal2016$SACKS + QBFinal2016$ATT.1 + QBFinal2016$YDS + QBFinal2016$TD + QBFinal2016$FL + QBFinal2016$G + QBFinal2016$FPTS + QBFinal2016$FPTS.G + QBFinal2016$ROST)
FinalReason2016
summary(FinalReason2016)
anova(FinalReason2016)


QBFinal2016 <- QBFinal2016 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(gtExtras)
library(webshot2)
QBADP2016
QBFinal2016
FinalFix2016 <- QBFinal2016 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

# Ensure ADP and Final are numeric
QBADP2016 <- QBADP2016 %>%
  mutate(AVG = as.numeric(AVG))

FinalFix2016 <- FinalFix2016 %>%
  mutate(Rank = as.numeric(Rank))

# Add Games (G) to the Final dataset if it exists
if ("G" %in% colnames(FinalFix2016)) {
  FinalFix2016 <- FinalFix2016 %>%
    filter(G > 0)
}

# Only QBs with pass attempts are recorded
if ("ATT" %in% colnames(FinalFix2016)) {
  FinalFix2016 <- FinalFix2016 %>%
    filter (ATT > 0)
}

# Only QBs with pass attempts are recorded
if ("FPTS/G" %in% colnames(FinalFix2016)) {
  FinalFix2016 <- FinalFix2018 %>%
    filter (FPTS/G > 5)
}

FinalFix2016
QBADP2016

# Select relevant columns
QBADP2016 <- QBADP2016 %>% dplyr::select(Player, QB, AVG)
FinalFix2016 <- FinalFix2016 %>% dplyr::select(Player, Rank)

# Merge the data on Player
QB_data2016 <- inner_join(QBADP2016, FinalFix2016, by = "Player")

QBADP2016
FinalFix2016
QB_data2016

# Calculate the difference
QB_data2016 <- QB_data2016 %>%
  mutate(Difference_2016 = AVG - Rank)

# Compute the variance of the differences
variance_diff2016 <- var(QB_data2016$Difference_2016)
variance_diff2016

# Print the summary table
QBsummary_table2016 <- QB_data2016 %>%
  dplyr::select(Player, AVG, Rank, Difference_2016)

print(QBsummary_table2016)

# # Output the variance
# print(paste("Variance of Differences:", variance_diff))

# Making gt table
QB_gt2016 <- QB_data2016 %>% 
  mutate(AVG = round(AVG, 1),
         Rank = round(Rank, 1),
         diff = AVG - Rank) |> 
  dplyr::select(Player, AVG, Rank, diff) |> 
  arrange(-diff) |> 
  gt() |>
  cols_align(align = "center") |> 
  cols_label(Player = "Quarterback",
             AVG = "Average Draft Position",
             Rank = "Final Rank",
             diff = "Difference_2016") |> 
  tab_header(
    title = md("**Quarterback ADP vs. Final Rank 2016**"),
    subtitle = md("**Comparing the Pre-Season and Post-Season Rankings**")
  ) %>%
  gtExtras::gt_theme_espn()

#save the gt table
gtsave(QB_gt2016, "qb_adp_vs_rank_2016.png")
summary(QB_data2016)

# Calculate mean of difference, create variance scale of distance from mean
QB_data2016

# Histogram Distribution 2016
ggplot(QB_data2016, aes(x = Difference_2016)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "QB ADP vs Finish 2016", x = "Difference", y = "Frequency")

# Boxplot Distribution 2016
ggplot(QB_data2016, aes(x = AVG, y = Rank)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "ADP vs Rank 2016", x = "AVG", y = "Rank")

# Scatterplot Distribution 2016
ggplot(QB_data2016, aes(x = AVG, y = Rank)) +
  geom_point(color = "blue") +
  labs(title = "ADP vs Rank 2016", x = "AVG", y = "Rank")

# Assuming you've already created a scatterplot and stored it in `data2017`
ggplot(QB_data2016, aes(x = AVG, y = Rank)) +
  geom_text(aes(label = Player), color = "blue", size = 3, vjust = -0.5) +  # Adds player names as text
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Adds a linear trend line
  labs(title = "Scatterplot of QB ADP vs Final Rank (2016)",
       x = "Average Draft Position (ADP)",
       y = "Final Rank") +
  theme_minimal()
```

```{r}
# 2015
library(MASS)
library(caret)
library(glmnet)
library(bayesplot)
library(loo)
QBADP2015 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\2015_QB_ADP_Rankings.csv")
QBFinal2015 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\ADP vs Rank\\Fantasy_Football_Statistics_QB_2015.csv")

str(QBFinal2015$YDS)
QBADP2015
QBFinal2015

FinalReason2015 <- glm(formula = QBFinal2015$Rank ~ QBFinal2015$CMP + QBFinal2015$ATT + QBFinal2015$PCT + QBFinal2015$YDS + QBFinal2015$Y.A + QBFinal2015$TD + QBFinal2015$INT + QBFinal2015$SACKS + QBFinal2015$ATT.1 + QBFinal2015$YDS + QBFinal2015$TD + QBFinal2015$FL + QBFinal2015$G + QBFinal2015$FPTS + QBFinal2015$FPTS.G + QBFinal2015$ROST)
FinalReason2015
summary(FinalReason2015)
anova(FinalReason2015)


QBFinal2015 <- QBFinal2015 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(gtExtras)
library(webshot2)
QBADP2015
QBFinal2015
FinalFix2015 <- QBFinal2015 %>%
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

# Ensure ADP and Final are numeric
QBADP2015 <- QBADP2015 %>%
  mutate(AVG = as.numeric(AVG))

FinalFix2015 <- FinalFix2015 %>%
  mutate(Rank = as.numeric(Rank))

# Add Games (G) to the Final dataset if it exists
if ("G" %in% colnames(FinalFix2015)) {
  FinalFix2015 <- FinalFix2015 %>%
    filter(G > 0)
}

# Only QBs with pass attempts are recorded
if ("ATT" %in% colnames(FinalFix2015)) {
  FinalFix2015 <- FinalFix2015 %>%
    filter (ATT > 0)
}

# Only QBs with points are recorded
if ("FPTS/G" %in% colnames(FinalFix2015)) {
  FinalFix2015 <- FinalFix2015 %>%
    filter (FPTS/G > 5)
}

FinalFix2015
QBADP2015

# Select relevant columns
QBADP2015 <- QBADP2015 %>% dplyr::select(Player, QB, AVG)
FinalFix2015 <- FinalFix2015 %>% dplyr::select(Player, Rank)

# Merge the data on Player
QB_data2015 <- inner_join(QBADP2015, FinalFix2015, by = "Player")

QBADP2015
FinalFix2015
QB_data2015

# Calculate the difference
QB_data2015 <- QB_data2015 %>%
  mutate(Difference_2015 = AVG - Rank)

# Compute the variance of the differences
variance_diff2015 <- var(QB_data2015$Difference_2015)
variance_diff2015

# Print the summary table
QBsummary_table2015 <- QB_data2015 %>%
  dplyr::select(Player, AVG, Rank, Difference_2015)

print(QBsummary_table2015)

# # Output the variance
# print(paste("Variance of Differences:", variance_diff))

# Making gt table
QB_gt2015 <- QB_data2015 %>% 
  mutate(AVG = round(AVG, 1),
         Rank = round(Rank, 1),
         diff = AVG - Rank) |> 
  dplyr::select(Player, AVG, Rank, diff) |> 
  arrange(-diff) |> 
  gt() |>
  cols_align(align = "center") |> 
  cols_label(Player = "Quarterback",
             AVG = "Average Draft Position",
             Rank = "Final Rank",
             diff = "Difference_2015") |> 
  tab_header(
    title = md("**Quarterback ADP vs. Final Rank 2015**"),
    subtitle = md("**Comparing the Pre-Season and Post-Season Rankings**")
  ) %>%
  gtExtras::gt_theme_espn()

#save the gt table
gtsave(QB_gt2015, "qb_adp_vs_rank_2015.png")
summary(QB_data2015)

# Calculate mean of difference, create variance scale of distance from mean
QB_data2015

# Histogram Distribution 2015
ggplot(QB_data2015, aes(x = Difference_2015)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "QB ADP vs Finish 2015", x = "Difference", y = "Frequency")

# Boxplot Distribution 2015
ggplot(QB_data2015, aes(x = AVG, y = Rank)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "ADP vs Rank 2015", x = "AVG", y = "Rank")

# Scatterplot Distribution 2015
ggplot(QB_data2015, aes(x = AVG, y = Rank)) +
  geom_point(color = "blue") +
  labs(title = "ADP vs Rank 2015", x = "AVG", y = "Rank")

# Assuming you've already created a scatterplot and stored it in `data2017`
ggplot(QB_data2015, aes(x = AVG, y = Rank)) +
  geom_text(aes(label = Player), color = "blue", size = 3, vjust = -0.5) +  # Adds player names as text
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Adds a linear trend line
  labs(title = "Scatterplot of QB ADP vs Final Rank (2015)",
       x = "Average Draft Position (ADP)",
       y = "Final Rank") +
  theme_minimal()
```

```{r}
library(purrr)

# Create a list of your datasets
datasets <- list(QB_data2023, QB_data2022, QB_data2021, QB_data2020, QB_data2019, QB_data2018, QB_data2017, QB_data2016, QB_data2015)

QBADP2015

# Perform a full join across all datasets
combined_QBdata <- reduce(datasets, full_join, by = "Player", "Team")
combined_QBdata

# Making gt table
QB_gt2023to2015 <- combined_QBdata %>% 
  dplyr::select(Player, Difference_2023, Difference_2022, Difference_2021, Difference_2020, Difference_2019, Difference_2018, Difference_2017, Difference_2016, Difference_2015) |> 
  gt() |>
  cols_align(align = "center") |> 
  cols_label(Player = "Quarterback",
             Difference_2023 = "Difference_2023",
             Difference_2022 = "Difference_2022",
             Difference_2021 = "Difference_2021",
             Difference_2020 = "Difference_2020",
             Difference_2019 = "Difference_2019",
             Difference_2018 = "Difference_2018",
             Difference_2017 = "Difference_2017",
             Difference_2016 = "Difference_2016",
             Difference_2015 = "Difference_2015") |> 
  tab_header(
    title = md("**Quarterback ADP vs. Final Rank**"),
    subtitle = md("**Comparing the Pre-Season and Post-Season Rankings**")
  ) %>%
  gtExtras::gt_theme_espn()

QB_gt2023to2015

#save the gt table
gtsave(QB_gt2023to2015, "qb_adp_vs_rank_by_year.png")
```

```{r}
library(magick)

BUF <- image_read("C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\BUF.png")

library(dplyr)

combined_QBdata

# Pivot longer to calculate mean difference for each year
mean_diff_by_year <- combined_QBdata %>%
  pivot_longer(cols = starts_with("Difference_"), names_to = "Year", values_to = "Difference") %>%
  group_by(Year) %>%
  summarize(Mean_Difference = mean(Difference, na.rm = TRUE)) %>%
  arrange(desc(Mean_Difference))

mean_diff_by_year

# Calculate mean difference for each player
mean_diff_by_player <- combined_QBdata %>%
  rowwise() %>%
  mutate(Mean_Difference = mean(c_across(starts_with("Difference_")), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Mean_Difference)) %>%
  select(Player, Mean_Difference)

mean_diff_by_player

library(ggplot2)

# Bar chart for mean difference by year
ggplot(mean_diff_by_year, aes(x = reorder(Year, -Mean_Difference), y = Mean_Difference)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Mean Difference by Year",
       x = "Year",
       y = "Mean Difference")

# Bar chart for mean difference by player
ggplot(mean_diff_by_player, aes(x = reorder(Player, -Mean_Difference), y = Mean_Difference)) +
  geom_bar(stat = "identity", fill = "darkred") +
  theme_minimal() +
  labs(title = "Mean Difference by Player",
       x = "Player",
       y = "Mean Difference")

mean_diff_by_player

library(dplyr)

# Calculate the sum of differences and the number of non-NA values for each player
mean_diff_by_playerQB <- combined_QBdata %>%
  rowwise() %>%
  mutate(
    Sum_Difference = sum(c_across(starts_with("Difference_")), na.rm = TRUE),
    Count_Years = sum(!is.na(c_across(starts_with("Difference_")))),
    Mean_Difference = Sum_Difference / Count_Years
  ) %>%
  ungroup() %>%
  arrange(desc(Mean_Difference)) %>%
  select(Player, Sum_Difference, Count_Years, Mean_Difference)

mean_diff_by_playerQB

combined_QBdata
names(combined_QBdata)

# Custom function to find the mode (most frequent value) 
find_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

combined_QBdata <- combined_QBdata %>%
  mutate(Frequent_Team = NA_character_)

manual_updates <- data.frame(
  Player = c("Ryan Tannehill", "Blake Bortles", "Philip Rivers", "Ryan Fitzpatrick", "Tom Brady", "Josh McCown", "Ben Roethlisberger", "Eli Manning", "Matt Ryan", "Cam Newton", "Drew Brees", "Brian Hoyer", "Teddy Bridgewater", "Nick Foles", "Aj McCarron", "Sam Bradford", "Jared Goff", "Kirk Cousins", "Josh Allen", "Justin Herbert", "Daniel Jones", "Patrick Mahomes II", "Tua Tagovailoa", "Matthew Stafford", "Derek Carr", "Kyler Murray", "Dak Prescott", "Deshaun Watson", "Joe Burrow", "Andy Dalton", "Russell Wilson", "Carson Wentz", "Baker Mayfield", "Geno Smith", "Sam Darnold", "Mitchell Trubisky", "Lamar Jackson", "Joe Flacco", "Jimmy Garoppolo", "Marcus Mariota", "Aaron Rodgers", "AJ McCarron", "Jameis Winston", "Tyrod Taylor"),  
  Frequent_Team = c("TEN", "JAC", "IND", "WAS", "NE", "PHI", "PIT", "NYG", "ATL", "CAR", "NO", "NE", "MIN", "PHI", "CIN", "LAR", "LAR", "MIN", "BUF", "LAC", "NYG", "KC", "MIA", "DET", "LV", "ARI", "DAL", "HOU", "CIN", "CIN", "SEA", "PHI", "CLE", "SEA", "NYJ", "CHI", "BAL", "BAL", "NE", "TEN", "GB", "CIN", "TB", "BUF")  
)

mean_diff_by_playerQB <- combined_QBdata %>%
  rowwise() %>%
  mutate(
    Sum_Difference = sum(c_across(starts_with("Difference_")), na.rm = TRUE),
    Count_Years = sum(!is.na(c_across(starts_with("Difference_")))),
    Mean_Difference = Sum_Difference / Count_Years
  ) %>%
  ungroup() %>%
  filter(Count_Years > 3) %>%
  arrange(desc(Mean_Difference)) %>%
  select(Player, Mean_Difference, Count_Years, Frequent_Team)

mean_diff_by_playerQB

mean_diff_by_playerQB <- mean_diff_by_playerQB %>%
  mutate(Frequent_Team = as.character(Frequent_Team))

mean_diff_by_playerQB <- mean_diff_by_playerQB %>%
  left_join(manual_updates, by = "Player", suffix = c("", ".update")) %>%
  mutate(
    Frequent_Team = coalesce(Frequent_Team.update, Frequent_Team)
  ) %>%
  select(-Frequent_Team.update)


library(gt)

QB_gtDiffYears <- mean_diff_by_playerQB %>% 
  dplyr::select(Player, Mean_Difference, Count_Years, Frequent_Team) |> 
  gt() |>
  cols_align(align = "center") |> 
  cols_label(
    Player = "Quarterback",
    Mean_Difference = "Avg. Diff",
    Count_Years = "Years",
    Frequent_Team = "Team"
  ) |> 
  tab_header(
    title = md("**Quarterback ADP vs. Final Rank**"),
    subtitle = md("**Comparing the Average Pre-Season and Post-Season Rankings**")
  ) %>%
  gtExtras::gt_theme_espn()

QB_gtDiffYears

#save the gt table
gtsave(QB_gtDiffYears, "qb_adp_vs_rank_by_year_diff.png")

### TEST
# Create a named vector for the team image paths
team_logos <- c(
  "BUF" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\BUF.png",
  "MIA" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\MIA.png",
  "NE" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\NE.png",
  "NYJ" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\NYJ.png",
  "BAL" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\BAL.png",
  "CIN" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\CIN.png",
  "CLE" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\CLE.png",
  "PIT" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\PIT.png",
  "HOU" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\HOU.png",
  "IND" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\IND.png",
  "JAC" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\JAC.png",
  "TEN" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\TEN.png",
  "DEN" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\DEN.png",
  "KC" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\KC.png",
  "LV" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\LV.png",
  "LAC" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\LAC.png",
  "DAL" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\DAL.png",
  "NYG" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\NYG.png",
  "PHI" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\PHI.png",
  "WAS" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\WAS.png",
  "CHI" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\CHI.png",
  "DET" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\DET.png",
  "GB" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\GB.png",
  "MIN" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\MIN.png",
  "ATL" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\ATL.png",
  "CAR" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\CAR.png",
  "NO" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\NO.png",
  "TB" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\TB.png",
  "ARI" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\ARI.png",
  "LAR" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\LAR.png",
  "SF" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\SF.png",
  "SEA" = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\NFL\\SEA.png"
  # Add other teams as you add their images, e.g.,
  # "NE" = "C:\\path\\to\\NE.png",
  # "DAL" = "C:\\path\\to\\DAL.png",
  # etc.
)

add_team_logo <- function(team_abbreviation) {
  team_logos[team_abbreviation]
}

mean_diff_by_playerQB <- mean_diff_by_playerQB %>%
  mutate(Team_Logo = sapply(Frequent_Team, add_team_logo))

library(gt)

QB_gtDiffYears <- mean_diff_by_playerQB %>%
  dplyr::select(Player, Mean_Difference, Count_Years, Team_Logo) |> 
  gt() |>
  cols_align(align = "center") |>
  cols_label(
    Player = "Quarterback",
    Mean_Difference = "Avg. Diff",
    Count_Years = "Years",
    Team_Logo = "Team"
  ) |> 
  text_transform(
    locations = cells_body(columns = vars(Team_Logo)),
    fn = function(x) {
      web_image(url = x, height = px(30))  # Adjust height as needed
    }
  ) |>
  tab_header(
    title = md("**Quarterback ADP vs. Final Rank**"),
    subtitle = md("**Comparing the Average Pre-Season and Post-Season Rankings**")
  ) %>%
  gtExtras::gt_theme_espn()

QB_gtDiffYears

# Save the gt table
gtsave(QB_gtDiffYears, "qb_adp_vs_rank_by_year_diff_with_logos.png")

combined_QBdata

# Count the number of positive differences for each player
positive_diff_count <- combined_QBdata %>%
  rowwise() %>%
  mutate(Positive_Count = sum(c_across(starts_with("Difference_")) > 0, na.rm = TRUE)) %>%
  ungroup() %>%
  select(Player, Positive_Count)
positive_diff_count

# You can arrange the table to see which players had the most positive differences
positive_diff_count <- positive_diff_count %>%
  arrange(desc(Positive_Count))

# Display the result
print(positive_diff_count)

# Limit the table to the top 20 values
positive_diff_count_top20 <- positive_diff_count %>%
  head(20)

# Create a gt table and add a title
positive_diff_count_gt <- positive_diff_count_top20 %>%
  gt() %>%
  tab_header(
    title = md("**Quarterbacks with Frequently Higher Finishes than ADP Value**"),
    subtitle = md("**Filtered for Positive Values**")
  )

#save the gt table
gtsave(positive_diff_count_gt, "qb_positive_count.png")

### Combined
library(dplyr)

# Count the number of positive and negative differences for each player
diff_count_summary <- combined_QBdata %>%
  rowwise() %>%
  mutate(
    Positive_Count = sum(c_across(starts_with("Difference_")) > 0, na.rm = TRUE),
    Negative_Count = sum(c_across(starts_with("Difference_")) < 0, na.rm = TRUE),
    Total_Count = Positive_Count - Negative_Count
  ) %>%
  ungroup() %>%
  select(Player, Positive_Count, Negative_Count, Total_Count)

# Optionally, arrange the table by Total_Count
diff_count_summary <- diff_count_summary %>%
  arrange(desc(Total_Count))

# Display the result
print(diff_count_summary)

### Improved
# Count the number of positive and negative differences for each player
diff_count_summary_improved <- combined_QBdata %>%
  rowwise() %>%
  mutate(
    Positive_Count = sum(c_across(starts_with("Difference_")) > 0, na.rm = TRUE),
    Negative_Count = sum(c_across(starts_with("Difference_")) < 0, na.rm = TRUE),
    Total_Count = Positive_Count - Negative_Count
  ) %>%
  ungroup() %>%
  # Filter to include only those with Total_Count > 2 or Total_Count < -2
  filter(Total_Count > 1 | Total_Count < -1) %>%
  arrange(desc(Total_Count)) %>%  # Optional: Arrange by Total_Count in descending order
  select(Player, Positive_Count, Negative_Count, Total_Count)

# Create a gt table and add a title
diff_count_summary_improved_gt <- diff_count_summary_improved %>%
  gt() %>%
  tab_header(
    title = md("**Quarterbacks with Significant ADP vs Rank Differences**"),
    subtitle = md("**Filtered for Total Count Greater than 1 or Less than -1**")
  )

# View the tables
diff_count_summary_improved
diff_count_summary_improved_gt
#save the gt table
gtsave(diff_count_summary_improved_gt, "qb_diff_count.png")
```
