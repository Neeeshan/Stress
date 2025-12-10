# Install & Load Packages

# install.packages("tidyverse")
# install.packages("gtsummary")
# install.packages("gt")
# install.packages("gridExtra")
# install.packages("sjPlot")
# install.packages("cowplot")
# install.packages("naniar")
# install.packages("extrafont")
# install.packages("ggpubr")
# install.packages("corrplot")
#install.packages("broom")



library(tidyverse)
library(gtsummary)
library(gt)
library(gridExtra)
library(sjPlot)
library(cowplot)
library(naniar)
library(extrafont)
library(ggpubr)
library(corrplot)
library(broom)
library(broom.helpers)



# Read Data

Stress <- read.csv("data/Stress_in_Focus.csv")

# 1. Dimension of a dataset, rows and columns

dim(Stress)
nrow(Stress)
ncol(Stress)

# 2. Show column names (Variables Name)

names(Stress)

# 3. Check data structure of every variables

str(Stress)
glimpse(Stress) #Best

# 4. Check for missing value

is.na(Stress) 
sum(is.na(Stress))

# 4a. Visualize missing data with `naniar`

miss_var_which(Stress)
miss_var_which(Stress)
pct_miss_var(Stress)
gg_miss_var(Stress) # Best
vis_miss(Stress)    # Also very good one

# 5. Check for any duplicate value

duplicated(Stress)
sum(duplicated(Stress))

# 6. Examine first few row

head(Stress, n = 10)

# 6. Examine last few row

tail(Stress, n = 10)

# 7. Summarize data

summary(Stress)

# 8. Modification from character or factor

Stress <- Stress |>
  mutate_if(is.character, as.factor)

glimpse(Stress) # Check data structure after conversion


# 9. Numeric Data
# integer => Frequency / Percentage  ~ Discrete 
# double => Unit / Fraction / Decimal ` ~ Continuous 

# 9a. Mean

Stress$Age

min(Stress$Age)
max(Stress$Age)
Range <- max(Stress$Age) - min(Stress$Age)
Range
mean(Stress$Age)
sd(Stress$Age)

# 9b. Median Way

median(Stress$Age)
quantile(Stress$Age, 0.25)
quantile(Stress$Age, 0.75)
inter_quartile_range <- quantile(Stress$Age, 0.75) - quantile(Stress$Age, 0.25)
inter_quartile_range


# 10. Categorical Data (Factor ~ fct)
# 3 types => Dichotomous, Nominal, and Ordinal

summary(Stress$Gender)
table(Stress$Gender) # Easy way # Count
prop.table(table(Stress$Gender)) #Frequency fraction
percnt <- (prop.table(table(Stress$Gender))*100) #Frequency Percent

percnt


# 11. Converting Ordinal Categorical Values to Numeric in R
# 2 ways for this Recoding
# `Categorical` to `Numeric Data` 

# 11a. Mutate the Variable => mutate only

names(Stress)

Stress <- Stress |>
  mutate(Tired_Score = as.numeric(Tired),
         Nervous_Score = as.numeric(Nervous),
         Calm_Down_Score = as.numeric(Calm_Down),
         Hopeless_Score = as.numeric(Hopeless),
         Restless_Score = as.numeric(Restless),
         Sit_Still_Score = as.numeric(Sit_Still),
         Depressed_Score = as.numeric(Depressed),
         Effort_Score = as.numeric(Effort),
         Cheer_Up_Score = as.numeric(Cheer_Up),
         Worthless_Score = as.numeric(Worthless),
         )

Stress

# 11b. Custom mapping => mutate (case_when) => Best for Grouping

Stress |>
  mutate(Tired_Score = case_when(
    Tired == "None of the time" ~ "1",
    Tired == "A little of the time" ~ "2",
    Tired == "Some of the time" ~ "3",
    Tired == "Most of the time" ~ "4",
    Tired == "All of the time" ~ "5"
    )) # Repeat for Every Variables => Tiresome


# 12. Transforming Data into New Categorical Variable
# Addition or Summation

names(Stress)

Stress <- Stress |>
  mutate( Perceived_stress_score = Tired_Score + 
            Nervous_Score +
            Calm_Down_Score +
            Hopeless_Score +
            Restless_Score +
            Sit_Still_Score +
            Depressed_Score  +
            Effort_Score +
            Cheer_Up_Score +
            Worthless_Score)

Stress

# 13. Categorization => `Numeric Data` to `Categorical`

names(Stress)

Stress <- Stress |>
  mutate(Perceived_Stress = case_when(
    Perceived_stress_score < 20 ~ "No Stress",
    Perceived_stress_score >= 20 & Perceived_stress_score <= 24 ~ "Mild Stress",
    Perceived_stress_score >= 24 & Perceived_stress_score <= 29 ~ "Moderate Stress",
    Perceived_stress_score >= 30 ~ "Severe Stress"
  ))

Stress


# 13a. Categorization => `Numeric Data` to `Dichotomous Categorical`

names(Stress)

Stress <- Stress |>
  mutate(Presence_of_Stress = case_when(
    Perceived_stress_score <= 29 ~ "No",
    Perceived_stress_score >= 30 ~ "Yes"
  ))

Stress

# 14.  Final Modification from character or factor

Stress <- Stress |>
  mutate_if(is.character, as.factor)

glimpse(Stress)

# 15. Publication Ready Descriptive Tables => Demographic characteristics

# 15a. Very Basic One

names(Stress)

Stress |>
  select(Gender) |>
  tbl_summary()

# 15b. Table 1. Demographic characteristics of the study participants (N=436)

names(Stress)

Stress |> # Median 
  select(2, 3, 5, 6, 7, 8) |> # Column Number
  tbl_summary() |>
  as_gt() |>
  gtsave("tables/Table1.docx")


# Recreate with (statistic = "Age" ~ "{mean} ± ({sd})")
# 15c. Table 1a. Demographic characteristics of the study participants (N=436)

names(Stress)

Stress |>
  select(2, 3, 5, 6, 7, 8) |> 
  tbl_summary(statistic = "Age" ~ "{mean} ± ({sd})") |>
  as_gt() |>
  gtsave("tables/Table1a.docx")


# Recreate with (statistic = "Age" ~ "{mean} ± ({sd})")
# 15d. Table 1b. Demographic characteristics of the study participants (N=436)

names(Stress)

Stress |>
  select(2, 3, 5, 6, 7, 8) |> 
  tbl_summary(statistic = list(all_categorical() ~ "{n} ({p}%)")) |>
  as_gt() |>
  gtsave("tables/Table1b.docx")

# Recreate with (statistic = "Age" ~ "{mean} ± ({sd})",type = all_categorical() ~ "categorical")
# 15e. Table 1c. Demographic characteristics of the study participants (N=436)

names(Stress)

Stress |> # Best One
  select(2, 3, 5, 6, 7, 8) |> 
  tbl_summary(statistic = "Age" ~ "{mean} ± ({sd})",
              type = all_categorical() ~ "categorical") |>
  as_gt() |>
  gtsave("tables/Table1c.docx")


# 16. Publication Ready Descriptive Tables => Stress Levels and Health Concerns
# Table 2. Stress Levels and Health Concerns among the study participants (N=436)

names(Stress)

Stress |>
  select(Perceived_Stress, 25:27) |>
  tbl_summary(type = all_categorical() ~ "categorical") |>
  as_gt() |>                 
  gtsave("tables/Table2.docx")


# 16. Publication Ready Descriptive Tables => Association between Stress and Study Variables 
# Table 3. Association between Stress and Study Variables among participants Education Level (N=436)

names(Stress)

Stress |>
  select(Perceived_Stress, 3, 5, 6, 8) |>
  tbl_summary(by = Perceived_Stress,
              type = all_categorical() ~ "categorical") |>
  add_overall() |>
 # add_p(test.args = all_tests("fisher.test") ~ list(workspace = 2e9)) |>
  bold_p(t = 0.05) |>
  as_gt() |>                 
  gtsave("tables/Table3.docx")


# 17. Publication Ready Descriptive Tables => Association between Stress and Risk Factors 
# Table 4. Association between Stress and Risk Factors among participants Education Level (N=436)

names(Stress)

Stress |>
  select(Perceived_Stress, 19:25) |>
  tbl_summary(by = Perceived_Stress,
              type = all_categorical() ~ "categorical") |>
  add_overall() |>
  add_p() |>
  bold_p(t = 0.05) |>
  as_gt() |>                 
  gtsave("tables/Table4.docx")



# 18. Publication Ready Descriptive Tables => Independence of Risk Factors of Stress 
# Table 5. Independence of Risk Factors of Stress among participants Education Level (N=436)
# Multivariate Logistic Regression

names(Stress)

Stress |>
  select(Gender, Study_Year, CGPA, Attendance, 
         Academic_Workload, Financial_Concerns, 
         Relationship, Family_Responsibilities,
         Assignment_Deadline, Health_Concerns,
         Medical_Illnesses, Perceived_Stress) |>
  tbl_uvregression(
    method = glm, 
    y = Perceived_Stress,
    method.args = list(family = binomial), # Linear => `gaussian`, # Logistic => `binomial`
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 2)) |>
  bold_p(t = 0.05) |> # Bold p-values under a given threshold (Default 0.05)
  as_gt() |>                 
  gtsave("tables/Table5.docx")



# 19. Publication Ready Descriptive Tables => Association between Stress and Study Variables (Dichotomous)
# Dichotomous Category
# Table 3D. Association between Stress and Study Variables among participants Education Level (N=436)

names(Stress)

Stress |>
  select(Presence_of_Stress, 3, 5, 6, 8) |>
  tbl_summary(by = Presence_of_Stress,
              type = all_categorical() ~ "categorical") |>
  add_overall() |>
 # add_p(test.args = all_tests("fisher.test") ~ list(workspace = 2e9)) |>
  bold_p(t = 0.05) |>
  as_gt() |>                 
  gtsave("tables/Table3D.docx")


# 20. Publication Ready Descriptive Tables => Association between Stress and Risk Factors (Dichotomous)
# Dichotomous Category
# Table 4D. Association between Stress and Risk Factors among participants Education Level (N=436)

names(Stress)

Stress |>
  select(Presence_of_Stress, 19:25) |>
  tbl_summary(by = Presence_of_Stress,
              type = all_categorical() ~ "categorical") |>
  add_overall() |>
  add_p() |>
  bold_p(t = 0.05) |>
  as_gt() |>                 
  gtsave("tables/Table4D.docx")



# 21. Publication Ready Descriptive Tables => Independence of Risk Factors of Stress (Dichotomous)
# Table 5D. Independence of Risk Factors of Stress among participants Education Level (N=436)
# Multivariate Logistic Regression
# Dichotomous Category

names(Stress)

Stress |>
  select(Gender, Study_Year, CGPA, Attendance, 
         Academic_Workload, Financial_Concerns, 
         Relationship, Family_Responsibilities,
         Assignment_Deadline, Health_Concerns,
         Medical_Illnesses, Presence_of_Stress) |>
  tbl_uvregression(
    method = glm, 
    y = Presence_of_Stress,
    method.args = list(family = binomial), # Linear => `gaussian`, # Logistic => `binomial`
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 2)) |>
  bold_p(t = 0.05) |> # Bold p-values under a given threshold (Default 0.05)
  as_gt() |>                 
  gtsave("tables/Table5D.docx")



# Data Visualization

# 22. Distribution of Perceived Stress Levels

names(Stress)

Stress1 <- Stress |>
  group_by(Perceived_Stress) |>
  summarize(AvgStress = round(mean(Perceived_stress_score), 2))

Stress1

ggpie(Stress1, "AvgStress","Perceived_Stress",
      fill = "Perceived_Stress",
      lab.pos = c("in"),
      lab.font = c(4, "plain", "black"),
      legend = "bottom",
      legend.title = "Perceived Stress",
      font.legend = c(10, "plain", "black"),
      ticks = T,
      main = "Distribution of Perceived Stress Levels") 

ggsave("figures/Distribution of Perceived Stress Levels.png", 
       units = "in", width = 10, height = 10, dpi = 1200)


# 23. Distribution of Perceived Stress Levels by Gender

names(Stress)

Stress2 <- Stress |>
  group_by(Perceived_Stress) |>
  count(Gender)

Stress2

ggplot(Stress2,
       aes(x = Perceived_Stress, y = n, 
           fill = Gender)) +
  geom_col() +
  scale_color_viridis_c() +
  facet_wrap(vars(Gender), nrow = 1) +
  labs(x = "Perceived Stress",          
       y = "Frequency", 
       title = "Distribution of Stress Levels by Gender",
       caption = "Data Source: {Stress in Focus: A Cross-Sectional Study on University Students' Perceived Stress Levels}") + 
  theme_bw() +
  theme(plot.title = element_text(size = 16,
                                  hjust = 0.5),
        legend.position = "top")

ggsave("figures/Distribution of Perceived Stress Levels by Gender.png", 
       units = "in", width = 10, height = 10, dpi = 1200)



# 24. Distribution of Perceived Stress Levels in Each Year of Study

names(Stress)

Stress3 <- Stress |>
  group_by(Perceived_Stress) |>
  count(Study_Year)

Stress3

ggplot(Stress3,
       aes(x = Perceived_Stress, y = n, 
           fill = Study_Year)) +
  geom_col() +
  scale_color_viridis_c() +
  scale_fill_discrete(name = "Year of Study") +
  facet_wrap(vars(Study_Year), nrow = 2) +
  labs(x = "Perceived Stress",          
       y = "Frequency", 
       title = "Distribution of Perceived Stress Levels in Each Year of Study",
       caption = "Data Source: {Stress in Focus: A Cross-Sectional Study on University Students' Perceived Stress Levels}") + 
  theme_bw() +
  theme(plot.title = element_text(size = 16,
                                  hjust = 0.5),
        legend.position = "top")

ggsave("figures/Distribution of Perceived Stress Levels in Each Year of Study.png", 
       units = "in", width = 10, height = 10, dpi = 1200)


# 25. Distribution of Perceived Stress Levels among Ages with Mean Scores


names(Stress)

Stress4 <- Stress |>
  group_by(Perceived_Stress) |>
  count(Age)

Stress4

ggplot(Stress4,
       aes(x = Age, y = n, 
           fill = Perceived_Stress)) +
  geom_col() +
  scale_color_viridis_d() +
  scale_fill_discrete(name = "Perceived Stress") +
  facet_wrap(vars(Perceived_Stress), ncol = 2) +
  labs(x = "Age",          
       y = "Frequency", 
       title = "Distribution of Perceived Stress Levels among Ages",
       subtitle = "Distribution of Stress with Mean Perceived Stress Scores",
       caption = "Data Source: {Stress in Focus: A Cross-Sectional Study on University Students' Perceived Stress Levels}") +
  geom_hline(yintercept = mean(Stress$Perceived_stress_score), linetype = 4) + 
  theme_bw() +
  theme(plot.title = element_text(size = 16,
                                  hjust = 0.5),
        legend.position = "top")

ggsave("figures/Distribution of Perceived Stress Levels among Ages with Mean Scores.png", 
       units = "in", width = 10, height = 10, dpi = 1200)


# 26. Distribution of CGPA and Perceived Stress Levels 


names(Stress)

Stress5 <- Stress |>
  group_by(Presence_of_Stress) |>
  count(CGPA)

Stress5

ggplot(Stress5,
       aes(x = CGPA, y = n, 
           fill = Presence_of_Stress)) +
  geom_col() +
  scale_color_viridis_d() +
  scale_fill_discrete(name = "Presence of Stress") +
  facet_wrap(vars(Presence_of_Stress), ncol = 2) +
  labs(x = "CGPA",          
       y = "Frequency", 
       title = "Distribution of Perceived Stress Levels and CGPA",
       caption = "Data Source: {Stress in Focus: A Cross-Sectional Study on University Students' Perceived Stress Levels}") +
  theme_bw() +
  theme(plot.title = element_text(size = 16,
                                  hjust = 0.5),
        legend.position = "top")

ggsave("figures/Distribution of Perceived Stress Levels and CGPA.png", 
       units = "in", width = 10, height = 10, dpi = 1200)


# 27. Density Plot of Perceived Stress Scores

ggplot(Stress,
       aes(x = Perceived_stress_score, 
           fill = Perceived_Stress)) +
  geom_density() +
  scale_color_viridis_d() +
  scale_fill_discrete(name = "Perceived Stress") +
  facet_wrap(vars(Perceived_Stress), ncol = 2) +
  labs(x = "Perceived Stress Score",          
       y = "Density", 
       title = "Density Plot of Perceived Stress Scores",
       caption = "Data Source: {Stress in Focus: A Cross-Sectional Study on University Students' Perceived Stress Levels}") +
  theme_bw() +
  theme(plot.title = element_text(size = 16,
                                  hjust = 0.5),
        legend.position = "top")

ggsave("figures/Density Plot of Perceived Stress Scores.png", 
       units = "in", width = 10, height = 10, dpi = 1200)


# 28. Distribution of Perceived Stress Scores by Gender

names(Stress)

ggplot(Stress,
       aes(x = Gender, y = Perceived_stress_score, 
           fill = Gender)) +
  geom_boxplot() +
  geom_jitter() +
  scale_color_viridis_d() +
  scale_fill_discrete(name = "Gender") +
  labs(x = "Gender",          
       y = "Perceived Stress Score", 
       title = "Distribution of Perceived Stress Scores by Gender",
       caption = "Data Source: {Stress in Focus: A Cross-Sectional Study on University Students' Perceived Stress Levels}",
       tag = "A") +
  geom_hline(yintercept = mean(Stress$Perceived_stress_score), linetype = 2) + 
  stat_compare_means(label.x = 1.4, method = "t.test") +
  theme_bw() +
  theme(plot.title = element_text(size = 16,
                                  hjust = 0.5),
        legend.position = "top")

ggsave("figures/Distribution of Perceived Stress Scores by Gender.png", 
       units = "in", width = 10, height = 10, dpi = 1200)


# 29. Distribution of Perceived Stress Scores by Attendance

names(Stress)

ggplot(Stress,
       aes(x = Attendance, y = Perceived_stress_score, 
           fill = Attendance)) +
  geom_boxplot() +
  geom_jitter() +
  scale_color_viridis_d() +
  scale_fill_discrete(name = "Attendance") +
  labs(x = "Attendance",          
       y = "Perceived Stress Score", 
       title = "Distribution of Perceived Stress Scores by Attendance",
       caption = "Data Source: {Stress in Focus: A Cross-Sectional Study on University Students' Perceived Stress Levels}",
       tag = "A") +
  geom_hline(yintercept = mean(Stress$Perceived_stress_score), linetype = 2) + 
  stat_compare_means(label.x = 1.4, method = "t.test") +
  theme_bw() +
  theme(plot.title = element_text(size = 16,
                                  hjust = 0.5),
        legend.position = "top")

ggsave("figures/Distribution of Perceived Stress Scores by Attendance.png", 
       units = "in", width = 10, height = 10, dpi = 1200)



# 30. Distribution of Perceived Stress Scores among Year of Study

names(Stress)

p1 <- ggplot(Stress,
       aes(x = Study_Year, y = Perceived_stress_score, 
           fill = Study_Year)) +
  geom_boxplot() +
  geom_jitter() +
  scale_color_viridis_d() +
  scale_fill_discrete(name = "Year of Study") +
  labs(x = "Year of Study",          
       y = "Perceived Stress Score", 
       title = "Distribution of Perceived Stress Scores among Year of Study",
       caption = "Data Source: {Stress in Focus: A Cross-Sectional Study on University Students' Perceived Stress Levels}",
       tag = "A") +
  geom_hline(yintercept = mean(Stress$Perceived_stress_score), linetype = 2) + 
  stat_compare_means(label.x = 2.5, method = "anova") +
  theme_bw() +
  theme(plot.title = element_text(size = 16,
                                  hjust = 0.5),
        legend.position = "top")

p1

ggsave("figures/Distribution of Perceived Stress Scores among Year of Study.png", 
       units = "in", width = 10, height = 10, dpi = 1200)

# 31. Distribution of Perceived Stress Scores among CGPA

names(Stress)

p2 <- ggplot(Stress,
       aes(x = CGPA, y = Perceived_stress_score, 
           fill = CGPA)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_color_viridis_d() +
  scale_fill_discrete(name = "CGPA") +
  labs(x = "CGPA",          
       y = "Perceived Stress Score", 
       title = "Distribution of Perceived Stress Scores among CGPA",
       caption = "Data Source: {Stress in Focus: A Cross-Sectional Study on University Students' Perceived Stress Levels}",
       tag = "B") +
  geom_hline(yintercept = mean(Stress$Perceived_stress_score), linetype = 2) + 
  stat_compare_means(label.x = 2.5, method = "anova") +
  theme_bw() +
  theme(plot.title = element_text(size = 16,
                                  hjust = 0.5),
        legend.position = "top")

p2

ggsave("figures/Distribution of Perceived Stress Scores among CGPA.png", 
       units = "in", width = 10, height = 10, dpi = 1200)

# Now, Make Grid

comboPlot <- cowplot::plot_grid(p1, p2, nrow = 1, labels = c("A", "B"))
comboPlot

#Final Export

ggsave2("figures/Study of Year_CGPA_Combined.png", 
        units = "in", width = 15, height = 10, dpi = 1200)


# 32. Correlation Heatmap: Stress vs Risk Factor

names(Stress)
glimpse(Stress)

# Factor to Numeric

Stress <- Stress |>
  mutate(Academic_Workload = as.numeric(Academic_Workload),
         Financial_Concerns = as.numeric(Financial_Concerns),
         Relationship = as.numeric(Relationship),
         Family_Responsibilities = as.numeric(Family_Responsibilities),
         Assignment_Deadline = as.numeric(Assignment_Deadline),
         Health_Concerns = as.numeric(Health_Concerns),
         Medical_Illnesses = as.numeric(Medical_Illnesses),
         Perceived_Stress = as.numeric(Perceived_Stress),
         Presence_of_Stress = as.numeric(Presence_of_Stress)
  )

glimpse(Stress)


Stress6 <- Stress |>
  select(Academic_Workload, Financial_Concerns,
         Relationship, Family_Responsibilities, 
         Assignment_Deadline, Health_Concerns,
         Medical_Illnesses, Presence_of_Stress) 

Stress6
glimpse(Stress6)

Stress6cor <- cor(Stress6, use = "all.obs")

Stress6cor_df <-
  as.data.frame(Stress6cor) |>
  rownames_to_column("Stress_F") |>
  pivot_longer(-Stress_F, 
               names_to = "Risk_Factor", 
               values_to = "Correlation")

ggplot(Stress6cor_df,
       aes(x = Risk_Factor, y = Stress_F, 
           fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "#000000",
                       mid = "#b2182b",
                       high = "#003c30",
                       name = "Correlation",
                       midpoint = 0,
                       limit = c(-1, 1)) +
  labs(x = "Risk Factors",          
       y = "Stress", 
       title = "Correlation Heatmap: Stress vs Risk Factor",
       caption = "Data Source: {Stress in Focus: A Cross-Sectional Study on University Students' Perceived Stress Levels}",
       tag = "A") +
  theme_bw() +
  theme(plot.title = element_text(size = 16,
                                  hjust = 0.5)
        )

ggsave2("figures/Correlation Heatmap_Stress vs Risk Factor.png", 
        units = "in", width = 20, height = 10, dpi = 1200)



# 32a. Correlation Heatmap: Stress vs Risk Factor

names(Stress)
glimpse(Stress)

# Factor to Numeric

Stress <- Stress |>
  mutate(Academic_Workload = as.numeric(Academic_Workload),
         Financial_Concerns = as.numeric(Financial_Concerns),
         Relationship = as.numeric(Relationship),
         Family_Responsibilities = as.numeric(Family_Responsibilities),
         Assignment_Deadline = as.numeric(Assignment_Deadline),
         Health_Concerns = as.numeric(Health_Concerns),
         Medical_Illnesses = as.numeric(Medical_Illnesses),
         Perceived_Stress = as.numeric(Perceived_Stress),
         Presence_of_Stress = as.numeric(Presence_of_Stress)
  )

glimpse(Stress)


Stress7 <- Stress |>
  select(Academic_Workload, Financial_Concerns,
         Relationship, Family_Responsibilities, 
         Assignment_Deadline, Health_Concerns,
         Medical_Illnesses, Perceived_Stress) 

Stress6
glimpse(Stress7)

Stress7cor <- cor(Stress7, use = "all.obs")

Stress7cor_df <-
  as.data.frame(Stress7cor) |>
  rownames_to_column("Stress_F") |>
  pivot_longer(-Stress_F, 
               names_to = "Risk_Factor", 
               values_to = "Correlation")

ggplot(Stress7cor_df,
       aes(x = Risk_Factor, y = Stress_F, 
           fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "#dfd8d2",
                       mid = "#00544d",
                       high = "#5f555a",
                       name = "Correlation",
                       midpoint = 0,
                       limit = c(-1, 1)) +
  labs(x = "Risk Factors",          
       y = "Stress", 
       title = "Correlation Heatmap: Stress vs Risk Factor",
       caption = "Data Source: {Stress in Focus: A Cross-Sectional Study on University Students' Perceived Stress Levels}",
       tag = "B") +
  theme_bw() +
  theme(plot.title = element_text(size = 16,
                                  hjust = 0.5))

ggsave2("figures/Correlation Heatmap Stress vs Risk Factor.png", 
        units = "in", width = 20, height = 12, dpi = 1200)


#===============================================================


# 33.  Independence of Risk Factors of Stress among participants Education Level
# in case of => Presence_of_Stress

names(Stress)
glimpse(Stress)

# Predictor => Factor to Binary

Stress <- Stress |>
  mutate(
    has_Stress = ifelse(Presence_of_Stress == "No", 0,1)
  )

# Fit the model

model1 <-  glm(Presence_of_Stress ~ Gender + Study_Year + CGPA + Attendance +
           Academic_Workload + Financial_Concerns +
         Relationship + Family_Responsibilities + 
         Assignment_Deadline + Health_Concerns +
         Medical_Illnesses,
         data = Stress,
         family = "binomial")

summary(model1)

# Calculate odds ratio

tidymodel1 <- tidy(model1, conf.int = TRUE,
                   exponentiate = TRUE)

tidymodel1

# Visualization with Forest Plot 

ggplot(data = tidymodel1 |> 
         filter(term != "(Intercept)"),
         mapping = aes (x = estimate, 
                        y = term)) +  
  geom_point(size = 3, color = "#00544d") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(xmin = conf.low,
                     xmax = conf.high),
                 width = 0.25) +
  labs(x = "Odds Ratio (Likelihood increase)",          
       y = "Predictor", 
       title = "Independence of Risk Factors of Stress among participants Education Level",
       subtitle = "Multiple Logistic Regression Results",
       caption = "Data Source: {Stress in Focus: A Cross-Sectional Study on University Students' Perceived Stress Levels}") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12,
                                     face = "bold",
                                  hjust = 0.5),
        legend.position = "top")

ggsave("figures/Independence of Risk Factors of Stress among participants Education Level.png", 
       units = "in", width = 10, height = 10, dpi = 1200)


# 33a.  Independence of Risk Factors of Stress among participants Education Level
# in case of => Perceived_Stress

names(Stress)
glimpse(Stress)

# Predictor => Factor to Binary

Stress <- Stress |>
  mutate(
    has_Stress = ifelse(Perceived_Stress == "No", 0,1)
  )

# Fit the model

model2 <-  glm(Perceived_Stress ~ Gender + Study_Year + CGPA + Attendance +
                 Academic_Workload + Financial_Concerns +
                 Relationship + Family_Responsibilities + 
                 Assignment_Deadline + Health_Concerns +
                 Medical_Illnesses,
               data = Stress,
               family = "binomial")

summary(model2)

# Calculate odds ratio

tidymodel2 <- tidy(model2, conf.int = TRUE,
                   exponentiate = TRUE)

tidymodel2

# Visualization with Forest Plot 

ggplot(data = tidymodel2 |> 
         filter(term != "(Intercept)"),
       mapping = aes (x = estimate, 
                      y = term)) +  
  geom_point(size = 3, color = "#00544d") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(xmin = conf.low,
                    xmax = conf.high),
                width = 0.25) +
  labs(x = "Odds Ratio (Likelihood increase)",          
       y = "Predictor", 
       title = "Independence of Risk Factors of Stress among participants Education Level",
       subtitle = "Multiple Logistic Regression Results",
       caption = "Data Source: {Stress in Focus: A Cross-Sectional Study on University Students' Perceived Stress Levels}") +
  theme_bw() +
  theme(plot.title = element_text(size = 16,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12,
                                     face = "bold",
                                     hjust = 0.5),
        legend.position = "top")

ggsave("figures/Independence of Risk Factors of Stress among participants Education Level(a).png", 
       units = "in", width = 10, height = 10, dpi = 1200)


# Visualization with Forest Plot but xmax => limited to a fixed level

ggplot(data = tidymodel2 |> 
         filter(term != "(Intercept)"),
       mapping = aes (x = estimate, 
                      y = term)) +  
  geom_point(size = 3, color = "#00544d") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(xmin = conf.low,
                    xmax = 5), # Usually, xmax = conf.high
                width = 0.25) +
  labs(x = "Odds Ratio (Likelihood increase)",          
       y = "Predictor", 
       title = "Independence of Risk Factors of Stress among participants Education Level",
       subtitle = "Multiple Logistic Regression Results",
       caption = "Data Source: {Stress in Focus: A Cross-Sectional Study on University Students' Perceived Stress Levels}") +
  theme_bw() +
  theme(plot.title = element_text(size = 16,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12,
                                     face = "bold",
                                     hjust = 0.5),
        legend.position = "top")

ggsave("figures/Independence of Risk Factors of Stress among participants Education Level(b).png", 
       units = "in", width = 10, height = 10, dpi = 1200)



#===================================================================



# 34. Multiple Logistic Regression Model Prediction Probability
# in case of => Presence_of_Stress

names(Stress)
glimpse(Stress)

# Predictor => Factor to Binary

Stress <- Stress |>
  mutate(
    has_Stress = ifelse(Presence_of_Stress == "No", 0,1)
  )

# Fit the model

model3 <-  glm(Presence_of_Stress ~ Age + Gender + Study_Year +
                 Academic_Workload + Financial_Concerns +
                 Assignment_Deadline + Health_Concerns +
                 Medical_Illnesses,
               data = Stress,
               family = "binomial")

summary(model3)

# Calculate odds ratio

tidymodel3 <- tidy(model3, conf.int = TRUE,
                   exponentiate = TRUE)

tidymodel3

# Visualization with Predicted Probabilities
# A grid for hypothetical calculation of prediction

hypodata <- expand_grid(
  Age = seq(18, 30, by = 0.1),
  Gender = "Male",
  Study_Year = "Third year",
  Academic_Workload = c("Low", "High"), # Spelling have to be same
  Financial_Concerns = c("High"),
  Assignment_Deadline = c("Moderate"),
  Health_Concerns = c("Low"),
  Medical_Illnesses = c("No")
)

# Now, Predict the Probabilities

predictdata <- augment(model3, 
                       newdata = hypodata, 
                       type.predict = "response")


ggplot(data = predictdata, 
       mapping = aes (x = Age, 
                      y = .fitted,
                      color = Academic_Workload)) +  
  geom_point(data = Stress |>
               filter(Gender == "Male"),
             aes(x = Age, y = has_Stress),
             size =3,
             alpha = 0.4) +
  geom_line(size = 2) +
  scale_fill_discrete(name = "Academic Workload") +
  labs(x = "Age (Years)",          
       y = "Predicted Probability (0 to 1)", 
       title = "Multiple Logistic Regression Model Prediction Probability",
       subtitle = "Comparing High vs Low Concerns across Ages",
       caption = "Dots = Real Student Data | Lines = Model Prediction",
       color = "Academic Workload") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12,
                                     face = "bold",
                                     hjust = 0.5),
        legend.position = "top")

ggsave("figures/Multiple Logistic Regression Model Prediction Probability.png", 
       units = "in", width = 10, height = 10, dpi = 1200)


# 34. Multiple Logistic Regression Model Prediction Probability
# in case of => Perceived_Stress

names(Stress)
glimpse(Stress)

# Predictor => Factor to Binary

Stress <- Stress |>
  mutate(
    has_Stress = ifelse(Perceived_Stress == "No", 0,1)
  )

# Fit the model

model4 <-  glm(Perceived_Stress ~ Age + Gender + Study_Year +
                 Academic_Workload + Financial_Concerns +
                 Assignment_Deadline + Health_Concerns +
                 Medical_Illnesses,
               data = Stress,
               family = "binomial")

summary(model4)

# Calculate odds ratio

tidymodel4 <- tidy(model4, conf.int = TRUE,
                   exponentiate = TRUE)

tidymodel4

# Visualization with Predicted Probabilities
# A grid for hypothetical calculation of prediction

hypodata <- expand_grid(
  Age = seq(18,30, by = 0.1),
  Gender = "Female",
  Study_Year = "Fourth year",
  Academic_Workload = c("Low"), # Spelling have to be same
  Financial_Concerns = c("Low", "High"),
  Assignment_Deadline = c("Moderate"),
  Health_Concerns = c("Low"),
  Medical_Illnesses = c("No")
)

# Now, Predict the Probabilities

predictdata <- augment(model4, 
                       newdata = hypodata, 
                       type.predict = "response")


ggplot(data = predictdata, 
       mapping = aes (x = Age, 
                      y = .fitted,
                      color = Financial_Concerns)) +  
  geom_point(data = Stress |>
               filter(Gender == "Male"),
             aes(x = Age, y = has_Stress),
             size =3,
             alpha = 0.4) +
  geom_line(size = 2) +
  scale_fill_discrete(name = "Financial Concerns") +
  labs(x = "Age (Years)",          
       y = "Predicted Probability (0 to 1)", 
       title = "Multiple Logistic Regression Model Prediction Probability (Female)",
       subtitle = "Comparing High vs Low Financial Concerns across Ages",
       caption = "Dots = Real Student Data | Lines = Model Prediction",
       color = "Financial Concerns") +
  theme_bw() +
  theme(plot.title = element_text(size = 16,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12,
                                     face = "bold",
                                     hjust = 0.5),
        legend.position = "top")

ggsave("figures/Multiple Logistic Regression Model Prediction Probability (Female).png", 
       units = "in", width = 10, height = 10, dpi = 1200)



#======================================================================

write.csv(Stress, "data/Stress40variables.csv")
