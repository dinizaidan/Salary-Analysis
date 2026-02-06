# Install packages
install.packages(c("dplyr","stringr"))
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("rcompanion")

# Import library
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(forcats)
library(rcompanion)
library(knitr)

# Import Data
DATA <- read_excel("C:/Users/User-PC/Desktop/Master of Data Science/SMT 1/Statistical Method/DATA.xlsx", 
                    sheet = "Sheet1", col_types = c("skip", 
                                                    "text", "skip", "skip", "numeric", 
                                                    "skip", "skip", "text", "skip"))
View(DATA)
str(DATA)


# ------- PREPROCESSING -------
# Check Missing Value
anyNA(DATA)
sum(is.na(DATA))
colSums(is.na(DATA))           # Look at which column that has missing value
which(!complete.cases(DATA))   # Find the row that has missing value

DATA <- DATA %>%
  drop_na()            # Remove rows with missing value
summary(DATA)


# ------- PREPROCESSING (Industry) -------
length(unique(DATA$Industry))    # Checking how many categories we currently have
unique(DATA$Industry)

# Standardize spacing, punctuation & lowercase
DATA <- DATA %>%
  mutate(
    Industry = Industry %>%
      str_squish() %>%                          # remove extra spaces
      str_replace_all("[[:punct:]]", " ") %>%   # replace punctuation with spaces
      str_squish() %>%                          
      str_to_lower()
  )

# Clean Industry categories into 11 categories
DATA <- DATA %>%
  mutate(
    Industry = case_when(
      
      # 1. TECHNOLOGY / COMPUTING
      str_detect(Industry, "tech|comput|software|it|saas|cyber|data|digital|internet|game|ux|programming") ~ "Technology",
      
      # 2. HEALTHCARE / MEDICAL / BIOTECH / PHARMA
      str_detect(Industry, "health|medical|pharma|biotech|life science|clinical|veterinary|biomed|diagnostic") ~ "Health & Biotech",
      
      # 3. EDUCATION / ACADEMIA / LIBRARIES
      str_detect(Industry, "educat|school|university|academy|library|librar|research (academic)|student|tutor") ~ "Education & Academia",
      
      # 4. SCIENCE & RESEARCH (NON‑MEDICAL)
      str_detect(Industry, "science|r d|laboratory|chemistry|biology|analysis|geology|ecology|environmental science") ~ "Science & Research",
      
      # 5. GOVERNMENT / PUBLIC SERVICE / DEFENSE
      str_detect(Industry, "government|public|municipal|federal|policy|defense|military|contractor|lobbying") ~ "Government & Public Service",
      
      # 6. BUSINESS / CONSULTING / FINANCE / HR
      str_detect(Industry, "business|consult|finance|bank|account|recruit|hr|investment|real estate|property") ~ "Business & Finance",
      
      # 7. MANUFACTURING / ENGINEERING / ENERGY / CONSTRUCTION
      str_detect(Industry, "manufactur|engineer|construction|energy|mining|oil|gas|automotive|production|supply chain") ~ "Engineering & Manufacturing",
      
      # 8. CREATIVE / MEDIA / ARTS / MARKETING
      str_detect(Industry, "art|design|creative|media|journal|marketing|advertis|publishing|film|entertain|fashion") ~ "Creative & Media",
      
      # 9. NONPROFIT / SOCIAL WORK / COMMUNITY SERVICES
      str_detect(Industry, "nonprofit|charity|community|social work|foundation|religion|church|caregiver") ~ "Nonprofit & Social Services",
      
      # 10. RETAIL / HOSPITALITY / FOOD / SERVICE JOBS
      str_detect(Industry, "retail|hospitality|restaurant|food|beverage|tourism|service|warehouse|logistics") ~ "Retail & Service",
      
      TRUE ~ "Others"
    )
  )
unique(DATA$Industry)


# ----------    DESCRIPTIVE ANALYSIS     ----------

# ----- Annual Salary (numeric) -----
# Summary statistics
salary_summary <- DATA %>%
  summarise(
    count = n(),
    min = min(AnnualSalaryUSD, na.rm = TRUE),
    max = max(AnnualSalaryUSD, na.rm = TRUE),
    mean = mean(AnnualSalaryUSD, na.rm = TRUE),
    sd = sd(AnnualSalaryUSD, na.rm = TRUE),
    median = median(AnnualSalaryUSD, na.rm = TRUE),
    IQR = IQR(AnnualSalaryUSD, na.rm = TRUE)
  )
kable(salary_summary, caption = "Summary of Annual Salary")

DATA_clean <- DATA %>% filter(AnnualSalaryUSD > 0 & AnnualSalaryUSD < 1e6)
print(
  ggplot(DATA, aes(x = AnnualSalaryUSD)) +
    geom_density(fill = "violetred2", alpha = 0.5) +
    scale_x_log10(labels = scales::comma) +
    labs(title = "Annual Salary Density (log scale)",
         x = "Annual Salary (USD, log scale)", y = "Density") +
    theme_minimal()
)


# ----- Industry (categorical) -----
# Counts salary per industry
sort(table(DATA$Industry), decreasing = TRUE)

# Bar chart of counts 
ggplot(DATA, aes(x = fct_rev(fct_infreq(Industry)))) +
  geom_bar(fill = "violetred2", width = 0.8) +
  labs(
    title = "Distribution of Respondents by Industry",
    x = "Industry",
    y = "Count"
  ) +
  theme_minimal() +
  coord_flip()

# Boxplot of salary by industry
ggplot(DATA, aes(x = Industry, y = AnnualSalaryUSD)) +
  geom_boxplot(fill = "mistyrose", outlier.alpha = 0.2, outlier.size = 0.8) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Annual Salary Distribution by Industry",
    x = "Industry",
    y = "USD Annual Salary (log scale)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# ----- Years of Experience (categorical) -----
# Counts salary per Level of Experience
sort(table(DATA$YearsExperienceOverall), decreasing = TRUE)

# Bar chart of counts 
DATA$YearsExperienceOverall <- factor(
  DATA$YearsExperienceOverall,
  levels = c("1 year or less",
             "2 - 4 years",
             "5-7 years",
             "8 - 10 years",
             "11 - 20 years",
             "21 - 30 years",
             "31 - 40 years",
             "41 years or more")
)
ggplot(DATA, aes(x = YearsExperienceOverall)) +
  geom_bar(fill = "violetred2", width = 0.8) +
  labs(
    title = "Distribution of Respondents by Level of Experience",
    x = "Years of Experience",
    y = "Count"
  ) +
  theme_minimal()

# Boxplot of salary by experience
ggplot(DATA, aes(x = YearsExperienceOverall, y = AnnualSalaryUSD)) +
  geom_boxplot(fill = "mistyrose", outlier.alpha = 0.2, outlier.size = 0.8) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Annual Salary Distribution by Experience Level",
    x = "Years of Experience",
    y = "USD Annual Salary (log scale)"
  ) +
  theme_minimal()





# ----------    INFERENTIAL ANALYSIS     ----------
# RQ1: Do annual salaries differ significantly across service industries?

# Use Kruskal Wallis bc the data is not normally distributed
kruskal.test(AnnualSalaryUSD ~ Industry, data = DATA)

# Use Wilcoxon to see the industry that differs the most
pw <- pairwise.wilcox.test(       # Save the result
  DATA$AnnualSalaryUSD,
  DATA$Industry,
  p.adjust.method = "bonferroni"
)
pw$p.value

pval_df <- as.data.frame(pw$p.value) %>%       
  rownames_to_column("Industry1") %>%
  pivot_longer(
    cols = -Industry1,
    names_to = "Industry2",
    values_to = "p_value"
  ) %>%
  drop_na()
pval_df <- pval_df %>%
  mutate(log_p = -log10(p_value))

ggplot(pval_df, aes(x = Industry1, y = Industry2, fill = log_p)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "mistyrose",
    high = "deeppink3",
    name = "-log10(p)"
  ) +
  labs(
    title = "Pairwise Wilcoxon Test (Bonferroni-adjusted)",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# RQ2: How does salary vary across different levels of professional experience?

# Building the order
experience_order <- c("1 year or less", "2 - 4 years", "5 - 7 years", "8 - 10 years", 
                      "11 - 20 years", "21 - 30 years", "31 - 40 years", "41 years or more")
DATA$YearsExperienceOverall <- factor(DATA$YearsExperienceOverall, 
                                      levels = experience_order, 
                                      ordered = TRUE)

# Use Kruskal Wallis bc the data is not normally distributed
kruskal.test(AnnualSalaryUSD ~ YearsExperienceOverall, data = DATA)

# Use Wilcoxon to see the level of working experience that differs the most
pw_exp <- pairwise.wilcox.test(       # Save the result
  DATA$AnnualSalaryUSD,
  DATA$YearsExperienceOverall,
  p.adjust.method = "bonferroni"
)
pw_exp$p.value

pval_exp_df <- as.data.frame(pw_exp$p.value) %>%
  rownames_to_column("Experience1") %>%
  pivot_longer(
    cols = -Experience1,
    names_to = "Experience2",
    values_to = "p_value"
  ) %>%
  drop_na() %>%
  mutate(
    # Re-apply factor order to BOTH axes
    Experience1 = factor(Experience1, levels = experience_order, ordered = TRUE),
    Experience2 = factor(Experience2, levels = experience_order, ordered = TRUE),
    log_p = -log10(p_value)
  )

ggplot(pval_exp_df, aes(x = Experience1, y = Experience2, fill = log_p)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "mistyrose",
    high = "deeppink3",
    name = "-log10(p)"
  ) +
  labs(
    title = "Pairwise Wilcoxon Test by Experience Level",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# RQ3: Is the industry effect on salary consistent across experience levels?

# Use Scheirer-Ray-Hare (non-parametric two-way ANOVA)
SRH <- scheirerRayHare(AnnualSalaryUSD ~ Industry + YearsExperienceOverall +
                         Industry:YearsExperienceOverall, data = DATA)
SRH

# Interaction plot
interaction_plot <- DATA %>%
  group_by(Industry, YearsExperienceOverall) %>%
  summarise(median_salary = median(AnnualSalaryUSD), .groups = "drop") %>%
  ggplot(aes(x = YearsExperienceOverall, y = median_salary, color = Industry, group = Industry)) +
  geom_line(size = 0.6) +
  geom_point(size = 3) +
  labs(title = "Interaction of Industry and Experience on Salary",
       x = "Experience Level",
       y = "Median Salary") +
  theme_minimal()
interaction_plot