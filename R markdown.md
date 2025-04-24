---
title: "Additive response toward biolubricants"
author: "Noor fatima"
date: "2025-04-21"
output:
     md_document:
       variant: gfm
     html_document:
       toc: true
---


```{r}
library(readxl)
BioLubricant_Additive_PairedData <- read_excel("C:/Users/noork/Desktop/PLPA course/Project design/Additive-response-toward-bio-lubricants-Class-Project/BioLubricant_Additive_PairedData.xlsx")
View(BioLubricant_Additive_PairedData)

```

🔗 [View GitHub Repository](https://github.com/auburn2024/Additive-response-toward-bio-lubricants-Class-Project.git)


-[Rmd_files](Rmd_files/figure-gfm)

-[Markdown_script](Markdown_script/R%20markdown.md)

``` r
fs::dir_tree()
```
├── Additive-response-toward-bio-lubricants-Class-Project.Rproj
├── BioLubricant_Additive_PairedData.xlsx
├── html.html
├── html.Rmd
├── Markdown_script
│   └── R markdown.md
├── R markdown.md
├── Rmd.html
├── Rmd.md
├── Rmd.Rmd
└── Rmd_files
    └── figure-gfm
        ├── unnamed-chunk-10-1.png
        ├── unnamed-chunk-11-1.png
        ├── unnamed-chunk-12-1.png
        ├── unnamed-chunk-13-1.png
        ├── unnamed-chunk-16-1.png
        ├── unnamed-chunk-17-1.png
        ├── unnamed-chunk-22-1.png
        ├── unnamed-chunk-23-1.png
        ├── unnamed-chunk-25-1.png
        ├── unnamed-chunk-25-2.png
        ├── unnamed-chunk-25-3.png
        ├── unnamed-chunk-25-4.png
        ├── unnamed-chunk-26-1.png
        ├── unnamed-chunk-26-2.png
        ├── unnamed-chunk-26-3.png
        ├── unnamed-chunk-26-4.png
        ├── unnamed-chunk-32-1.png
        ├── unnamed-chunk-33-1.png
        ├── unnamed-chunk-35-1.png
        ├── unnamed-chunk-36-1.png
        ├── unnamed-chunk-37-1.png
        ├── unnamed-chunk-38-1.png
        ├── unnamed-chunk-39-1.png
        ├── unnamed-chunk-40-1.png
        ├── unnamed-chunk-41-1.png
        └── unnamed-chunk-42-1.png
```
    


```{r}
library(tidyverse)
library(dplyr)
```



```{r}
Vis.data <- BioLubricant_Additive_PairedData %>%
  mutate(Viscosity_cSt = c(1:21, rep(NA, n() - 21)))
head(Vis.data$Viscosity_cSt, 25)  # First few values to check
```


```{r}
library("tidyverse")
COF_without_additive<- BioLubricant_Additive_PairedData %>%
  mutate(Friction_Coeff = c(Friction_Coefficient[1:21], rep(NA, n() - 21)))
head(COF_without_additive$Friction_Coeff, 25)

```


```{r}
Vis.data1 <- BioLubricant_Additive_PairedData %>%
  mutate(
    Vis.data = c(1:21, rep(NA, n() - 21)),
    COF_without_additive = c(BioLubricant_Additive_PairedData$Friction_Coefficient[1:21], 
                             rep(NA, n() - 21))
  )

```




```{r}
Vis.plot <- Vis.data1 %>% 
  filter(!is.na(Viscosity_cSt), !is.na(Friction_Coefficient))
```


```{r}
myPalette <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#999999",
  "#A6761D", "#984EA3", "#E41A1C", "#377EB8",
  "#4DAF4A", "#FF7F00", "#FFFF33", "#A65628",
  "#F781BF", "#66C2A5", "#E7298A", "#BC80BD", "#8DD3C7"
)
```


```{r}
Vis.data1_plot <- Vis.data1 %>%
  filter(!is.na(COF_without_additive),
         !is.na(Viscosity_cSt),
         !is.na(Vis.data)) %>%
  mutate(Viscosity_cSt = as.factor(Viscosity_cSt))
```



```{r}

ggplot(Vis.data1_plot, aes(x = COF_without_additive, y = Vis.data, fill = Viscosity_cSt)) +
  geom_boxplot() +
  geom_point(shape = 21, color = "black", position = position_jitter(width = 0.2)) +
  theme_classic() +
  xlab("COF (rounded)") +
  ylab("Viscosity (cSt)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

```{r}
BioLubricant_Additive_PairedData$Wear_Scar_Diameter_mm <- as.factor(BioLubricant_Additive_PairedData$Wear_Scar_Diameter_mm)
```


```{r}
ggplot(BioLubricant_Additive_PairedData,
       aes(x = as.factor(round(Viscosity_cSt, 1)),
           y = Friction_Coefficient,
           fill = Oxidation_Stability_hr,
           group = interaction(round(Viscosity_cSt, 1), Oxidation_Stability_hr))) +
  stat_summary(fun = mean, geom = "bar",
               position = position_dodge(width = 0.9), width = 0.8) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.9), width = 0.2) +
  xlab("Viscosity (cSt)") +
  ylab("Friction Coefficient") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

```




```{r}

BioLubricant_Additive_PairedData$Viscosity_Group <- cut(BioLubricant_Additive_PairedData$Viscosity_cSt, breaks = 4)

ggplot(BioLubricant_Additive_PairedData, 
       aes(x = Viscosity_Group, y = Friction_Coefficient, fill = Viscosity_Group)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  theme_classic() +
  xlab("Viscosity Group (cSt)") +
  ylab("Friction Coefficient")
```





```{r}
n_total <- nrow(BioLubricant_Additive_PairedData)
print(n_total)
```



```{r}
Vis.data_with <- BioLubricant_Additive_PairedData %>%
  mutate(Viscosity_cSt = c(
    rep(NA, 21),  # rows 1–21 (no additive)
    BioLubricant_Additive_PairedData$Viscosity_cSt[22:40]  # rows 22–40 (with additive)
  ))
head(Vis.data_with$Viscosity_cSt, 25)   # should be NA
tail(Vis.data_with$Viscosity_cSt, 20) 

```


```{r}
Vis.data$Wear_Scar_Diameter_mm <- as.numeric(Vis.data$Wear_Scar_Diameter_mm)
Vis.data_with$Wear_Scar_Diameter_mm <- as.numeric(Vis.data_with$Wear_Scar_Diameter_mm)

# Add Additive_Status labels
Vis.data$Additive_Status <- "Without"
Vis.data_with$Additive_Status <- "With"

# Now  bind the two together
library(dplyr)
Vis.data_combined <- bind_rows(Vis.data, Vis.data_with)
```




```{r}
ggplot(Vis.data_combined, aes(x = Additive_Status, y = Viscosity_cSt, fill = Additive_Status)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  theme_classic() +
  labs(
    x = "Additive Status",
    y = "Viscosity (cSt)",
    title = "Effect of Additive on Viscosity"
  ) +
  scale_fill_manual(values = c("Without" = "#E69F00", "With" = "#56B4E9"))
```




```{r}
library(ggpubr)      # for stat_compare_means
library(emmeans)     # for emmeans, Tukey test
library(multcomp)    # for cld letters

```



```{r}
model <- lm(Viscosity_cSt ~ Additive_Status, data = Vis.data_combined)
anova_result <- anova(model)
print(anova_result)
```



```{r}
summary(model)
```



```{r}
library(emmeans)
em_result <- emmeans(model, pairwise ~ Additive_Status, adjust = "tukey")
print(em_result$contrasts)
```



```{r}
model <- lm(Viscosity_cSt ~ Additive_Status, data = Vis.data_combined)
em_result <- emmeans(model, pairwise ~ Additive_Status, adjust = "tukey")

cld_result <- cld(em_result$emmeans, Letters = letters)
print(cld_result)
```




```{r}
library(ggplot2)

ggplot(cld_result, aes(x = Additive_Status, y = emmean, fill = Additive_Status)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  geom_text(aes(label = .group, y = emmean + 0.7), 
            size = 6, fontface = "bold", color = "black") +
  scale_fill_manual(values = c("Without" = "#E69F00", "With" = "#56B4E9")) +
  labs(
    title = "Estimated Marginal Means of Viscosity by Additive Status",
    x = "Additive Status",
    y = "Estimated Viscosity (cSt)"
  ) +
  theme_classic()
```




```{r}
response_vars <- c("Viscosity_cSt", "Friction_Coefficient", "Oxidation_Stability_hr", "Wear_Scar_Diameter_mm")
```



```{r}
compare_plot <- function(data, response_var) {
  ggplot(data, aes(x = Additive_Status, y = .data[[response_var]], fill = Additive_Status)) +
    geom_violin(trim = FALSE, alpha = 0.6) +
    geom_boxplot(width = 0.1, outlier.shape = NA) +
    labs(title = paste("Comparison of", response_var),
         y = response_var,
         x = "Additive Status") +
    theme_minimal() +
    theme(legend.position = "none")
}
```




```{r}
for (var in response_vars) {
  plot_out <- compare_plot(Vis.data_combined, response_var = var)
  print(plot_out)
}
```


```{r}
library(dplyr)

# Filter rows where Additive is "Yes"
oxidative_with_additive <- BioLubricant_Additive_PairedData %>%
  filter(Additive == "Yes")

# Summary of oxidation stability with additive
summary(oxidative_with_additive$Oxidation_Stability_hr)
```

```{r}
library(readxl)
library(dplyr)
library(tidyr)

Biolubricant_Additive_PairedData <- read_excel("C:/Users/noork/Desktop/PLPA course/Project design/Additive-response-toward-bio-lubricants-Class-Project/BioLubricant_Additive_PairedData.xlsx")
View(Biolubricant_Additive_PairedData)

ox_stability_wide <- df %>%
  dplyr::select(Sample_ID, Additive, Oxidation_Stability_hr) %>%
  pivot_wider(names_from = Additive, values_from = Oxidation_Stability_hr) %>%
  mutate(diff_with_vs_without = Yes - No)
  print(ox_stability_wide)

```

```{r}
df_yes <- df[df$Additive == "Yes", ]
```

```{r}
df_subset <- df[df$Additive == "Yes" & df$Oxidation_Stability_hr > 75, ]
```


```{r}
df_no_additive <- BioLubricant_Additive_PairedData %>% 
  filter(Additive == "No")
```






```{r}
# Filter rows where Additive is "No"
oxidative_without_additive <- BioLubricant_Additive_PairedData %>%
  filter(Additive == "No")

# Summary of oxidation stability without additive
summary(oxidative_without_additive$Oxidation_Stability_hr)
```



```{r}
# Load necessary library
library(ggplot2)

# boxplot comparing oxidative stability with and without additive
ggplot(BioLubricant_Additive_PairedData, aes(x = Additive, y = Oxidation_Stability_hr, fill = Additive)) +
  geom_boxplot() +
  labs(
    title = "Effect of Additive on Oxidation Stability",
    x = "Additive Status",
    y = "Oxidation Stability (hr)"
  ) +
  theme_minimal()
```





```{r}
# Fit a linear model: Oxidation Stability as a function of Additive
lm_model <- lm(Oxidation_Stability_hr ~ Additive, data = BioLubricant_Additive_PairedData)

# View model summary
summary(lm_model)
```


```{r}
# Create an empty data frame
oxidation_summary <- NULL

# Loop over each additive group
for (group in unique(BioLubricant_Additive_PairedData$Additive)) {
  
  # Filter data for current group
  group_data <- BioLubricant_Additive_PairedData %>%
    filter(Additive == group)
  
  # Calculate mean and standard deviation
  mean_val <- mean(group_data$Oxidation_Stability_hr)
  sd_val <- sd(group_data$Oxidation_Stability_hr)
  n_val <- nrow(group_data)
  
  # Combine into a data frame
  result_i <- data.frame(
    Additive = group,
    Mean_Oxidation_Stability = mean_val,
    SD = sd_val,
    N = n_val
  )
  
  # Append to final data frame
  oxidation_summary <- rbind(oxidation_summary, result_i)
}

# View final summary table
print(oxidation_summary)

```






```{r}
ggplot(BioLubricant_Additive_PairedData, aes(
  x = Additive,
  y = Oxidation_Stability_hr,
  color = Viscosity_cSt,
  size = Wear_Scar_Diameter_mm
)) +
  geom_jitter(width = 0.2, alpha = 0.7) +
  labs(
    title = "Multi-variable Visualization of Bio-lubricants",
    y = "Oxidation Stability (hr)",
    x = "Additive",
    color = "Viscosity (cSt)",
    size = "Wear Scar (mm)"
  ) +
  theme_minimal()
```



```{r}

ggplot(BioLubricant_Additive_PairedData, aes(
  x = Additive,
  y = Oxidation_Stability_hr,
  fill = Additive
)) +
  geom_boxplot(
    position = position_dodge(0.8),
    width = 0.6,
    alpha = 0.5,
    color = "black"   # outline color of boxes
  ) +
  geom_point(
    aes(color = Additive),  # color = border of points
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
    size = 2,
    alpha = 0.8
  ) +
  labs(
    title = "Oxidation Stability by Additive",
    y = "Oxidation Stability (hr)",
    x = "Additive"
  ) +
  theme_classic()
```






```{r}
library(tidyr)
library(ggplot2)
library(dplyr)

# Convert factor to numeric
BioLubricant_Additive_PairedData <- BioLubricant_Additive_PairedData %>%
  mutate(Wear_Scar_Diameter_mm = as.numeric(as.character(Wear_Scar_Diameter_mm)))

# Now pivot to long format
long_data <- BioLubricant_Additive_PairedData %>%
  pivot_longer(cols = c(Viscosity_cSt, Friction_Coefficient, Wear_Scar_Diameter_mm, Oxidation_Stability_hr),
               names_to = "Property", values_to = "Value")

# Faceted boxplot
ggplot(long_data, aes(x = Additive, y = Value, fill = Additive)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~ Property, scales = "free_y") +
  theme_minimal() +
  labs(title = "Effect of Additive on Bio-lubricant Properties")


```




```{r}
ggplot(BioLubricant_Additive_PairedData, aes(x = Viscosity_cSt, y = Oxidation_Stability_hr, color = Additive)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Relationship between Viscosity and Oxidation Stability",
    x = "Viscosity (cSt)",
    y = "Oxidation Stability (hr)"
  ) +
  theme_classic()
```

```{r}
# Step 1: Calculate mean and SD for each group
ox_summary <- BioLubricant_Additive_PairedData %>%
  group_by(Additive) %>%
  summarise(
    mean_OS = mean(Oxidation_Stability_hr, na.rm = TRUE),
    sd_OS = sd(Oxidation_Stability_hr, na.rm = TRUE),
    .groups = "drop"
  )
```


```{r}
ggplot(ox_summary, aes(x = Additive, y = mean_OS, fill = Additive)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean_OS - sd_OS, ymax = mean_OS + sd_OS), 
                width = 0.2, linewidth = 1) +
  ylab("Mean Oxidation Stability (hr)") +
  xlab("Additive Status") +
  theme_minimal()
```

```{r}
t.test(Oxidation_Stability_hr ~ Additive, data = BioLubricant_Additive_PairedData)
```

```{r}
ggplot(ox_summary, aes(x = Additive, y = mean_OS, fill = Additive)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean_OS - sd_OS, ymax = mean_OS + sd_OS), width = 0.2) +
  geom_text(
    aes(
      y = mean_OS + sd_OS + 1,
      label = paste("Mean =", round(mean_OS, 1), "SD =", round(sd_OS, 1))
    ),
    size = 3
  ) +
  theme_minimal() +
  labs(
    y = "Oxidation Stability (hr)",
    x = "Additive"
  )
```














