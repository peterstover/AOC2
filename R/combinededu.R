library(tidyverse)

# Prepare male data
enrollment_long <- male_enrollment %>%
  pivot_longer(cols = c(Metro, Nonmetro), names_to = "Category", values_to = "EnrollmentRate") %>%
  mutate(Sex = "Male", NonEnrollment =  EnrollmentRate)

# Prepare female data
fenrollment_long <- f_enrollment %>%
  pivot_longer(cols = c(Metro, Nonmetro), names_to = "Category", values_to = "EnrollmentRate") %>%
  mutate(Sex = "Female", NonEnrollment = EnrollmentRate)

# Combine both datasets
combined_enrollment <- bind_rows(enrollment_long, fenrollment_long)

# Plot
ggplot(combined_enrollment, aes(x = Year, y = 1- NonEnrollment, color = Category, linetype = Sex)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(.15, 0.5)
  ) +
  scale_x_continuous(breaks = seq(2009, 2023, by = 2)) +
  scale_color_manual(values = c("Metro" = "darkgreen", "Nonmetro" = "skyblue")) +
  labs(
    title = "Proportion Enrolled in Higher Education (Ages 18–24)",
    subtitle = "By Sex and County Type",
    x = "Year",
    y = "Enrollment Rate",
    color = "County Type",
    linetype = "Sex"
  ) +
  theme_minimal()
