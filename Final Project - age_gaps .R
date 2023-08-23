setwd("/Users/Diarra/Desktop/R Bootcamp Doc/final-project")

age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv')
install.packages(gtsummary)
library(gtsummary)


#GtSummary
summary(age_gaps)
view(age_gaps)

tbl_summary(
	age_gaps,
	by = age_difference,
	include = c(character_1_gender, character_2_gender,
							actor_1_age, actor_2_age),
	label = list(
		character_1_gender ~ "Gender of the oldest",
		character_2_gender ~ "Gender of the youngest",
		actor_1_age ~ "Age of the oldest",
		actor_2_age ~ "Age of the youngest"
	),
	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")




#Linear regression

linear_model <- lm(age_difference ~ release_year + actor_1_age + actor_2_age,
									 data = age_gaps)
tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		actor_1_age ~ "Age of the oldest",
		actor_2_age ~ "Age of the youngest"
	))


# Create a figure

figure1 <- hist(age_gaps$age_difference)



# write and use function

new_mean <- function(x){ n <- length(x)
	mean_val <- sum(x)/n
	return(mean_val)}

new_mean(age_gaps$age_difference)










