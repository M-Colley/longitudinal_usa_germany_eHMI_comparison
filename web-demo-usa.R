library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))


library(devtools)
source_url("https://raw.githubusercontent.com/M-Colley/rCode/main/r_functionality.R")


main_df <- read_xlsx(path = "DemoData_USA.xlsx", sheet = "DemoData_USA")
main_df <- as.data.frame(main_df)
names(main_df)

print(count(main_df))

growing_place <- c("Georgia", "California", "Ohio", "Arizona", "Michigan", "Chicago", "Utah", "New York", "Colarado", "New Jersey", "Florida", "Pennsylvania")
growing_place <- sort(growing_place)
print(growing_place)

print("Output: age of participants")
print(mean(main_df$age))
print(sd(main_df$age))
print(max(main_df$age))
print(min(main_df$age))
print("----------")

m.gender.male <- sum(substr(main_df$gender, 1, 1) == "M") # M for male or W for female 
m.gender.female <- sum(substr(main_df$gender, 1, 1) == "F") # M for male or W for female 
print("Output: count of gender")
print(m.gender.male)
print(m.gender.female)
print("----------")

print("Output: interest_av")
print(mean(main_df$interestav))
print(sd(main_df$interestav))
print("----------")

print("Output: interest_reach")
print(mean(main_df$interest_reach))
print(sd(main_df$interest_reach))
print("----------")

print("Output: interest_years")
print(mean(main_df$interest_years))
print(sd(main_df$interest_years))
print("----------")



###PBS
main_df$aggressive <- rowSums(main_df[, c("aggressive11", "aggressive15", "aggressive23", "aggressive28")]) / 4.0
main_df$error <- rowSums(main_df[, c("error16", "error35", "error9", "error6")]) / 4.0
main_df$filter <- rowSums(main_df[, c("filter19", "filter32", "filter7")]) / 3.0
main_df$lapse <- rowSums(main_df[, c("lapse22", "lapse36", "lapse39", "lapse4")]) / 4.0
main_df$positive <- rowSums(main_df[, c("positive21", "positive34", "positive46", "positive5")]) / 4.0
main_df$violation <- rowSums(main_df[, c("violation17", "violation25", "violation37", "violation40")]) / 4.0
main_df$filter <- rowSums(main_df[, c("filter19", "filter32", "filter7")])/ 3.0


print(mean(main_df$aggressive))
print(sd(main_df$aggressive))
print("----------")
print(mean(main_df$error))
print(sd(main_df$error))
print("----------")
print(mean(main_df$filter))
print(sd(main_df$filter))
print("----------")
print(mean(main_df$lapse))
print(sd(main_df$lapse))
print("----------")
print(mean(main_df$positive))
print(sd(main_df$positive))
print("----------")
print(mean(main_df$violation))
print(sd(main_df$violation))
print("----------")
print(mean(main_df$filter))
print(sd(main_df$filter))
print("----------")