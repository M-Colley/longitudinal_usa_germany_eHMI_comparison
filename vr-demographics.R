library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))


main_df <- read_xlsx(path = "data-demographic.xlsx", sheet = "results")
main_df <- as.data.frame(main_df)
names(main_df)


# participant 4 got sick
main_df <- subset(main_df, user_id != 'T28F01M98')


print("Output: age of participants")
print(mean(main_df$age))
print(sd(main_df$age))
print("----------")

m.gender.male <- sum(substr(main_df$gender, 1, 1) == "M") # M for male or W for female 

m.gender.female <- sum(substr(main_df$gender, 1, 1) == "W") # M for male or W for female 
print("Output: count of gender")
print(m.gender.male)
print(m.gender.female)
print("----------")

#vr evaluation: vr games experience
vr.games.experience <- main_df$vr_games_experience 
vr.games.experience <- gsub(pattern = "stimme überhaupt nicht zu", replacement = 1, x = vr.games.experience)
vr.games.experience <- gsub(pattern = "stimme nicht zu", replacement = 2, x = vr.games.experience)
vr.games.experience <- gsub(pattern = "neutral", replacement = 3, x = vr.games.experience)
vr.games.experience <- gsub(pattern = "stimme zu", replacement = 4, x = vr.games.experience)
vr.games.experience <- gsub(pattern = "stimme voll zu", replacement = 5, x = vr.games.experience)
vr.games.experience <- as.integer(vr.games.experience)

print("Output: vr games experience")
print(mean(vr.games.experience))
print(sd(vr.games.experience))
print("----------")

#vr evaluation: vr studies experience
vr.studies.experience <- main_df$vr_study_experience 
vr.studies.experience <- gsub(pattern = "stimme überhaupt nicht zu", replacement = 1, x = vr.studies.experience)
vr.studies.experience <- gsub(pattern = "stimme nicht zu", replacement = 2, x = vr.studies.experience)
vr.studies.experience <- gsub(pattern = "neutral", replacement = 3, x = vr.studies.experience)
vr.studies.experience <- gsub(pattern = "stimme zu", replacement = 4, x = vr.studies.experience)
vr.studies.experience <- gsub(pattern = "stimme voll zu", replacement = 5, x = vr.studies.experience)
vr.studies.experience <- as.integer(vr.studies.experience)
print("Output: vr studies experience")
print(mean(vr.studies.experience))
print(sd(vr.studies.experience))
print("----------")

#vr evaluation: vr games playing
vr.games.playing <- main_df$vr_games_playing
vr.games.playing <- gsub(pattern = "stimme überhaupt nicht zu", replacement = 1, x = vr.games.playing)
vr.games.playing <- gsub(pattern = "stimme nicht zu", replacement = 2, x = vr.games.playing)
vr.games.playing <- gsub(pattern = "neutral", replacement = 3, x = vr.games.playing)
vr.games.playing <- gsub(pattern = "stimme zu", replacement = 4, x = vr.games.playing)
vr.games.playing <- gsub(pattern = "stimme voll zu", replacement = 5, x = vr.games.playing)
vr.games.playing <- as.integer(vr.games.playing)
print("Output: vr games playing")
print(mean(vr.games.playing))
print(sd(vr.games.playing))
print("----------")

#vr evaluation: vr games no playing
vr.games.no.playing <- main_df$vr_games_no_playing
vr.games.no.playing <- gsub(pattern = "stimme überhaupt nicht zu", replacement = 1, x = vr.games.no.playing)
vr.games.no.playing <- gsub(pattern = "stimme nicht zu", replacement = 2, x = vr.games.no.playing)
vr.games.no.playing <- gsub(pattern = "neutral", replacement = 3, x = vr.games.no.playing)
vr.games.no.playing <- gsub(pattern = "stimme zu", replacement = 4, x = vr.games.no.playing)
vr.games.no.playing <- gsub(pattern = "stimme voll zu", replacement = 5, x = vr.games.no.playing)
vr.games.no.playing <- as.integer(vr.games.no.playing)
print("Output: vr games no playing")
print(mean(vr.games.no.playing))
print(sd(vr.games.no.playing))
print("----------")


#vr evaluation: interest in av
interest_av <- main_df$interest_av
interest_av <- gsub(pattern = "stimme überhaupt nicht zu", replacement = 1, x = interest_av)
interest_av <- gsub(pattern = "stimme nicht zu", replacement = 2, x = interest_av)
interest_av <- gsub(pattern = "neutral", replacement = 3, x = interest_av)
interest_av <- gsub(pattern = "stimme zu", replacement = 4, x = interest_av)
interest_av <- gsub(pattern = "stimme voll zu", replacement = 5, x = interest_av)
interest_av <- as.integer(interest_av)
print("Output: interest_av")
print(mean(interest_av))
print(sd(interest_av))
print("----------")

#vr evaluation: ease av
ease_av <- main_df$ease_av
ease_av <- gsub(pattern = "stimme überhaupt nicht zu", replacement = 1, x = ease_av)
ease_av <- gsub(pattern = "stimme nicht zu", replacement = 2, x = ease_av)
ease_av <- gsub(pattern = "neutral", replacement = 3, x = ease_av)
ease_av <- gsub(pattern = "stimme zu", replacement = 4, x = ease_av)
ease_av <- gsub(pattern = "stimme voll zu", replacement = 5, x = ease_av)
ease_av <- as.integer(ease_av)
print("Output: ease_av")
print(mean(ease_av))
print(sd(ease_av))
print("----------")

#vr evaluation: av in ten years
ten_years_av <- main_df$ten_years_av
ten_years_av <- gsub(pattern = "stimme überhaupt nicht zu", replacement = 1, x = ten_years_av)
ten_years_av <- gsub(pattern = "stimme nicht zu", replacement = 2, x = ten_years_av)
ten_years_av <- gsub(pattern = "neutral", replacement = 3, x = ten_years_av)
ten_years_av <- gsub(pattern = "stimme zu", replacement = 4, x = ten_years_av)
ten_years_av <- gsub(pattern = "stimme voll zu", replacement = 5, x = ten_years_av)
ten_years_av <- as.integer(ten_years_av)
print("Output: ten_years_av")
print(mean(ten_years_av))
print(sd(ten_years_av))
print("----------")

