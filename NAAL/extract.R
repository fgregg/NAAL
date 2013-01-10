naal <- read.csv("naal.csv")

naal_extract <- naal[, c("NUMID", # Randomly assigned case identifier
                         "DAGE", # Age (6 categories) NOTE: continuous
                                 # age not available
                         "DSEX", # Gender
                         "DRACE", # Race/Ethnicity
                         "DEDATTN", # Educational Attainment (9 categories)
                         "DMED", # Mother's educational attainment
                         "DFED", # Father's educational attainment
                         "DBQ2421", # Approximate personal income (8 categories)
                         "DWEEKWG", # Weekly wage (previous week),
                         "OCODE_C", # Occupation
                         "DLFORCE", # Labor force participation
                         "DBQ1530C", # Not employed because taking care of home or family
                         "DBQ1430", # Volunteered (unpaid) during past year
                         "DBQ1530D" # Not employed because going to school
                         )]

names(naal_extract) <- c("id",
                         "age",
                         "gender",
                         "ethnicity",
                         "educational_attainment",
                         "mothers_educational_attainment",
                         "fathers_educational_attainment",
                         "approx_personal_income",
                         "weekly_wage",
                         "occupation",
                         "labor_force_participation",
                         "home_maker",
                         "volunteer",
                         "in_school")

naal_extract[naal_extract == "."] <- NA

naal_extract$age <- ordered(naal_extract$age,
                            labels=c(
                              "16-18",
                              "19-24",
                              "25-39",
                              "40-49",
                              "50-64",
                              "65+"))

naal_extract$gender <- factor(naal_extract$gender,
                              labels=c("female", "male"))

naal_extract$ethnicity <- factor(naal_extract$ethnicity,
                                 labels=c(
                                   "white",
                                   "black",
                                   "hispanic",
                                   "other (incl multi-racial)"))

naal_extract$educational_attainment <- ordered(naal_extract$educational_attainment,
                                               labels = c(
                                                 "Still in high school",
                                                 "Less than/some high school",
                                                 "GED/high school equivalency",
                                                 "High school graduate",
                                                 "Vocational/trade/business school",
                                                 "Some college",
                                                 "Associate's/2-year degree",
                                                 "College graduate",
                                                 "Graduate studes/degree")

                                               )

naal_extract$mothers_educational_attainment <- ordered(naal_extract$mothers_educational_attainment,
                                                       labels = c(
                                                         "Less than/some high school",
                                                         "GED/high school equivalency",
                                                         "High school graduate",
                                                         "Vocational/trade/business school",
                                                         "Some college",
                                                         "Associate's/2-year degree",
                                                         "College graduate",
                                                         "Graduate studes/degree"))


naal_extract$fathers_educational_attainment <- ordered(naal_extract$fathers_educational_attainment,
                                               labels = c(
                                                 "Less than/some high school",
                                                 "GED/high school equivalency",
                                                 "High school graduate",
                                                 "Vocational/trade/business school",
                                                 "Some college",
                                                 "Associate's/2-year degree",
                                                 "College graduate",
                                                 "Graduate studes/degree")
                                                       )

naal_extract$occupation <- factor(as.numeric(as.character(naal_extract$occupation)),
                                  labels = c(
                                    "(0010-0430) executive/administration/managerial",
                                    "(0500-0950) management related",
                                    "(1000-1240) math and comp scientists",
                                    "(1300-1530) engineers/architects/surveyors",
                                    "(1540-1560) engineering and related technicians",
                                    "(1600-1760) physical scientists",
                                    "(1800-1860) social scientists and related",
                                    "(1900-1960) life/physical/social science techs",
                                    "(2000-2060) counselors/social/religious",
                                    "(2100-2150) lawyers/judges/legal support",
                                    "(2200-2340) teachers",
                                    "(2400-2550) education/training/library",
                                    "(2600-2760) entertainers and performers/sports and related",
                                    "(2800-2960) media and communication",
                                    "(3000-3260) health diagnosing and treating",
                                    "(3300-3650) health care tech and support",
                                    "(3700-3950) protective service",
                                    "(4000-4160) food prep and serving related",
                                    "(4200-4250) cleaning and building service",
                                    "(4300-4430) entertainment attendants and related",
                                    "(4460-4650) funeral related/personal care and service",
                                    "(4700-4960) sales and related",
                                    "(5000-5930) office and admin support",
                                    "(6000-6130) farming/fishing/forestry",
                                    "(6200-6940) construction trades and extraction",
                                    "(7000-7620) installation/maintenance/repair",
                                    "(7700-7750) production and operating",
                                    "(7800-7850) food preparation",
                                    "(7900-8960) setter/operators/tenders",
                                    "(9000-9750) transportation and material moving",
                                    "Have not worked in the past 3 years")
                                  )



naal_extract$volunteer <- factor(naal_extract$volunteer,
                                 labels = c("Yes", "No"))

naal_extract$home_maker <- factor(naal_extract$home_maker,
                                  labels = c("Yes", "No", "Not Applicable"))

naal_extract$in_school <- factor(naal_extract$in_school,
                                  labels = c("Yes", "No", "Not Applicable"))

naal_extract$labor_force_participation <- factor(naal_extract$labor_force_participation,
                                                 labels = c(
                                                   "Employed full time",
                                                   "Employed part time",
                                                   "Employed but not at work",
                                                   "Unemployed",
                                                   "Out of labor force"))
                                                   

# Only include those within the labor force
naal_extract <- naal_extract[naal_extract$labor_force_participation != "Out of labor force",]

naal_extract <- naal_extract[!(naal_extract$labor_force_participation == "Employed part time" & naal_extract$home_maker == "Yes"),]

naal_extract <- naal_extract[!(naal_extract$labor_force_participation == "Employed part time" & naal_extract$volunteer == "Yes"),]

naal_extract <- naal_extract[!(naal_extract$labor_force_participation == "Employed part time" & naal_extract$in_school == "Yes"),]

# Subset the data for those with a valid age
naal_extract <- naal_extract[!is.na(naal_extract$age),]

# Only include those between 25 and 60
naal_extract <- naal_extract[naal_extract$age %in% c("25-39",
                                                     "40-49",
                                                     "50-64"),]
