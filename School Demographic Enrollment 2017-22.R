# # https://data.cityofnewyork.us/Education/2017-18-2021-22-Demographic-Snapshot/c7ru-d68s
# 
# 2017-18 - 2021-22 Demographic SnapshotEducation
# View Data
# 
# Visualize
# ExportAPI
# 
# "Enrollment counts are based on the October 31 Audited Register for the 2017-18 to 2019-20 school years. To account for the delay in the start of the school year, enrollment counts are based on the November 13 Audited Register for 2020-21 and the November 12 Audited Register for 2021-22.
# * Please note that October 31 (and November 12-13) enrollment is not audited for charter schools or Pre-K Early Education Centers (NYCEECs). Charter schools are required to submit enrollment as of BEDS Day, the first Wednesday in October, to the New York State Department of Education."
# Enrollment counts in the Demographic Snapshot will likely exceed operational enrollment counts due to the fact that long-term absence (LTA) students are excluded for funding purposes.
# Data on students with disabilities, English Language Learners, students' povery status, and students' Economic Need Value are as of the June 30 for each school year except in 2021-22. Data on SWDs, ELLs, Poverty, and ENI in the 2021-22 school year are as of March 7, 2022.
# 3-K and Pre-K enrollment totals include students in both full-day and half-day programs. Four-year-old students enrolled in Family Childcare Centers are categorized as 3K students for the purposes of this report.
# All schools listed are as of the 2021-22 school year. Schools closed before 2021-22 are not included in the school level tab but are included in the data for citywide, borough, and district. Programs and Pre-K NYC Early Education Centers (NYCEECs) are not included on the school-level tab.
# Due to missing demographic information in rare cases at the time of the enrollment snapshot, demographic categories do not always add up to citywide totals.
# Students with disabilities are defined as any child receiving an Individualized Education Program (IEP) as of the end of the school year (or March 7 for 2021-22).
# NYC DOE "Poverty" counts are based on the number of students with families who have qualified for free or reduced price lunch, or are eligible for Human Resources Administration (HRA) benefits. In previous years, the poverty indicator also included students enrolled in a Universal Meal School (USM), where all students automatically qualified, with the exception of middle schools, D75 schools and Pre-K centers. In 2017-18, all students in NYC schools became eligible for free lunch. In order to better reflect free and reduced price lunch status, the poverty indicator does not include student USM status, and retroactively applies this rule to previous years.
# "The school’s Economic Need Index is the average of its students’ Economic Need Values. The Economic Need Index (ENI) estimates the percentage of students facing economic hardship. The 2014-15 school year is the first year we provide ENI estimates. The metric is calculated as follows:
# * The student’s Economic Need Value is 1.0 if:
# o The student is eligible for public assistance from the NYC Human Resources Administration (HRA);
# o The student lived in temporary housing in the past four years; or
# o The student is in high school, has a home language other than English, and entered the NYC DOE for the first time within the last four years.
# * Otherwise, the student’s Economic Need Value is based on the percentage of families (with school-age children) in the student’s census tract whose income is below the poverty level, as estimated by the American Community Survey 5-Year estimate (2020 ACS estimates were used in calculations for 2021-22 ENI). The student’s Economic Need Value equals this percentage divided by 100.
# 
# Due to differences in the timing of when student demographic, address and census data were pulled, ENI values may vary, slightly, from the ENI values reported in the School Quality Reports.
# 
# In previous years, student census tract data was based on students’ addresses at the time of ENI calculation. Beginning in 2018-19, census tract data is based on students’ addresses as of the Audited Register date of the given school year.
# 
# In previous years, the most recent new entry date was used for students with multiple entry dates into the NYCDOE. Beginning in 2018-19, students’ earliest entry date is used in ENI calculations.
# 
# Beginning in 2018-19, students missing ENI data are imputed with the average ENI at their school.
# "
# In order to maintain student privacy, schools with % Poverty and ENI values below 5% or above 95% have had their exact values for each category replaced with "Below 5%" and "Above 95%", respectively.
# Before the start of the 2017-18 school year, the New York State Education Department implemented a new data matching process that refined the methods to identify families eligible for free lunch. This new matching system provides a more efficient and accurate process for matching students across a range of forms that families already complete. This new matching process yielded an increase in the number of students directly certified for free lunch (in other words, matched to another government program) and therefore increased the direct certification rate. As such, the increase in the percent of students in poverty and the Economic Need Index for the 2017-18 school year and later reflects this new matching process, which allows the City to better identify students eligible for free lunch.
# Approximately 25% of charter schools in NYC do not use NYC DOE School Food to provide meal services. The NYC DOE Office of School Food does not collect documentation on students’ eligibility for Free or Reduced Price Lunch from schools that do not utilize NYC DOE School Food. As a result, the Poverty figures may be understated for approximately 25% of charter schools.
# New York State Education Department begins administering assessments to be identified as an English Language Learner (ELL) in Kindergarten, but students in Pre-K are still included in the denominator for the ELL calculations. Also, Pre-K NYC Early Education Centers do not use NYC DOE School Food to provide meal services, but are included in the denominator for Poverty calculations.
# 
# About this Dataset
# Mute Dataset
# Updated
# June 16, 2022
# Data Last Updated
# June 15, 2022
# Metadata Last Updated
# June 16, 2022
# Date Created
# June 15, 2022
# Views
# 346
# Downloads
# 64
# Data Provided by
# New York City Department of Education
# Dataset Owner
# New York City Department of Education
# Dataset Information
# Agency	Department of Education (DOE)
# Update
# Update Frequency	Historical data
# Automation	No
# Date Made Public	06/15/2022
# Attachments
# Demographic Snapshot 2017-18 to 2021-22 (Public).xlsx
# 2017-18 -2021-22 Demographic Snapshot - Citywide DD.xlsx
# 


## KEEP IT SIMPLE

#rm(list = ls())
library(tidyverse)
(SchoolDemo <- read_csv("~/Documents/R/Datasets/2017-18__-_2021-22_Demographic_Snapshot.csv"))
colnames(SchoolDemo)
fivenum(SchoolDemo$`% Black`) # Range of % black students
filter(SchoolDemo, `% Black` == 0 ) #Which school had 0 black students
Demo1 <- select(SchoolDemo, #Cleaning SchoolDemo
                     School = "School Name",
                     Enroll = 'Total Enrollment',
                     Black = '% Black', 
                     Hisp = '% Hispanic',
                     Asian = '% Asian',
                     White = '% White',
                     Poverty = '% Poverty')
Demo1$Year <- substr(SchoolDemo$Year, 1, 4) #fix year - remove range
# Demo1$change2018 <- filter(Demo1, year == 2018) - filter(Demo1, year == 2017)
  # We can't compare 2017 and 2018 here because the columns are not the same length.
  # Not every school has data for 2017 and 2018
  # But we need this to calculate percent change for each year
  # Time to remove school for which we have incomplete data
  school_names <- unique(Demo1$School)
  x <- Demo1[Demo1$School == school_names[1],] 
  #(x <- filter(Demo1, School == school_names[1])) #same thing
  x$all5yrs <- nrow(x) == 5 #wrosk
  
  All5yrs <- vector()
  Schoolx <- vector()
for (i in school_names) {
  # i <- school_names[1]
  x <- Demo1 %>% filter(School == i) # filter by school
  t <- nrow(x) == 5 #which have 5 rows
  Schoolx <- c(Schoolx, i)
  All5yrs <- c(All5yrs, t)
}
  #check it works
  All5yrs_df <- data.frame(school = Schoolx,
                      allyrs = All5yrs) %>% tibble()
  length(unique(Demo1$School)) == nrow(All5yrs_df)
  
  # blah blach blah
  
# @#$%*&@#$(%(&@(#$*%&(#@*$%&@#$(*%&)))))
# #$*%&#$@(%*&#@$(*%&#$(%*&@(#$%*&@#$(@*%)))))
# @#$%#@$*%)*$#)(%*@#$)%(*@#$%)(#$@%)(*%@#) :(
  
  
  
  
  
  
# Blah blah blah
  #starting over
  rm(list = ls())
  
  
  
# STARTING F ROM SCRATCH
  #Clean SchoolDemo
library(tidyverse)
(SchoolDemo <- read_csv("~/Documents/R/Datasets/2017-18__-_2021-22_Demographic_Snapshot.csv"))
colnames(SchoolDemo) 
SchoolDemo$Year  <- substr(SchoolDemo$Year, 1, 4)


#new cleaner supremer School Demographics
newSchoolDemo <- select(SchoolDemo, 
                        School = `School Name`, 
                        Year = Year,
                        Enrollment = `Total Enrollment`,
                        Black = `% Black`,
                        Asian = `% Asian`,
                        White = `% White`,
                        Hispanic = `% Hispanic`,
                        Poverty = `% Poverty`)
newSchoolDemo$Poverty <- parse_number(newSchoolDemo$Poverty)/100





#@)#$*(%#)$@(%*@$#%*@$#(%!(*@#%&)))*************
# START run from here 
school_names <- unique(SchoolDemo$`School Name`)
Poverty_Change <- vector()  # should be == school names x years (for each school)
Poverty_df <- vector(length = 3)
Howmanyyears <- vector() #How many years of data 

### FOR Go by each school and count changes in percent poverty over each year
for (i in school_names) { # Filter by school
      # i <- school_names[1] # EXAMPLE
      # print(i) # TESTING
  x <- filter(newSchoolDemo, School == i)
  years <- as.numeric(x$Year) #years available (1-5)
  Howmanyyears <- c(Howmanyyears, length(years))
  
    for (y in years) { # Compare this year and previous
          # y <- 2018 #  EXAMPLE
          # print(y) #TESTING
      poverty_yr <- x$Poverty[x$Year == y]
      poverty_yr_prev <- x$Poverty[x$Year == (y-1)]
      if (is_empty(poverty_yr_prev) == TRUE) {poverty_yr_prev <- NA} #if prev year empty then NA
      z <- poverty_yr - poverty_yr_prev # Difference in poverty (or NA)

      #Organize outputs
      Poverty_Change <- c(Poverty_Change, z)
      Poverty_df <- rbind(Poverty_df, 
                          c(i, y, z)) # school name, year, change in % poverty
    }
}

# Clean Povety_df (change in percent poverty for each school for every year) 
# and combine with newSchoolDemo
Poverty_df <- as_tibble(Poverty_df)
Poverty_df<- Poverty_df[-1,] 
colnames(Poverty_df) <- c("School", "Year", "Change_perc_poverty")
newSchoolDemo <- left_join(newSchoolDemo, Poverty_df, by = c("School", "Year"))

# Count duplicates (how many years data at each school)
Duplicates_df <- tibble(
  School = unique(newSchoolDemo$School),
  Duplicates = newSchoolDemo$School %>% as_factor() %>% tabulate() 
)
newSchoolDemo <- left_join(newSchoolDemo, Duplicates_df, by = "School")

# DATA IS CLEAN NOW!!
#analyzzzze

#First look: Enrollment
ggplot(newSchoolDemo, aes(x = Year, y = Enrollment, group = School)) +
  geom_point(alpha = .05)

# Zoomed in and connecting each school over years
# ENROLLMENT
ggplot(newSchoolDemo, aes(x = Year, y = Enrollment, group = School)) +
  geom_point(alpha = .01) +
  geom_line(alpha = .01, size= 1.1) +
  ylim(0, 1500)
  # Not much going on here


# POVERTY
#First look
ggplot(newSchoolDemo, aes(x = Year, y = Poverty, group = School)) +
  geom_point(alpha = .05)
#Zoomed in
# Made the lines darker and skinnier
ggplot(newSchoolDemo, aes(x = Year, y = Poverty, group = School)) +
  geom_line(alpha = .1, size= .2) +
  ylim(.5,.95)
  # Poverty is very high and does not seem to increase or change much
  ggplot(newSchoolDemo, aes(x = Year, y = Poverty, group = School)) +
  geom_line(alpha = .2, size= .2) +
  ylim(.8,.95) #same
#CHANGE IN POVERTY
  ggplot(newSchoolDemo, aes(x = Year, y = Change_perc_poverty, group = School)) +
    geom_line(alpha = .2, size= .2) # idk tf
  
  #RACE yeehaw
# https://open.spotify.com/track/2M4gBVSuTLI1GIcM9vL6DH?si=f85a0dff3c304d7b
  # BLACK
  bd <- ggplot(newSchoolDemo, aes(x = Year, y = Black, group = School)) +
    geom_line(alpha = .1, size= .2) +
    ylim(0,1)# wow super low :(
  # ASIAN
  ad <- ggplot(newSchoolDemo, aes(x = Year, y = Asian, group = School)) +
    geom_line(alpha = .1, size= .2) +
    ylim(0,1)
  #HISPANIC
  hd <- ggplot(newSchoolDemo, aes(x = Year, y = Hispanic, group = School)) +
    geom_line(alpha = .1, size= .2) +
    ylim(0,1)
  # WHITE
  wd <- ggplot(newSchoolDemo, aes(x = Year, y = White, group = School)) +
    geom_line(alpha = .1, size= .2) +
    ylim(0,1)
  
install.packages("patchwork")
library(patchwork)
# GRAPHING ALL RACES
bd + ad + hd + wd
# Not sure what to make of these

# HOW TO USE TAPPLY
data("iris")
head(iris)
tapply(iris$Sepal.Width, iris$Species, median)
# Average of Black students
tapply(newSchoolDemo$Black, newSchoolDemo$School, median) %>% head
  newSchoolDemo$Black[newSchoolDemo$School == "A-Tech High School"] %>% median() #check
# FOR CAPTURE AVERAGE % RACE FOR EACH YEAR
Average_per_race <- vector(length = 5)
for (i in c("Black", "White", "Asian", "Hispanic")) {
  d <- tapply( as_vector(newSchoolDemo[,i]), newSchoolDemo$Year, median)
  new_row <- c(race = i, d)
  Average_per_race <- rbind(Average_per_race, new_row)
}
Average_per_race <- Average_per_race[-1,] 
Average_per_race <- as_tibble(Average_per_race)
# Buuuutt we want years as their own column
# and all races combined into one column for pretty graphs
Average_per_race <- pivot_longer(Average_per_race, cols = 2:6, names_to = "year", values_to = "avg") #sexy
Average_per_race$avg <- as.numeric(Average_per_race$avg)
# PLOT
ggplot(Average_per_race, aes(x = year, y = avg, group = race, color = race)) +
  geom_line() 

ggplot() + 
  geom_line(data = newSchoolDemo, aes(x = Year, y = White, group = School), alpha = .1, size= .2) +
  geom_line(data = newSchoolDemo, aes(x = Year, y = Asian, group = School), alpha = .1, size= .2) +
  geom_line(data = newSchoolDemo, aes(x = Year, y = Black, group = School), alpha = .1, size= .2) +
  geom_line(data = newSchoolDemo, aes(x = Year, y = Hispanic, group = School), alpha = .1, size= .2) +
  geom_line(data = Average_per_race, aes(x = year, y = avg, group = race, color = race)) + 
  ylim(0,1)
# Wow we can see each school and the averages (Median remember) for each race super imposed.

# Divided by color? 
ggplot() + 
  geom_line(data = newSchoolDemo, aes(x = Year, y = White, group = School), alpha = .05, size= .2, color = "Purple") +
  geom_line(data = newSchoolDemo, aes(x = Year, y = Asian, group = School), alpha = .05, size= .2, color = "Red") + 
  geom_line(data = newSchoolDemo, aes(x = Year, y = Black, group = School), alpha = .05, size= .2, color = "Green") +
  geom_line(data = newSchoolDemo, aes(x = Year, y = Hispanic, group = School), alpha = .05, size= .2, color = "Blue") 
#Not super useful



  


# LOOK AT OTHER VARIABLES IN DATASET!!!!!!!!!!!1


SchoolDemo
colnames(SchoolDemo) <- colnames(SchoolDemo) %>% 
  str_replace("%", "Percent") %>% 
  str_replace("#", "Numbr") %>% 
  str_replace_all(" ", "_")
SchoolDemo$Percent_Poverty <- parse_number(SchoolDemo$Percent_Poverty)/100
colnames(SchoolDemo)

# Pre-k vs enrollment
lm_gk <- lm(Total_Enrollment ~ Grade_K, data = SchoolDemo)
summary(lm_gk)
plot(SchoolDemo$Grade_K, SchoolDemo$Total_Enrollment, col = alpha("orange", .2))
abline(lm_gk, col = "red")
# Adjusted R-squared:  0.042 
# Estimate Std. Error t value Pr(>|t|)    
#   Grade_K       2.0213     0.1003   20.16   <2e-16 ***

# Grade 12 vs enrollment
lm_g12 <- lm(Total_Enrollment ~ Grade_12, data = SchoolDemo)
summary(lm_g12)
plot(SchoolDemo$Grade_12, SchoolDemo$Total_Enrollment, col = alpha("purple", .2))
# Adjusted R-squared:  0.4319 
# Estimate Std. Error t value Pr(>|t|)    
#   Grade_12      2.82894    0.03373   83.86   <2e-16 ***

#Comparing enrollment of each race (Each point represents a school)
bvh <- ggplot(SchoolDemo, aes(y= Percent_Black, x= Percent_Hispanic, color = Year)) +  geom_point(alpha = .2)
wva <- ggplot(SchoolDemo, aes(x = Percent_White, y = Percent_Asian, color = Year)) +  geom_point(alpha = .2)
bvw <- ggplot(SchoolDemo, aes(y = Percent_Black, x = Percent_White, color = Year)) +  geom_point(alpha = .2)
hva <- ggplot(SchoolDemo, aes(y= Percent_Hispanic, x= Percent_Asian, color = Year)) +   geom_point(alpha = .2)
bvh + bvw + hva + wva
#White and Asian students tend to go to school where population of Black and Hispanic students are lower 
# and where their white/asian counterparts are higher.
# Either Black and Hispanic students are segregating themselves or....

#Hispanic versus black student percent
lm_bh <- lm(Percent_Hispanic ~ Percent_Black, data = SchoolDemo)
summary(lm_bh)
plot(SchoolDemo$Percent_Black, SchoolDemo$Percent_Hispanic, col = alpha("purple", .2))
abline(lm_bh)
# Multiple R-squared:  0.1827,	Adjusted R-squared:  0.1826 
#   Estimate Std. Error t value Pr(>|t|)    
#   Percent_Black -0.408187   0.008978  -45.47   <2e-16 ***


#White versus Asian
lm_wa <- lm(Percent_Asian ~ Percent_White, data = SchoolDemo)
summary(lm_wa)
plot(SchoolDemo$Percent_White, SchoolDemo$Percent_Asian, col = alpha("purple", .2))
abline(lm_wa)
#   Estimate Std. Error t value Pr(>|t|)    
#   Percent_White 0.196672   0.009726   20.22   <2e-16 ***
# Multiple R-squared:  0.04234,	Adjusted R-squared:  0.04223


# Lets look at Native Americans and poverty
# https://open.spotify.com/track/2lJH4jsHg7CgxtaNjIJa95?si=ed4eb931cff541ee
pva <- ggplot(SchoolDemo, aes(x = Percent_Poverty, y = Percent_Asian, color = Year)) +  geom_point(alpha = .4)
pvh <- ggplot(SchoolDemo, aes(x= Percent_Poverty, y= Percent_Hispanic, color = Year)) +  geom_point(alpha = .4) 
pvw <- ggplot(SchoolDemo, aes(x = Percent_Poverty, y = Percent_White, color = Year)) +  geom_point(alpha = .4)
pvb <- ggplot(SchoolDemo, aes(x= Percent_Poverty, y= Percent_Black, color = Year)) +   geom_point(alpha = .4)
pva + pvh + pvw + pvb 

#White
lm_povertyW <- lm(Percent_White ~ Percent_Poverty, data = SchoolDemo)
summary(lm_povertyW)
plot(SchoolDemo$Percent_Poverty, SchoolDemo$Percent_White, col = alpha("purple", .2))
abline(lm_povertyW)
# Multiple R-squared:  0.6417,	Adjusted R-squared:  0.6417 
#   Estimate Std. Error t value Pr(>|t|)    
#   Percent_Poverty -7.309e-03  5.679e-05  -128.7   <2e-16 ***

#Hispanic
lm_povertyH <- lm(Percent_Hispanic ~ Percent_Poverty, data = SchoolDemo)
summary(lm_povertyH)
plot(SchoolDemo$Percent_Poverty, SchoolDemo$Percent_Hispanic, col = alpha("purple", .2))
abline(lm_povertyH)
# Multiple R-squared:  0.2334,	Adjusted R-squared:  0.2333 
#   Estimate Std. Error t value Pr(>|t|)    
#   Percent_Poverty  0.633256   0.011934  53.065  < 2e-16 ***

#Black
lm_povertyB <- lm(Percent_Black ~ Percent_Poverty, data = SchoolDemo)
summary(lm_povertyB)
plot(SchoolDemo$Percent_Poverty, SchoolDemo$Percent_Black, col = alpha("purple", .2))
abline(lm_povertyB)
# Multiple R-squared:  0.09512,	Adjusted R-squared:  0.09502 
# Estimate Std. Error t value Pr(>|t|)    
# Percent_Poverty  0.42330    0.01358   31.18   <2e-16 ***

#Asian
lm_povertyA <- lm(Percent_Asian ~ Percent_Poverty, data = SchoolDemo)
summary(lm_povertyA)
plot(SchoolDemo$Percent_Poverty, SchoolDemo$Percent_Asian, col = alpha("purple", .2))
abline(lm_povertyA)
# Multiple R-squared:  0.08515,	Adjusted R-squared:  0.08505 
# Estimate Std. Error t value Pr(>|t|)    
#   Percent_Poverty -0.254471   0.008673  -29.34   <2e-16 ***

#Native-American
lm_povertyNA <- lm(Percent_Native_American ~ Percent_Poverty, data = SchoolDemo)
summary(lm_povertyNA)
plot(SchoolDemo$Percent_Poverty, SchoolDemo$Percent_Native_American, col = alpha("purple", .2))
abline(lm_povertyNA)
#Taking a closer look at Native American school (Usually less than 10% of population)
plot(SchoolDemo$Percent_Poverty[SchoolDemo$Percent_Native_American <.1], SchoolDemo$Percent_Native_American[SchoolDemo$Percent_Native_American <.1], col = alpha("purple", .2))
# Multiple R-squared:  0.005395,	Adjusted R-squared:  0.005287 
# Estimate Std. Error t value Pr(>|t|)    
#   Percent_Poverty 0.0080519  0.0011368   7.083 1.52e-12 ***
