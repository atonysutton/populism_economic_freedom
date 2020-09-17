library(tidyverse)

#load and merge data ----
##Team Populism data from Hawkins, Kirk A., Rosario Aguilar, Erin Jenne, Bojana Kocijan, Crist�bal Rovira Kaltwasser, Bruno Castanho Silva. 2019.
 ##Global Populism Database: Populism Dataset for Leaders 1.0.
 ##Available for download at populism.byu.edu
pop <- read_csv('C:/Tony/Political Science MA/data/Team Populism/team_populism_prepared.csv',
                col_types = "ccccccdinfii") %>%
  rename_all(tolower)

pop <- pop %>% rename(populism_score = average_score)

skimr::skim(pop)

##locate one row with impossible data: tenure ends before it begins
pop %>% mutate(time_travel = year_end - year_begin) %>% filter(time_travel < 0) %>% select(year_begin, year_end, iso_code, country, leader)
##manually change value to 2003, per external sources on Noboa's tenure
pop$year_end <- if_else(pop$year_begin == 2000 & pop$iso_code == 'ECU',
                        as.integer(2003),
                        pop$year_end) 

##recode into left-right-center
pop <- pop %>% mutate(ideology = fct_recode(as.factor(left_right),
                                            center = '0',
                                            left = '-1',
                                            right = '1'))


##Fraser data from Fraser Institute's Economic Freedom Index,
 ##downloaded from https://www.fraserinstitute.org/studies/economic-freedom-of-the-world-2020-annual-report
 ##on 14 September 2020
 ##see also James Gwartney, Robert Lawson, Joshua Hall, and Ryan Murphy
 ##Economic Freedom of the World Annual Report 2020
fraser <- read_csv('C:/Tony/Political Science MA/data/Fraser economic freedom/EFW_panel_data.csv',
                col_types = 'iccdddddd') %>%
  rename_all(tolower)

skimr::skim(fraser)

##rename last year of tenure for leaders still in office
##last available year for efi is 2018; therefore, set all current and recent leaders (1899 and 2019) to 2018
pop_last <- max(pop$year_end)
efi_last <- max(fraser$year)
pop <- pop %>% filter(year_begin <= (efi_last - 1))
pop$year_end <- as.integer(if_else(pop$year_end %in% c(1899, 2019), as.numeric(efi_last), as.numeric(pop$year_end)))
summary(pop$year_end)


pef <- fraser %>%
  inner_join(pop, by = c('iso_code', 'year' = 'year_begin'))


##note length of tenure and number of terms
pef <- pef %>% mutate(tenure = year_end - year)

pef <- pef %>% arrange(leader, year)
pef <- pef %>% group_by(leader) %>% mutate(total_terms = n()) %>% ungroup()


pef[1, 'term_number'] <- 1
for (i in 1:(nrow(pef)-1)){
  i = i+1
  pef[i, 'term_number'] <-
    if_else(as.character(pef[i, 'leader']) == as.character(pef[(i-1), 'leader']),  #if the prior row is the same leader...
            as.integer(pef[(i-1), 'term_number']) + 1,                             #...then count an additional term
            1)
}


pef[1, 'cumulative_tenure'] <- as.integer(pef[1, 'tenure'])
for (i in 1:(nrow(pef)-1)){
  i = i+1
  pef[i, 'cumulative_tenure'] <-
    if_else(as.integer(pef[i, 'term_number']) > 1,                                         #if multiple terms...
            as.integer(pef[(i-1), 'cumulative_tenure']) + as.integer(pef[(i), 'tenure']),  #...then add combined tenure
            as.integer(pef[1, 'tenure']))
}


#add column for economic freedom index at end of tenure ----

##functions to find efi and its components at end

find_efi_end <- function(iso_var, year_end_var, efi_df, component){
  q_component <- enquo(component)
  answer <- efi_df %>% filter(iso_code == iso_var, year == year_end_var) %>% select(output = !!q_component)
  return(answer$output)
}

##loop over each row (tenure) in pef dataframe
pef$efi_end <- as.numeric(NA)
pef$area1_end <- as.numeric(NA)
pef$area2_end <- as.numeric(NA)
pef$area3_end <- as.numeric(NA)
pef$area4_end <- as.numeric(NA)
pef$area5_end <- as.numeric(NA)

for (i in seq_len(nrow(pef))){
  pef$efi_end[i] <- find_efi_end(iso_var = pef$iso_code[i],
                                 year_end_var = pef$year_end[i],
                                 efi_df = fraser,
                                 component = efi)
  pef$area1_end[i] <- find_efi_end(iso_var = pef$iso_code[i],
                                   year_end_var = pef$year_end[i],
                                   efi_df = fraser,
                                   component = area1)
  pef$area2_end[i] <- find_efi_end(iso_var = pef$iso_code[i],
                                   year_end_var = pef$year_end[i],
                                   efi_df = fraser,
                                   component = area2)
  pef$area3_end[i] <- find_efi_end(iso_var = pef$iso_code[i],
                                   year_end_var = pef$year_end[i],
                                   efi_df = fraser,
                                   component = area3)
  pef$area4_end[i] <- find_efi_end(iso_var = pef$iso_code[i],
                                   year_end_var = pef$year_end[i],
                                   efi_df = fraser,
                                   component = area4)
  pef$area5_end[i] <- find_efi_end(iso_var = pef$iso_code[i],
                                   year_end_var = pef$year_end[i],
                                   efi_df = fraser,
                                   component = area5)
}

summary(pef$efi_end)
summary(pef$area1_end)
summary(pef$area2_end)
summary(pef$area3_end)
summary(pef$area4_end)
summary(pef$area5_end)


#Calculate change in EFI and its components during each tenure ----
pef <- pef %>% mutate(efi_change = efi_end - efi,
                      area1_change = area1_end - area1,
                      area2_change = area2_end - area2,
                      area3_change = area3_end - area3,
                      area4_change = area4_end - area4,
                      area5_change = area5_end - area5,)

##check general trends in efi components during tenures of populism database leaders
lapply(pef[,27:32], FUN = mean)

ggplot(data = pef)+
  geom_density(aes(x = efi_change))+
  geom_density(aes(x = area1_change), color = 'red')+
  geom_density(aes(x = area2_change), color = 'yellow')+
  geom_density(aes(x = area3_change), color = 'green')+
  geom_density(aes(x = area4_change), color = 'blue')+
  geom_density(aes(x = area5_change), color = 'violet')
 
 
#Analyze relationship between populism and economic freedom

pef %>% filter(populism_score >= 0.8) %>% summarise(avg_effect = mean(efi_change), count= n())
summary(lm(efi_change ~ term_number, data = (pef %>% filter(populism_score >= 0.8))))
summary(lm(efi_change ~ cumulative_tenure, data = (pef %>% filter(populism_score >= 1))))

summary(lm(efi_change ~ populism_score + term_number, data = pef))


ggplot(data = pef, aes(x = efi_change, y = populism_score))+
  geom_point(aes(color = ideology))+
  geom_smooth(aes(color = ideology), se = FALSE)


#Project notes
for analysis, consider effects of ideology, length of tenure, term number, region?

Filter at outset to only democracies?  would need to link to BMR, I guess


