library(tidyverse)

#load and merge data ----
##Team Populism data from Hawkins, Kirk A., Rosario Aguilar, Erin Jenne, Bojana Kocijan, Cristóbal Rovira Kaltwasser, Bruno Castanho Silva. 2019.
##Global Populism Database: Populism Dataset for Leaders 1.0.
##Available for download at populism.byu.edu
pop <- read_csv('team_populism_prepared.csv',
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
fraser <- read_csv('EFW_panel_data.csv',
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

 ##for cases with missing data, interpolate by comparing prior and subsequent fraser efi scores
temp2000 <- fraser %>% filter(year == 2000) %>% select(year, iso_code, efi, area1, area2, area3, area4, area5)
temp1995 <- fraser %>% filter(year == 1995)
interpolated_fraser_98 <- temp1995 %>%
  left_join(temp2000, by = 'iso_code', suffix = c('.95', '.00')) %>%
  mutate(year = as.integer(1998),
         efi =  ((efi.00 - efi.95) * 0.6) + efi.95,
         area1 = ((area1.00 - area1.95) * 0.6) + area1.95,
         area2 = ((area2.00 - area2.95) * 0.6) + area2.95,
         area3 = ((area3.00 - area3.95) * 0.6) + area3.95,
         area4 = ((area4.00 - area4.95) * 0.6) + area4.95,
         area5 = ((area5.00 - area5.95) * 0.6) + area5.95) %>%
  select(year, iso_code, countries, efi, area1, area2, area3, area4, area5)
interpolated_fraser_99 <- temp1995 %>%
  left_join(temp2000, by = 'iso_code', suffix = c('.95', '.00')) %>%
  mutate(year = as.integer(1999),
         efi =  ((efi.00 - efi.95) * 0.8) + efi.95,
         area1 = ((area1.00 - area1.95) * 0.8) + area1.95,
         area2 = ((area2.00 - area2.95) * 0.8) + area2.95,
         area3 = ((area3.00 - area3.95) * 0.8) + area3.95,
         area4 = ((area4.00 - area4.95) * 0.8) + area4.95,
         area5 = ((area5.00 - area5.95) * 0.8) + area5.95) %>%
  select(year, iso_code, countries, efi, area1, area2, area3, area4, area5)

fraser <- fraser %>% 
  rbind(interpolated_fraser_98[,]) %>%
  rbind(interpolated_fraser_99[,]) 

##combine datasets
pef <- fraser %>%
  inner_join(pop, by = c('iso_code', 'year' = 'year_begin'))


n_distinct(pef$leader)
n_distinct(pef$iso_code)
summary(pef$year)
summary(pef$year_end)

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


#note length of tenure and number of terms
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
pef[1, 'cumulative_efi_change'] <- pef[1,'efi_change']
pef[1, 'cumulative_area1_change'] <- pef[1,'area1_change']
pef[1, 'cumulative_area2_change'] <- pef[1,'area2_change']
pef[1, 'cumulative_area3_change'] <- pef[1,'area3_change']
pef[1, 'cumulative_area4_change'] <- pef[1,'area4_change']
pef[1, 'cumulative_area5_change'] <- pef[1,'area5_change']

for (i in 0:(nrow(pef)-1)){
  i = i+1
  pef[i, 'cumulative_tenure'] <-
    if_else(as.integer(pef[i, 'term_number']) > 1,                                         #if multiple terms...
            as.integer(pef[(i-1), 'cumulative_tenure']) + as.integer(pef[i, 'tenure']),  #...then add combined tenure
            as.integer(pef[1, 'tenure']))
  pef[i, 'cumulative_efi_change'] <-
    if_else(as.integer(pef[i, 'term_number']) > 1,                                               #if multiple terms...
            as.numeric(pef[(i-1), 'cumulative_efi_change']) + as.numeric(pef[i, 'efi_change']),  #...then add combined change
            as.numeric(pef[i, 'efi_change']))
  pef[i, 'cumulative_area1_change'] <-
    if_else(as.integer(pef[i, 'term_number']) > 1,                                                   #if multiple terms...
            as.numeric(pef[(i-1), 'cumulative_area1_change']) + as.numeric(pef[i, 'area1_change']),  #...then add combined change
            as.numeric(pef[i, 'area1_change']))
  pef[i, 'cumulative_area2_change'] <-
    if_else(as.integer(pef[i, 'term_number']) > 1,                                                   #if multiple terms...
            as.numeric(pef[(i-1), 'cumulative_area2_change']) + as.numeric(pef[i, 'area2_change']),  #...then add combined change
            as.numeric(pef[i, 'area2_change']))
  pef[i, 'cumulative_area3_change'] <-
    if_else(as.integer(pef[i, 'term_number']) > 1,                                                   #if multiple terms...
            as.numeric(pef[(i-1), 'cumulative_area3_change']) + as.numeric(pef[i, 'area3_change']),  #...then add combined change
            as.numeric(pef[i, 'area3_change']))
  pef[i, 'cumulative_area4_change'] <-
    if_else(as.integer(pef[i, 'term_number']) > 1,                                                   #if multiple terms...
            as.numeric(pef[(i-1), 'cumulative_area4_change']) + as.numeric(pef[i, 'area4_change']),  #...then add combined change
            as.numeric(pef[i, 'area4_change']))
  pef[i, 'cumulative_area5_change'] <-
    if_else(as.integer(pef[i, 'term_number']) > 1,                                                   #if multiple terms...
            as.numeric(pef[(i-1), 'cumulative_area5_change']) + as.numeric(pef[i, 'area5_change']),  #...then add combined change
            as.numeric(pef[i, 'area5_change']))
  
}


##note starting efi in each leader's first term
first_terms <- pef %>% filter(term_number == 1) %>% select(leader, first_efi = efi)

pef <- pef %>% left_join(first_terms, by = 'leader')

##check general trends in efi components during tenures of populism database leaders
lapply(pef[,27:32], FUN = mean)

ggplot(data = pef)+
  geom_density(aes(x = efi_change))+
  geom_density(aes(x = area1_change), color = 'red')+
  geom_density(aes(x = area2_change), color = 'yellow')+
  geom_density(aes(x = area3_change), color = 'green')+
  geom_density(aes(x = area4_change), color = 'blue')+
  geom_density(aes(x = area5_change), color = 'violet')


#Analyze relationship between populism and economic freedom ----

pef %>% filter(populism_score >= 0.8) %>%
  summarise(avg_effect = mean(efi_change), count= n())
summary(lm(efi_change ~ term_number, data = (pef %>% filter(populism_score >= 0.8))))
summary(lm(efi_change ~ cumulative_tenure, data = (pef %>% filter(populism_score >= 1))))

summary(lm(efi_change ~ populism_score + term_number, data = pef))

summary(lm(efi_change ~ populism_score + efi, data = pef))

##chart populism against efi, split by ideology
ggplot(data = pef, aes(x = efi_change, y = populism_score))+
  geom_point(aes(color = ideology))+
  geom_smooth(aes(color = ideology), se = FALSE)

##chart efi change by term number (but few populists with many terms)
ggplot(data = (pef %>% filter(populism_score >=0.8)),
       aes(x = term_number, y = efi_change))+
  geom_point()+
  geom_smooth()

## focus on area 2, property rights
pef %>% filter(populism_score >= 0.8) %>%
  summarise(avg_effect = mean(area2_change), count= n())
summary(lm(area2_change ~ populism_score, data = pef))

ggplot(data = pef, aes(x = area2_change, y = populism_score))+
  geom_point(aes(color = ideology))+
  geom_smooth(aes(color = ideology), se = FALSE)

ggplot(data = pef, aes(y = populism_score))+
  geom_smooth(aes(x = area1_change), color = 'red', se = FALSE)+
  geom_smooth(aes(x = area2_change), color = 'yellow', se = FALSE)+
  geom_smooth(aes(x = area3_change), color = 'green', se = FALSE)+
  geom_smooth(aes(x = area4_change), color = 'blue', se = FALSE)+
  geom_smooth(aes(x = area5_change), color = 'violet', se = FALSE)


ggplot(data = pef, aes(x = area3_change, y = populism_score))+
  geom_point()+
  facet_wrap('ideology')

summary(lm(area3_change ~ populism_score + cumulative_tenure + ideology, data = pef))

ggplot(data = pef, aes(x = area5_change, y = populism_score))+
  geom_point(aes(color = ideology))

ggplot(data = pef,
         aes(x=cumulative_tenure, y = efi_change, color = populism_score >= 0.8))+
  geom_point()+
  geom_smooth(span = 5, se = FALSE)



ggplot(data = (pef %>% mutate(populist = (populism_score >=0.8))),
       aes(x =efi, y = efi_change))+
  geom_point(aes(color = ideology))+
  geom_smooth()+
  geom_hline(yintercept = 0)+
  facet_wrap('populist')


ggplot(data = (pef %>% mutate(populist = (populism_score >=0.8))),
       aes(x = efi, y = cumulative_efi_change))+
  geom_point(aes(color = populist))+
  geom_smooth(aes(color = populist), method = 'lm')+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  labs(title = 'Economic Freedom Stalls Under Populist Leaders')

pef %>% nrow()
pef %>% filter(term_number == total_terms) %>% nrow()

## hunt for break point to group populists and non-populists
ggplot(data = pef, aes(x = populism_score))+
  geom_histogram(binwidth = 0.1)
##resume main analysis


## repeat with only one observation per leader ----
pop_colors <- c(populist = 'firebrick', nonpopulist = 'lightsteelblue')

ggplot(data = (pef %>% filter(term_number == total_terms) %>% mutate(populist = if_else(populism_score >=0.8, 'populist', 'nonpopulist'))),
       aes(x = first_efi, y = cumulative_efi_change))+
  geom_hline(yintercept = 0)+
  geom_point(aes(color = populist), size = 2)+
  geom_smooth(aes(color = populist), method = 'loess', span = 1.3, se = FALSE, size = 2)+
  scale_color_manual(values = pop_colors)+
#  facet_wrap('populist')+
  theme_minimal()+
  scale_y_continuous(limits = c(-2.5, 2.1))+
  labs(title = 'Populists Might Stall Economic Freedom',
       subtitle = '  as measured by Fraser Index during tenure',
       x = 'Economic Freedom at Outset',
       y = 'Change in Economic Freedom')+
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 18),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text = element_text(size = 16),
        strip.text = element_text(size = 18),
        panel.grid.minor = element_blank(),
        legend.position = 'none')+
  annotate("text", x = 4.5, y = 1.4, color = 'steelblue', size = 6, label = 'Non-populist')+
  annotate("text", x = 4.5, y = -1.4, color = 'firebrick', size = 6, label = 'Populist')
 
ggsave(filename = "populist_affect_econ_freedom.jpg",
       width = 10,
       height = 6,
       units = 'in')
 
 ##look at components - none seems to singly drive the overall result
ggplot(data = (pef %>% filter(term_number == total_terms) %>% mutate(populist = if_else(populism_score >=0.8, 'populist', 'nonpopulist'))),
       aes(x = efi, y = cumulative_area1_change))+
  geom_point(aes(color = populist), size = 2)+
  geom_smooth(aes(color = populist), method = 'loess', se = FALSE, size = 1.5)+
  scale_color_manual(values = pop_colors)+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  theme(legend.position = 'none')
ggplot(data = (pef %>% filter(term_number == total_terms) %>% mutate(populist = if_else(populism_score >=0.8, 'populist', 'nonpopulist'))),
       aes(x = efi, y = cumulative_area2_change))+
  geom_point(aes(color = populist), size = 2)+
  geom_smooth(aes(color = populist), method = 'loess', se = FALSE, size = 1.5)+
  scale_color_manual(values = pop_colors)+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  theme(legend.position = 'none')
ggplot(data = (pef %>% filter(term_number == total_terms) %>% mutate(populist = if_else(populism_score >=0.8, 'populist', 'nonpopulist'))),
       aes(x = efi, y = cumulative_area3_change))+
  geom_point(aes(color = populist), size = 2)+
  geom_smooth(aes(color = populist), method = 'loess', se = FALSE, size = 1.5)+
  scale_color_manual(values = pop_colors)+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  theme(legend.position = 'none')
ggplot(data = (pef %>% filter(term_number == total_terms) %>% mutate(populist = if_else(populism_score >=0.8, 'populist', 'nonpopulist'))),
       aes(x = efi, y = cumulative_area4_change))+
  geom_point(aes(color = populist), size = 2)+
  geom_smooth(aes(color = populist), method = 'loess', se = FALSE, size = 1.5)+
  scale_color_manual(values = pop_colors)+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  theme(legend.position = 'none')
ggplot(data = (pef %>% filter(term_number == total_terms) %>% mutate(populist = if_else(populism_score >=0.8, 'populist', 'nonpopulist'))),
       aes(x = efi, y = cumulative_area5_change))+
  geom_point(aes(color = populist), size = 2)+
  geom_smooth(aes(color = populist), method = 'loess', se = FALSE, size = 1.5)+
  scale_color_manual(values = pop_colors)+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  theme(legend.position = 'none')


pef %>% arrange(cumulative_efi_change) %>% select(country, leader, cumulative_efi_change, ideology, year, first_efi) %>% head(10)


##rate of populism over time (subject to selection bias in dataset)
ggplot(data = pef, aes(x = year, y = populism_score))+
  geom_point(position = 'jitter')+
  geom_smooth()

ggplot(data = pef, aes(x = year, y = populism_score))+
  geom_point(aes(color = region), position = 'jitter')+
  geom_smooth(aes(color = region), se = FALSE)

################# Project notes ###################
#for analysis, consider effects of ideology, length of tenure, term number, region?
#  
# Filter at outset to only democracies?  would need to link to BMR, I guess
#
#how to deal with too small n? 
#  
#  seems that most important control variable is starting efi
#
#may be better to take leader as unit of analysis, instead of term
#
#Reference:
#  Area 1: Size of Government—
#government spending, taxation, and the size of government-controlled enterprises 
#Area 2: Legal System and Property Rights
#Protection of persons and their rightfully acquired property
#Area 3: Sound Money
#Inflation height and volatility 
#Area 4: Freedom to Trade Internationally
#Area 5: Regulation
#right to exchange, gain credit, hire or work for whom you wish, or freely operate your business
#
################# end project notes ###################
