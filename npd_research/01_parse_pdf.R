library(tidyverse)
library(pdftools)
library(tabulizer)
library(janitor)
library(viridis)
library(scales)
library(shades)

# # Read in using PDF Tools
# # This is messy enough I'm just going to extract specific data manually 
# current_budget_path <- 'https://www.northamptonma.gov/DocumentCenter/View/15238/FY2021-Budget'
# dat <- tabulizer::extract_tables(current_budget_path)
# 
# gen_fund <- rbind(dat[[125]], dat[[126]], dat[[127]])
# colnames(gen_fund) <- c('spend_recipient', '')
# 
# dat2 <- pdf_data(current_budget_path)

# read data
budget_dat <- read_csv('~/Documents/ilellosmith/r_files/npd_analysis/NPD Analysis - City Budget 2009-2021.csv') %>% janitor::clean_names()

# clean data
dat <- budget_dat %>% 
  # remove empty rows
  filter_at('expenditure', Negate(is.na)) %>%
  # convert percentage columns from char to numeric
  mutate(pct_change_fy2020_fy2021 = as.numeric(str_replace(pct_change_fy2020_fy2021, '%', ''))) %>%
  mutate(pct_change_fy2020_fy2021_recalc = as.numeric(str_replace(pct_change_fy2020_fy2021_recalc, '%', ''))) %>%
  # convert columns with $ to numeric
  mutate(proposed_cut = as.numeric(str_replace_all(proposed_cut, '\\$|,', ''))) %>%
  mutate(change_fy2020_fy2021_recalc = as.numeric(str_replace_all(change_fy2020_fy2021_recalc, '\\$|,', ''))) 

# limit to budget data
bud <- dat %>% 
  select(expenditure:x2021) %>%
  pivot_longer(cols = x2009:x2021, 
               names_to = 'year', 
               values_to = 'budget') 

# Line showing increase from 2017
bud %>% 
  filter(expenditure == 'Police') %>%
  mutate(year = as.numeric(str_replace(year, 'x', ''))) %>%
  ggplot(aes(x = year, y = budget)) +
  geom_line(aes(color = expenditure)) +
  ylim(0, 7e6) +
  ylab('City Budget Allocated') +
  xlab('Fiscal Year') +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0))

# Plot Planned vs. edited PD Data
pd1 <- dat %>% 
  filter(expenditure == 'Police') %>%
  pivot_longer(cols = c(x2009:x2021, x2021_recalc)
               , names_to = 'year'
               , values_to = 'budget') %>%
  select(expenditure, year, budget)

# Original Budget
pd_orig <- pd1 %>%
  mutate(plan = 'Original Proposal') %>%
  filter(year != 'x2021_recalc')

# Updated Budget
pd_update <- pd1 %>%
  mutate(plan = 'New Proposal') %>%
  filter(year !='x2021')

pd <- pd_orig %>% 
  rbind(pd_update) %>%
  mutate(year = as.numeric(str_replace_all(year, 'x|_recalc', ''))) 

# Get reference year Spend for comparison
spend_reference <- pd %>%
  filter(plan == 'Original Proposal') %>%
  filter(year == 2009) %>%
  pull(budget)

# Set line parameters
line_size = 2
line_end = 'round'

# Set color parameters
right_end_hex <- "#00204DFF"
left_end_hex <- "#F6DD4DFF"
middle_hex <- "#7C7B78FF"

pd %>% 
  # Layers
  ggplot(aes(x = year, y = budget)) +
  geom_line(aes(color = plan),
            size = line_size,
            lineend = line_end) +
  # geom_segment(aes(x=2009,
  #                  xend=2021,
  #                  y=spend_reference,
  #                  yend=spend_reference), 
  #              size = 1,
  #              lineend = line_end, 
  #              color = brightness("#7C7B78FF", 1.2),
  #              ) +
  # Axes Labels
  labs(
    title = 'Northampton Police Budget 2009-2021',
    caption = '\nThe Northampton City Council voted on 06/19/2020 to decrease the NPD\'s budget \n
by 10% for 2021. Despite this cut, the NPD budget is still $6,049,867... 
     '
  ) +
  xlab('\n Budget Category') +
  ylab('City Budget Allocated') +
  xlab('\n Fiscal Year') +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,7000000)) +
  # Themes
  theme_minimal() +
  theme_minimal() +
  theme(
    text= element_text(color = 'white', size = 15),
    axis.text = element_text(color = 'white'),
    plot.title = element_text(),
    plot.subtitle = element_text(face= 'italic'),
    legend.position = 'top', 
    legend.justification = 'left',
    legend.direction = "horizontal",
    plot.caption = element_text(hjust = 0),
    plot.caption.position = 'plot',
    legend.title = element_blank(),
    axis.title.y = element_blank(), 
    plot.background = element_rect(fill = 'black'),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y  = element_blank()
  )  +
  # scale_color_manual(values = c(brightness(right_end_hex, 1.5),
  #                      left_end_hex),
  #                     labels = c('Original Proposal' = "Original Proposal",
  #                                 'New Proposal' = '10% Decrease'
  #                                 ))
  scale_color_manual(values = c("Original Proposal" =  as.character(brightness(right_end_hex, 1.5)),
                       "New Proposal" = left_end_hex),
                      labels = c('Original Proposal' = "Original Proposal",
                                  'New Proposal' = '10% Decrease'
                                  ))

# Adjusted budget 
adjusted_budget <- dat %>% 
  filter(expenditure == 'Police') %>%
  select('x2021_recalc') %>%
  pull()

# Budget relative to other services
bud %>% 
  filter(expenditure %in% c('Police',
                            'Total Human Services',
                            'Total Culture and Recreation' )) %>%
  mutate(year = as.numeric(str_replace_all(year, 'x|_recalc', ''))) %>%
  filter(year == 2021) %>%
  mutate(budget = case_when(
    expenditure == 'Police' ~ adjusted_budget,
    T ~ budget
  )) %>%
  ggplot() +
  geom_bar(aes(y = budget, x = expenditure, fill = expenditure), 
           stat = 'identity'
           ) +
# Axes Labels 
  labs(
    title = 'Northampton City Budget Allocated in 2021',
    subtitle = '(Under the proposed 10% budget cut to the NPD)',
    caption = '\n ...which makes the NPD Budget: \n
            • 3.9x the Total Human Services budget, and \n
            • 2.9x the Culture and Recreation budget'
  ) +
  xlab('\n Budget Category') +
# Axes Scale
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,6250000)) +
# Themes
  theme_minimal() +
  theme(
    text= element_text(color = 'white', size = 15),
    axis.text = element_text(color = 'white'),
    plot.title = element_text(),
    plot.subtitle = element_text(face= 'italic'),
    axis.title.y = element_blank(), 
    legend.position = 'none',
    plot.background = element_rect(fill = 'black'),
    plot.caption = element_text(hjust = 0),
    plot.caption.position = 'plot'
  #  line = element_blank()
  )  +
  scale_fill_manual(values = c('Police' = as.character(brightness(right_end_hex, 1.5)), 
                                'Total Culture and Recreation' = left_end_hex,
                                'Total Human Services' = as.character(brightness(middle_hex, 1.5))
                     ))

