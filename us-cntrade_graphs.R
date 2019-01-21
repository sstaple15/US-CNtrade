# Data Visualization: Victims of US-China Trade War
# Code for second week of data viz portfolio project:
# Data exploration and 3 graphs with narratives
# Author: Stephen Stapleton

library(tidyverse)
library(ggplot2)
library(zoo)
library(dplyr)
library(here)

#########################################################
# Create intro line graph detailing US-China trade balance
#########################################################

# Source: https://www.census.gov/foreign-trade/balance/index.html
USall <- read_csv('country.csv')

# Parse dates for use in ggplot
USall$new_date <- as.Date(as.yearmon(USall$date))

# Use only if interested in cumulative trade deficit
#USall %<>%
#  arrange(cty_code,new_date) %>%
#  group_by(cty_code) %>%
#  mutate(roll_def = cumsum(deficit))

# Redefine variable deficit to trade balance
USall$trd_bal <- -USall$deficit

# Restrict to years 1985-2017 for yearly total
USall <- USall[USall$new_date < '2018-09-01',]

# Exclude regional trade comparisons
USuni <- USall[USall$cty_code > 1000,]

# Summarize over year to adjust for seasonal spikes
USyr <- USuni %>%
  group_by(cty_code, year) %>%
  summarize(trd_bal = sum(trd_bal))

# Prepare for ggplot
USyr$is_china <- ifelse(USyr$cty_code == 5700, 1, 0)
USyr$trd_bal <- USyr$trd_bal/1000
USyr <- USyr[USyr$year < 2018,]

# Plot trade balance per country with coloring and alpha aesthetics
# Create non-aesthetic xintercept for WTO adoption by China
# Employ geom_line(), geom_vline()
# Adjust scaling for both x and y axis
ggplot(USyr, aes(x = year, y = trd_bal,
  color = as.factor(cty_code), alpha = as.factor(is_china))) +
  geom_line() +
  geom_vline(xintercept = 2001, linetype = 'dashed', color = 'turquoise') +
  scale_alpha_manual(values = c(.4, 1)) +
  scale_color_manual(values = c(rep('grey', 150), 'red', rep('grey', 93))) +
  scale_x_continuous(breaks = seq(1985, 2017, 5), expand = c(0,0)) +
  theme_classic() +
  theme(legend.position = 'none') +
  ylim(-400,100) +
  labs(x = 'Year', y = 'Trade Balance (billions $USD)',
       title = 'The US-China trade gap stands out among US trade relations',
       subtitle = 'In nominal terms, import-export deficit has widened since China entered the World Trade Organization in December 2001.', 
       caption = 'Data Source: United States Census Bureau')

# Close out this section, clean house
ggsave('us_balance.pdf')
rm(USall)
rm(USuni)
rm(USyr)

#########################################################
# Create bubble graph of by-state trade relation to China
#########################################################

library(readxl)

# source: https://www.census.gov/foreign-trade/statistics/state/data/index.html
EXall <- read_excel('exctyall.xls',
            range = cell_rows(5:1436),
            col_types = c(rep('text', 3), rep('numeric', 9))
            )
IMall <- read_excel('imctyall.xls',
            range = cell_rows(5:1436),
            col_types = c(rep('text', 3), rep('numeric', 9))
            )

# extract only 2017 imports/exports
IMall$IMval2017 <- IMall$val2017
EXall$EXval2017 <- EXall$val2017

IMall$IMshr2017 <- IMall$share17
EXall$EXshr2017 <- EXall$share17

IMall$IMrank <- IMall$rank
EXall$EXrank <- EXall$rank

# combine by state and trading country
ImEx <- left_join(IMall, EXall, by = c('statename', 'countryd'))

# subset only chinese imports and exports
ImExCN <- subset(ImEx, (ImEx$countryd %in% 'China'))

# specify if china in top 3 trading partners
ImExCN$trd_main <- ifelse(ImExCN$IMrank %in% c(1:3) & ImExCN$EXrank %in% c(1:3), 'Yes', 'No')

# convert millions to billions for each of graph reading
ImExCN$IMval2017 <- ImExCN$IMval2017/1000
ImExCN$EXval2017 <- ImExCN$EXval2017/1000

ImExCN$IMshr2017 <- ImExCN$IMshr2017/100
ImExCN$EXshr2017 <- ImExCN$EXshr2017/100

# Plot trade balance per state with coloring and size aesthetics
# Create non-aesthetic balanced trade line (dashed turquoise)
# Employ geom_jitter() and geom_abline()
# Adjust scaling for both x and y axis, scale dots
ggplot(ImExCN, aes(x = IMval2017, y = EXval2017,
  fill = as.factor(trd_main), size = IMshr2017)) +
  geom_jitter(pch=21) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank()) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', color = 'turquoise') +
  guides(fill=guide_legend(title="China among top trade partners?")) +
  guides(size=guide_legend(title="Share of total imports from China :")) +
  scale_size_area(max_size = 8) +
  labs(x = 'Imports (billions USD)', y = 'Exports (billions USD)',
       title = 'Most states rely heavily on Chinese market access for imports and exports',
       subtitle = 'In 2017, most states counted China a major trade partner, with California relying heavily on Chinese imports.', 
       caption = 'Data Source: United States Census Bureau')

# save plot and clean house
ggsave('state_balance.pdf')
rm(EXall)
rm(IMall)
rm(ImEx)
rm(ImExCN)
rm(subImEx)

#########################################################
# Create national breakdown of trade by commodity type
#########################################################

# source: https://comtrade.un.org/data/
com <- read_csv('comtrade.csv')

# Remove re-import/export data
com <- subset(com, com$`Trade Flow Code` < 3 & com$`Commodity Code` < 98)

# group export/import data into larger HS code families
# source: https://www.foreign-trade.com/reference/hscode.htm

com$`Commodity Family` <- ifelse(com$`Commodity Code` %in% c(1:5), 'Animal & Animal Products',
                          ifelse(com$`Commodity Code` %in% c(6:15), 'Vegetable Products',
                          ifelse(com$`Commodity Code` %in% c(16:24), 'Foodstuffs',
                          ifelse(com$`Commodity Code` %in% c(25:27), 'Mineral Products',
                          ifelse(com$`Commodity Code` %in% c(28:38), 'Chemicals & Allied Industries',
                          ifelse(com$`Commodity Code` %in% c(39:40), 'Plastics/Rubbers',
                          ifelse(com$`Commodity Code` %in% c(41:43), 'Raw Hides, Skins, Leather, & Furs',
                          ifelse(com$`Commodity Code` %in% c(44:49), 'Wood & Wood Products',
                          ifelse(com$`Commodity Code` %in% c(50:63), 'Textiles',
                          ifelse(com$`Commodity Code` %in% c(64:67), 'Footwear/Headgear',
                          ifelse(com$`Commodity Code` %in% c(68:71), 'Stone/Glass',
                          ifelse(com$`Commodity Code` %in% c(72:83), 'Metals',
                          ifelse(com$`Commodity Code` %in% c(84:85), 'Machinery/Electrical',
                          ifelse(com$`Commodity Code` %in% c(86:89), 'Transportation',
                          ifelse(com$`Commodity Code` %in% c(90:97), 'Miscellaneous',
                                 NA)))))))))))))))
# turn to millions USD
com_fam <- com %>%
  group_by(`Trade Flow`, `Commodity Family`) %>%
  summarize(`Trade Value` = sum(`Trade Value (US$)`))
com_fam$`Trade Value` <- as.numeric(com_fam$`Trade Value`)
com_fam$`Trade Value` <- com_fam$`Trade Value`/1000000

com_fam_ex <- subset(com_fam, com_fam$`Trade Flow` == 'Export')
com_fam_im <- subset(com_fam, com_fam$`Trade Flow` == 'Import')

com_fam_ex <- com_fam_ex[order(com_fam_ex$`Trade Value`),]
com_fam_im <- com_fam_im[order(com_fam_im$`Trade Value`),]

com_fam_ex$`Commodity Family` <- factor(com_fam_ex$`Commodity Family`,
                     levels = com_fam_ex$`Commodity Family`[
                       order(com_fam_ex$`Trade Value`)])

com_fam_im$`Commodity Family` <- factor(com_fam_im$`Commodity Family`,
                                        levels = com_fam_im$`Commodity Family`[
                                          order(com_fam_im$`Trade Value`)])

# Plot comparative trade in commodity groups nationally
library(scales)
ggplot(com_fam_ex, aes(`Commodity Family`, `Trade Value`)) +
  geom_col(aes(fill = `Trade Value`), color = 'black', width = 0.8) +
  coord_flip() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  scale_y_continuous(labels = comma, breaks = seq(0, 30000, 5000), expand = c(0,0)) +
  theme(legend.position = 'none') +
  labs(x = '', y = 'Trade Exports (millions $USD)',
       title = 'US Exports to China consist of machinery and rural goods',
       subtitle = 'As of 2017, US exports to China were concentrated in goods produced by rural and industrial centers.', 
       caption = 'Data Source: UN Comtrade International Trade Statistics Database')

# save plot
ggsave('nation_exports.pdf')

ggplot(com_fam_im, aes(`Commodity Family`, `Trade Value`)) +
  geom_col(aes(fill = `Trade Value`), color = 'black', width = 0.8) +
  coord_flip() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  scale_y_continuous(labels = comma, breaks = seq(0, 300000, 50000), expand = c(0,0)) +
  theme(legend.position = 'none') +
  labs(x = '', y = 'Trade Imports (millions $USD)',
       title = 'US Imports from China are primarily consumer electronics and machines',
       subtitle = 'As of 2017, Chinese imports were heavily concentrated in a few consumer commodity groups.', 
       caption = 'Data Source: UN Comtrade International Trade Statistics Database')

# save plot
ggsave('nation_imports.pdf')

# clean house
rm(com)
rm(com_fam)
rm(com_fam_ex)
rm(com_fam_im)