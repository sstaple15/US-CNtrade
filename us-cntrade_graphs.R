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
  scale_y_continuous(breaks = seq(-400, 100, 100), expand = c(0,0)) +
  theme_classic() +
  annotate("text", x = 2015, y = -300, label = 'China', color = 'red') +
  annotate("text", x = 2000.5, y = -200, label = 'China enters WTO', angle = 90) +
  annotate("rect", xmin = 2008, xmax = 2009, ymin = -400, ymax = 100, alpha = .1) +
  theme(legend.position = 'none') +
  labs(x = 'Year', y = 'Trade Balance (billions $USD)',
       title = 'The US-China trade gap stands out among US trade relationships',
       subtitle = 'In nominal terms, the deficit trends downward after China entered the World Trade Organization in December 2001, \nquadrupling since 2000. While population weighting reduces the deficit somewhat, it does not change this trend.', 
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

# specify if china is top trading partner
ImExCN$trd_main <- ifelse(ImExCN$IMrank %in% c(1:3) & ImExCN$EXrank %in% c(1:3), 'Yes', 'No')

# convert millions to billions for each of graph reading
ImExCN$IMval2017 <- ImExCN$IMval2017/1000
ImExCN$EXval2017 <- ImExCN$EXval2017/1000

ImExCN$IMshr2017 <- ImExCN$IMshr2017/100
ImExCN$EXshr2017 <- ImExCN$EXshr2017/100

# Create relative measure of (imports+exports)/gdp per state
# Source: https://apps.bea.gov/regional/histdata/releases/0718gdpstate/index.cfm
gdp <- read_csv('SQGDP2.csv')
gdp <- subset(gdp, gdp$IndustryId == 1)
colnames(gdp)[2] <- "statename"
ImExCN <- left_join(ImExCN, gdp, by = 'statename')
ImExCN <- subset(ImExCN, !is.na(ImExCN$Unit))
ImExCN <- subset(ImExCN, ImExCN$statename != 'District of Columbia')
ImExCN$share_gdp <- 100*(ImExCN$IMval2017 + ImExCN$EXval2017)/(as.numeric(ImExCN$`2017:Q4`)/1000)
library(ggrepel)

# Plot trade balance per state with coloring and size aesthetics
# Create non-aesthetic balanced trade line (dashed turquoise)
# Employ geom_jitter() and geom_abline()
# Adjust scaling for both x and y axis, scale dots
ggplot(ImExCN, aes(x = IMval2017, y = EXval2017,
  fill = as.factor(trd_main), size = share_gdp)) +
  geom_jitter(pch=21) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank()) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', color = 'turquoise') +
  guides(size=guide_legend(title="% 2017 State GDP")) +
  guides(fill=guide_legend(title="China top partner?")) +
  coord_trans(x="log2") +
  ylim(0,20) +
  theme(legend.position = c(.15, .66), legend.background = element_rect(fill=NA)) +
  scale_fill_manual(values = c('grey', 'red')) +
  geom_text_repel(aes(label=ifelse(share_gdp > 4 & trd_main == "Yes",as.character(statename),'')),hjust=.5, vjust=-2, size = 3) +
  annotate("text", x = 9.25, y = 12.25, angle = 65, label = "Balanced-Trade Line") +
  scale_radius(range = c(1,12)) +
  labs(x = 'Log Imports (billions USD)', y = 'Exports (billions USD)',
       title = 'The states rely heavily on Chinese market access for imports and exports',
       subtitle = 'In 2017, a plurality of states counted China their primary trade partner, with imports and exports \n with China alone representing four to eight percent of state gross domestic product (GDP). \nThe dotted balanced-trade line indicates very few states run a surplus with China.', 
       caption = 'Data Source: United States Census Bureau & Bureau of Economic Analysis')

# save plot and clean house
ggsave('state_balance.pdf')
rm(EXall)
rm(IMall)
rm(ImEx)
rm(ImExCN)
rm(gdp)

#########################################################
# Create national breakdown of trade by commodity type
#########################################################

# source: https://comtrade.un.org/data/
com <- read_csv('comtrade.csv')

# Remove re-import/export data
com <- subset(com, com$`Trade Flow Code` < 3 & com$`Commodity Code` < 98)

# turn to billions USD
com_fam <- com %>%
  group_by(`Trade Flow`, `Commodity Code`) %>%
  summarize(`Trade Value` = sum(`Trade Value (US$)`))
com_fam$`Trade Value` <- as.numeric(com_fam$`Trade Value`)
com_fam$`Trade Value` <- com_fam$`Trade Value`/1000000000

# filter to only top 25 of exports/imports
com_fam_ex <- subset(com_fam, com_fam$`Trade Flow` == 'Export')
com_fam_im <- subset(com_fam, com_fam$`Trade Flow` == 'Import')

com_fam_ex <- com_fam_ex[order(com_fam_ex$`Trade Value`),]
com_fam_im <- com_fam_im[order(com_fam_im$`Trade Value`),]

com_fam_ex$`Commodity Code` <- factor(com_fam_ex$`Commodity Code`,
                          levels = com_fam_ex$`Commodity Code`[
                          order(com_fam_ex$`Trade Value`)])
com_fam_ex <- com_fam_ex[72:96,]

com_fam_im$`Commodity Code` <- factor(com_fam_im$`Commodity Code`,
                          levels = com_fam_im$`Commodity Code`[
                          order(com_fam_im$`Trade Value`)])
com_fam_im <- com_fam_im[72:96,]

# rename, since categories are just horrible
com_fam_ex$Commodity <- ifelse(com_fam_ex$`Commodity Code` == 88, 'Aircraft & Spacecraft',
                        ifelse(com_fam_ex$`Commodity Code` == 84, 'Mechanical Machinery',
                        ifelse(com_fam_ex$`Commodity Code` == 12, 'Oils, Grains, & Straw',
                        ifelse(com_fam_ex$`Commodity Code` == 87, 'Cars, Vehicles, & Parts',
                        ifelse(com_fam_ex$`Commodity Code` == 85, 'Electronics & Equipment',
                        ifelse(com_fam_ex$`Commodity Code` == 90, 'Medical/Scientific Instruments',
                        ifelse(com_fam_ex$`Commodity Code` == 27, 'Mineral Fuels & Products',
                        ifelse(com_fam_ex$`Commodity Code` == 39, 'Plastic Goods',
                        ifelse(com_fam_ex$`Commodity Code` == 47, 'Recycled Wood Pulp & Paper',
                        ifelse(com_fam_ex$`Commodity Code` == 44, 'Wood & Charcoal',
                        ifelse(com_fam_ex$`Commodity Code` == 29, 'Organic Chemicals',
                        ifelse(com_fam_ex$`Commodity Code` == 30, 'Pharmaceuticals',
                        ifelse(com_fam_ex$`Commodity Code` == 38, 'Chemical Products',
                        ifelse(com_fam_ex$`Commodity Code` == 74, 'Copper',
                        ifelse(com_fam_ex$`Commodity Code` == 76, 'Aluminum',
                        ifelse(com_fam_ex$`Commodity Code` == 10, 'Grains & Cereals',
                        ifelse(com_fam_ex$`Commodity Code` == 3, 'Fish & Other Seafoods',
                        ifelse(com_fam_ex$`Commodity Code` == 71, 'Precious Stones & Metals',
                        ifelse(com_fam_ex$`Commodity Code` == 41, 'Hides & Leather',
                        ifelse(com_fam_ex$`Commodity Code` == 26, 'Ores & Ash',
                        ifelse(com_fam_ex$`Commodity Code` == 72, 'Iron & Steel, Raw',
                        ifelse(com_fam_ex$`Commodity Code` == 52, 'Cotton',
                        ifelse(com_fam_ex$`Commodity Code` == 28, 'Inorgnic Compounds & Rare Metals',
                        ifelse(com_fam_ex$`Commodity Code` == 48, 'Paper & Paperboard',
                        ifelse(com_fam_ex$`Commodity Code` == 73, 'Iron or Steel Goods',
                        NA)))))))))))))))))))))))))

com_fam_im$Commodity <- ifelse(com_fam_im$`Commodity Code` == 94, 'Furniture, Bedding, & Lights',
                        ifelse(com_fam_im$`Commodity Code` == 84, 'Mechanical Machinery',
                        ifelse(com_fam_im$`Commodity Code` == 95, 'Toys & Sports Equipment',
                        ifelse(com_fam_im$`Commodity Code` == 87, 'Cars, Vehicles, & Parts',
                        ifelse(com_fam_im$`Commodity Code` == 85, 'Electronics & Equipment',
                        ifelse(com_fam_im$`Commodity Code` == 90, 'Medical/Scientific Instruments',
                        ifelse(com_fam_im$`Commodity Code` == 61, 'Knitted Clothing/Apparel',
                        ifelse(com_fam_im$`Commodity Code` == 39, 'Plastic Goods',
                        ifelse(com_fam_im$`Commodity Code` == 64, 'Footwear',
                        ifelse(com_fam_im$`Commodity Code` == 44, 'Wood & Charcoal',
                        ifelse(com_fam_im$`Commodity Code` == 29, 'Organic Chemicals',
                        ifelse(com_fam_im$`Commodity Code` == 62, 'Non-Knitted Clothing/Apparel',
                        ifelse(com_fam_im$`Commodity Code` == 63, 'Textiles & Worn Clothing',
                        ifelse(com_fam_im$`Commodity Code` == 42, 'Leather Goods',
                        ifelse(com_fam_im$`Commodity Code` == 76, 'Aluminum',
                        ifelse(com_fam_im$`Commodity Code` == 83, 'Base Metal Products',
                        ifelse(com_fam_im$`Commodity Code` == 82, 'Tools & Cutlery',
                        ifelse(com_fam_im$`Commodity Code` == 71, 'Precious Stones & Metals',
                        ifelse(com_fam_im$`Commodity Code` == 40, 'Rubber Products',
                        ifelse(com_fam_im$`Commodity Code` == 96, 'Other Manufactured Goods',
                        ifelse(com_fam_im$`Commodity Code` == 70, 'Glass & Glassware',
                        ifelse(com_fam_im$`Commodity Code` == 69, 'Ceramic Products',
                        ifelse(com_fam_im$`Commodity Code` == 49, 'Printed Books & Other Print',
                        ifelse(com_fam_im$`Commodity Code` == 48, 'Paper & Paperboard',
                        ifelse(com_fam_im$`Commodity Code` == 73, 'Iron or Steel Goods',
                        NA)))))))))))))))))))))))))                                                                                                            

com_fam_ex$`Commodity` <- factor(com_fam_ex$`Commodity`,
                                 levels = com_fam_ex$`Commodity`[
                                   order(com_fam_ex$`Trade Value`)])
com_fam_ex <- com_fam_ex[10:25,]
com_fam_ex$tariff <- ifelse(com_fam_ex$`Commodity Code` %in% c(3, 10, 12, 52, 87, 28, 29, 30, 38), 'Tariff in Place',
                                   'No Tariff Set')

com_fam_im$`Commodity` <- factor(com_fam_im$`Commodity`,
                                 levels = com_fam_im$`Commodity`[
                                   order(com_fam_im$`Trade Value`)])
com_fam_im <- com_fam_im[10:25,]
com_fam_im$tariff <- ifelse(com_fam_im$`Commodity Code` %in% c(44, 83, 42, 29, 94), 'Round 3 (Sept 2018)',
                     ifelse(com_fam_im$`Commodity Code` %in% c(73, 39), 'Round 2 (Aug 2018)', 
                     ifelse(com_fam_im$`Commodity Code` %in% c(90, 87, 84, 85), 'Round 1 (Jul 2018)', 
                     'No Tariff Set')))

# Plot comparative trade in commodity groups nationally
library(scales)
ggplot(com_fam_ex, aes(`Commodity`, `Trade Value`)) +
  geom_col(aes(fill = tariff), color = 'black', width = 0.8) +
  coord_flip() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        legend.position = c(.7, .40), legend.background = element_rect(fill=NA)) +
  scale_y_continuous(labels = comma, breaks = seq(0, 15, 5), expand = c(0,0)) +
  scale_fill_manual("Tariffs Set by China", values = c('light grey', 'red')) +
  labs(x = '', y = 'Trade Exports (billions $USD)',
       title = 'US exports to China come from the rust belt and rural industries',
       subtitle = 'As of 2017, US exports to China mostly consist of vehicles and agricultural products - both primarily \nfrom rural and midwest industrial hubs. These regions have been targeted by Chinese tariffs.', 
       caption = 'Data Source: UN Comtrade International Trade Statistics Database')

# save plot
ggsave('nation_exports.pdf')

ggplot(com_fam_im, aes(`Commodity`, `Trade Value`)) +
  geom_col(aes(fill = tariff), color = 'black', width = 0.8) +
  coord_flip() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        legend.position = c(.7, .40), legend.background = element_rect(fill=NA)) +
  scale_y_continuous(labels = comma, breaks = seq(0, 150, 50), expand = c(0,0)) +
  scale_fill_manual("Tariffs Set by US", values = c('light grey', 'dark red', 'red', 'pink')) +
  labs(x = '', y = 'Trade Imports (billions $USD)',
       title = 'Most US imports from China are consumer electronics targeted by current tariffs',
       subtitle = 'As of 2017, Chinese imports were most modern consumer staples, including electronics and household \nappliances. The Trump administration has imposed 25 percent tariffs on most of these goods.', 
       caption = 'Data Source: UN Comtrade International Trade Statistics & Personally Compiled Tariffs Listings')

# save plot
ggsave('nation_imports.pdf')

# clean house
rm(com)
rm(com_fam)
rm(com_fam_ex)
rm(com_fam_im)

#########################################################
# Create simple supply-demand example of what tariffs do
#########################################################

# credit to Andrew Heiss for creating this package: https://github.com/andrewheiss/reconPlots
library(devtools)
library(reconPlots)

line1 <- data.frame(Hmisc::bezier(c(10, 80, 90), c(10, 50, 90)))
line2 <- data.frame(Hmisc::bezier(c(10, 30, 90), c(90, 30, 10)))
line3 <- data.frame(Hmisc::bezier(c(0, 100), c(15, 15)))
line4 <- data.frame(Hmisc::bezier(c(0, 100), c(25, 25)))

line_intersect <- as.data.frame(curve_intersect(line1, line2))
base_intersect1 <- as.data.frame(curve_intersect(line1, line3))
base_intersect2 <- as.data.frame(curve_intersect(line2, line3))
new_intersect1 <- as.data.frame(curve_intersect(line1, line4))
new_intersect2 <- as.data.frame(curve_intersect(line2, line4))

ggplot(mapping = aes(x = x, y = y)) +
  geom_path(data = line1, color = "red", size = 1) +
  geom_path(data = line2, color = "blue", size = 1) +
  geom_path(data = line3, color = "black", linetype = 'dashed', size = 1) +
  geom_path(data = line4, color = "black", size = 1) +
  geom_point(data = base_intersect1, size = 2) +
  geom_point(data = base_intersect2, size = 2) +
  geom_ribbon(data = subset(line2, line2$x <= 58.51075),
      aes(ymin = 25, ymax = y), fill = 'light blue', alpha = .2) +
  geom_ribbon(data = subset(line1, line1$x <= 34.13912),
      aes(ymin = y, ymax = 25), fill = 'red', alpha = .2) +
  geom_segment(data = new_intersect1,
      aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
  geom_segment(data = new_intersect2,
      aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +  coord_equal() +
  theme_classic() +
  annotate("text", x = 85, y = 27.5, label = 'World Price + Tariffs', color = 'black') +
  annotate("text", x = 75, y = 55, angle = 50, label = 'Domestic Supply', color = 'red') +
  annotate("text", x = 30, y = 60, angle = -60, label = 'Demand', color = 'blue') +
  annotate("text", x = 47, y = 20, label = 'Lost Consumer Surplus', color = 'black', size = 3) +
  annotate("text", x = 92, y = 17.5, label = 'World Price', color = 'black') +
  annotate("text", x = 25, y = 35, label = 'Consumer Surplus', color = 'blue', size = 3) +
  labs(x = 'Quantity', y = 'Price',
       title = 'Theory suggests tariffs hurt consumers but may help domestic suppliers',
       subtitle = 'With a given imported good, consumers are benefiting from low costs and high quantity. \nWhen tariffs push up the outside price, quantity goes down and prices go up, \nhurting consumers while providing relatively little benefit to domestic businesses.', 
       caption = 'Credit where credit is due: Andrew Heiss for his relevant methodology')
