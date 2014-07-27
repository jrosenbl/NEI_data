# plot 6
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle 
# sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes 
# over time in motor vehicle emissions?
#
library(ggplot2)
library(plyr)

use_existing_dataframe = function(df_name) {
  if (exists(df_name)) {
    message('data frame ',df_name,' with ',nrow(eval(as.symbol(df_name))),' rows exists')
    yn = readline('use it? ')
    if (yn %in% c('Y','y')) {
      return(TRUE)
    }
  }
  return(FALSE) 
}

PLOT_NUMBER = 6
NEI_DATA_FILE_NAME = 'summarySCC_PM25.rds'
SCC_DATA_FILE_NAME = 'Source_Classification_Code.rds'
IN_DIR = './'
PLOT_DIR = './'

nei_data_file = paste0(IN_DIR,NEI_DATA_FILE_NAME)
scc_data_file = paste0(IN_DIR,SCC_DATA_FILE_NAME)
plot_name = sprintf('plot%s.png',PLOT_NUMBER)
plot_file = paste0(PLOT_DIR,plot_name)

if (!use_existing_dataframe('nei_df')) {
  message('reading nei data')
  nei_df = readRDS(nei_data_file)
  colnames(nei_df) = tolower(colnames(nei_df))
}

if (!use_existing_dataframe('scc_df')) {
  message('reading scc data')
  scc_df = readRDS(scc_data_file)
  colnames(scc_df) = tolower(colnames(scc_df))
}

# subset to Baltimore  (fips == 24510) and LA County (fips == '06037') and
#   type == ON-ROAD to get motor vehicle sources (cars, buses, trucks, etc)
baltimore_mv = nei_df[nei_df$fips == '24510' & nei_df$type == 'ON-ROAD',]
la_cty_mv = nei_df[nei_df$fips == '06037' & nei_df$type == 'ON-ROAD',]

# generate annual total and cumulative percent change tables for each palce
balt_sum_by_yr = ddply(baltimore_mv, .(year), summarize, total_emissions=sum(emissions))
lacy_sum_by_yr = ddply(la_cty_mv, .(year), summarize, total_emissions=sum(emissions))

balt_sum_by_yr$cum_pc_chg = (balt_sum_by_yr$total_emissions / balt_sum_by_yr[1,"total_emissions"] - 1) * 100
balt_sum_by_yr$place='baltimore'
lacy_sum_by_yr$cum_pc_chg = (lacy_sum_by_yr$total_emissions / lacy_sum_by_yr[1,"total_emissions"] - 1) * 100
lacy_sum_by_yr$place='la_cty'

# combine place tables into into one data frame
sum_by_yr = rbind(balt_sum_by_yr, lacy_sum_by_yr)

# plot cumulative % change in total annual emissions for both places
ggplot(sum_by_yr, aes(x=year, y=cum_pc_chg, color=place, shape=place)) + 
  geom_point(size=3) +
  geom_line() +
  geom_text(aes(label=round(cum_pc_chg,2),vjust=-.75),show_guide=F) +
  ylim(-80,20) +
  # geom_text(aes(label=round(percent_chg,2),vjust=-.75)) +
  ggtitle('Cumulative % Change Yr to Yr in Total Annual Emissions\nfor Motor Vehicle Sources')
  labs(x = 'Year', y = 'Percent Change') 

ggsave(plot_file, width=8, height=8, units='in')
message(plot_name,' saved')