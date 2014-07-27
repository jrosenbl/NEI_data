# plot 5
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City? 
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

PLOT_NUMBER = 5
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

# subset to Baltimore Maryland (fips == 24510) and years 1999 to 2008
baltimore_nei = subset(nei_df, fips == 24510 & (year > 1998 & year < 2009))
# subset to type == 'ON-ROAD' to get motor vehicle sources (cars, buses, trucks, etc)
baltimore_nei_mv = subset(baltimore_nei, type=='ON-ROAD')

sum_by_yr = ddply(baltimore_nei_mv, .(year), summarize, total_emissions=sum(emissions))
sum_by_yr$cum_pc_chg = (sum_by_yr$total_emissions / sum_by_yr[1,"total_emissions"] - 1) * 100

# plot cumulative % change in total annual emissions
ggplot(sum_by_yr, aes(x=year, y=cum_pc_chg)) + 
  geom_point() +
  geom_line(color='red',aes(x=year, y=cum_pc_chg)) +
  geom_text(aes(label=round(cum_pc_chg,2),vjust=-.75),show_guide=F) +
  ggtitle('Cumulative % Change Yr to Yr in Total Annual Emissions\n1999-2008 for Motor Vehicle Sources') +
  labs(x = 'Year', y = 'Percent Change')

ggsave(plot_file, width=8, height=8, units='in')
message(plot_name,' saved')