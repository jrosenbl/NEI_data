# plot 3
#   Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#   which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
#   Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make 
#   a plot answer this question.
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

PLOT_NUMBER = 3
NEI_DATA_FILE_NAME = 'summarySCC_PM25.rds'
IN_DIR = './'
PLOT_DIR = './'

nei_data_file = paste0(IN_DIR,NEI_DATA_FILE_NAME)
plot_name = sprintf('plot%s.png',PLOT_NUMBER)
plot_file = paste0(PLOT_DIR,plot_name)

if (!use_existing_dataframe('nei')) {
   message('reading nei data')
   nei = readRDS(nei_data_file)
}

# subset to Baltimore Maryland (fips == 24510) and years 1999 to 2008
baltimore_nei = subset(nei, fips == 24510 & (year > 1998 & year < 2009))

# compute table of total Emissions by type and year
sum_by_type_yr = ddply(baltimore_nei, type ~ year, summarize, sum=sum(Emissions))

# plot a facet of annual totals for each type
ggplot(sum_by_type_yr, aes(x=factor(year), y=sum)) + 
  facet_grid(.~type) + 
  geom_bar(stat='identity') + 
  geom_text(aes(label=round(sum,4)), size=2, vjust=-1) + 
  theme(axis.text.x = element_text(size=8)) + 
  labs(title='Total Emissions by Source and Year', x = 'Year', y = 'Total Emissions')

ggsave(plot_file, width=8, height=8, units='in')
message(plot_name,' saved')

