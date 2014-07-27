# plot 4
#   Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
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

PLOT_NUMBER = 4
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

# search the combination of 5 source description fields below for words repesenting coal AND
#  words representing combustion
scc_description = paste(scc_df$short.name,
                    scc_df$scc.level.one,
                    scc_df$scc.level.two,
                    scc_df$scc.level.three,
                    scc_df$scc.level.four)
coal_sources = grep('[Cc]oal|[Bb]itum|[Ll]ignite|[Aa]nthrac',scc_description)
comb_sources = grep('[Cc]omb|-fired',scc_description)
coal_comb_sources = intersect(coal_sources,comb_sources)

# subset to coal combustion sources
nei_coal_comb = subset(nei_df, scc %in% scc_df[coal_comb_sources,'scc'])

sum_by_yr = ddply(nei_coal_comb, .(year), summarize, total_emissions=sum(emissions))
sum_by_yr$cum_pc_chg = (sum_by_yr$total_emissions / sum_by_yr[1,"total_emissions"] - 1) * 100

# plot cumulative % change in total annual emissions
ggplot(sum_by_yr, aes(x=year, y=cum_pc_chg)) + 
  geom_point() +
  geom_line(color='blue',alpha=.5, show_guide=FALSE) +
  geom_text(aes(label=round(cum_pc_chg,2),vjust=-.75)) +
  ggtitle('Cumulative % Change Yr to Yr in Total Annual Emissions\n1999-2008 for Coal Combustion Sources')
  labs(x = 'Year', y = 'Percent Change')

ggsave(plot_file, width=8, height=8, units='in')
message(plot_name,' saved')