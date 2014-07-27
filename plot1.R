# plot 1
#   Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base 
#   plotting system, make a plot showing the total PM2.5 emission from all sources for each of the 
#   years 1999, 2002, 2005, and 2008.

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

PLOT_NUMBER = 1
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

total_emissions_by_yr = tapply(nei$Emissions,nei$year,sum)
png(plot_file,480,480)
barplot(total_emissions_by_yr, xlab='Year',ylab='Total Emissions (tons of PM2.5)', 
        main='Total Emissions from All Sources by Year')
dev.off()
message(plot_name,' saved')
