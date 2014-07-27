# plot 2
#  Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
#  from 1999 to 2008? Use the base plotting system to make a plot answering this question.


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

PLOT_NUMBER = 2
NEI_DATA_FILE_NAME = 'summarySCC_PM25.rds'
IN_DIR = './'
plot_dir = './'

PLOT_NUMBER = 2
nei_data_file = paste0(IN_DIR,NEI_DATA_FILE_NAME)
plot_name = sprintf('plot%s.png',PLOT_NUMBER)
plot_file = paste0(plot_dir,plot_name)

if (!use_existing_dataframe('nei')) {
  message('reading nei data')
  nei = readRDS(nei_data_file)
}

# subset to Baltimore Maryland (fips == 24510) and years 1999 to 2008
bm_nei = subset(nei, fips == 24510 & (year > 1998 & year < 2009))

total_emissions_by_yr = tapply(bm_nei$Emissions,bm_nei$year,sum)
png(plot_file,480,480)
barplot(total_emissions_by_yr, xlab='Year',ylab='Total Emissions (tons of PM2.5)', 
        main='Total Emissions from All Sources by Year for Baltimore, MD')
dev.off()
message(plot_name,' saved')
