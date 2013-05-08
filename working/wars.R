data(wars) 

wars.us <- wars[which(wars$SideA %in% c('United States','United States of America')),]
timeline(wars.us[,c('WarName')])
