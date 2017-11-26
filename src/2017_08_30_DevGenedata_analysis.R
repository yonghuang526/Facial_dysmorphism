data = read.csv('/wdata/devGenes/Kevin - Gait Analyses/DevGenesDatabases_2017-08-29_1147.csv')
feature = read.csv('/wdata/devGenes/CURRENT DATA /kevin_facial_point_extraction/features.csv')
Gait_Model= read.csv('/wdata/devGenes/Kevin - Gait Analyses/9.19.16 Data to Kevin for Gait Model.csv')
matchdata = data[match(row.names(feature),row.names(data)),]

subdata = data[match(feature[,1],data[,1]),]
na_data = subdata[-NA,]

subdata_new = subdata[-match(na_data[,1],subdata[,1]),]

affected_pattern <- "_1$"
affected_Auti = data[grep(affected_pattern,data[,1]),]
unaffected_Auti = data[-match(affected_Auti[,1],data[,1]),]

affected_AUTI_LI = affected_Auti[which(affected_Auti$Language.Impairment..dep. == 'TRUE'|affected_Auti$Language.Impairment..dep. == '1'|affected_Auti$Language.Impairment..dep. == 'True'),]
affected_AUTI_None_LI = affected_Auti[which(affected_Auti$Language.Impairment..dep. == 'FALSE'|affected_Auti$Language.Impairment..dep. == 'False'|affected_Auti$Language.Impairment..dep. == 'n/a'|affected_Auti$Language.Impairment..dep.== ''|affected_Auti$Language.Impairment..dep.==0),]


save(affected_AUTI_LI,affected_AUTI_None_LI,file='/wdata/rotating_students/yonghuang/data/2017_08_30.Rdata')
load('/wdata/rotating_students/yonghuang/data/2017_08_30.Rdata')
