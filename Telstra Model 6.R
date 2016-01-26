
 mtest2  = as.data.frame(matrix(nrow = nrow(train), ncol = 390))
mtest2[,5:390] = 0
mtest2[1:4] = train[1:4]
mtest2 = rename(mtest2, c('V1' = 'id', 'V2' = 'log_feature', 'V3' = 'volume'))




for(i in 1:386)
{
colnames(mtest2)[i + 3] = 
as.character(log_feature[unique(log_feature$log_feature),2][i])
}
log_feature[unique(log_feature$log_feature),2]




