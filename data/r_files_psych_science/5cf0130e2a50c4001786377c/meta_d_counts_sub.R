data = read.table('~/Dropbox/KMSD_AnalysisScripts/KMSD_All_Behave_Delay.txt',F)

names(data)=c('id','run','trial','phase','phase2','ons','rt','resp','onstime','jitter_time','fixation_time','pair_one_label','pair_two_label','pair1','pair2','feedcat','localizer_type', 'trial_type','loc_correct','cpos_list', 'rate_t_list', 'conf', 'old', 'ttype','delay_start','jol_correct')


submatrix=matrix(nrow=20,ncol=57)
for(i in c(1:20)){
	
	subdata=data[data$id==i&!is.na(data$ttype=='immediate'),]
  
	
	subdata$is_1=ifelse(subdata$old==1,1,2)
	subdata$resp_1=ifelse(subdata$resp==1,1,2)

s1_imm_vec=c(
sum(subdata$ttype=='immediate'&subdata$conf==7&subdata$is_1==1&subdata$resp_1==1),
sum(subdata$ttype=='immediate'&subdata$conf==6&subdata$is_1==1&subdata$resp_1==1),
sum(subdata$ttype=='immediate'&subdata$conf==5&subdata$is_1==1&subdata$resp_1==1),
sum(subdata$ttype=='immediate'&subdata$conf==4&subdata$is_1==1&subdata$resp_1==1),
sum(subdata$ttype=='immediate'&subdata$conf==3&subdata$is_1==1&subdata$resp_1==1),
sum(subdata$ttype=='immediate'&subdata$conf==2&subdata$is_1==1&subdata$resp_1==1),
sum(subdata$ttype=='immediate'&subdata$conf==1&subdata$is_1==1&subdata$resp_1==1),
sum(subdata$ttype=='immediate'&subdata$conf==1&subdata$is_1==1&subdata$resp_1==2),
sum(subdata$ttype=='immediate'&subdata$conf==2&subdata$is_1==1&subdata$resp_1==2),
sum(subdata$ttype=='immediate'&subdata$conf==3&subdata$is_1==1&subdata$resp_1==2),
sum(subdata$ttype=='immediate'&subdata$conf==4&subdata$is_1==1&subdata$resp_1==2),
sum(subdata$ttype=='immediate'&subdata$conf==5&subdata$is_1==1&subdata$resp_1==2),
sum(subdata$ttype=='immediate'&subdata$conf==6&subdata$is_1==1&subdata$resp_1==2),
sum(subdata$ttype=='immediate'&subdata$conf==7&subdata$is_1==1&subdata$resp_1==2))

s1_del_vec=c(
sum(subdata$ttype=='delayed'&subdata$conf==7&subdata$is_1==1&subdata$resp_1==1),
sum(subdata$ttype=='delayed'&subdata$conf==6&subdata$is_1==1&subdata$resp_1==1),
sum(subdata$ttype=='delayed'&subdata$conf==5&subdata$is_1==1&subdata$resp_1==1),
sum(subdata$ttype=='delayed'&subdata$conf==4&subdata$is_1==1&subdata$resp_1==1),
sum(subdata$ttype=='delayed'&subdata$conf==3&subdata$is_1==1&subdata$resp_1==1),
sum(subdata$ttype=='delayed'&subdata$conf==2&subdata$is_1==1&subdata$resp_1==1),
sum(subdata$ttype=='delayed'&subdata$conf==1&subdata$is_1==1&subdata$resp_1==1),
sum(subdata$ttype=='delayed'&subdata$conf==1&subdata$is_1==1&subdata$resp_1==2),
sum(subdata$ttype=='delayed'&subdata$conf==2&subdata$is_1==1&subdata$resp_1==2),
sum(subdata$ttype=='delayed'&subdata$conf==3&subdata$is_1==1&subdata$resp_1==2),
sum(subdata$ttype=='delayed'&subdata$conf==4&subdata$is_1==1&subdata$resp_1==2),
sum(subdata$ttype=='delayed'&subdata$conf==5&subdata$is_1==1&subdata$resp_1==2),
sum(subdata$ttype=='delayed'&subdata$conf==6&subdata$is_1==1&subdata$resp_1==2),
sum(subdata$ttype=='delayed'&subdata$conf==7&subdata$is_1==1&subdata$resp_1==2))

s2_imm_vec=c(
sum(subdata$ttype=='immediate'&subdata$conf==7&subdata$is_1==2&subdata$resp_1==1),
sum(subdata$ttype=='immediate'&subdata$conf==6&subdata$is_1==2&subdata$resp_1==1),
sum(subdata$ttype=='immediate'&subdata$conf==5&subdata$is_1==2&subdata$resp_1==1),
sum(subdata$ttype=='immediate'&subdata$conf==4&subdata$is_1==2&subdata$resp_1==1),
sum(subdata$ttype=='immediate'&subdata$conf==3&subdata$is_1==2&subdata$resp_1==1),
sum(subdata$ttype=='immediate'&subdata$conf==2&subdata$is_1==2&subdata$resp_1==1),
sum(subdata$ttype=='immediate'&subdata$conf==1&subdata$is_1==2&subdata$resp_1==1),
sum(subdata$ttype=='immediate'&subdata$conf==1&subdata$is_1==2&subdata$resp_1==2),
sum(subdata$ttype=='immediate'&subdata$conf==2&subdata$is_1==2&subdata$resp_1==2),
sum(subdata$ttype=='immediate'&subdata$conf==3&subdata$is_1==2&subdata$resp_1==2),
sum(subdata$ttype=='immediate'&subdata$conf==4&subdata$is_1==2&subdata$resp_1==2),
sum(subdata$ttype=='immediate'&subdata$conf==5&subdata$is_1==2&subdata$resp_1==2),
sum(subdata$ttype=='immediate'&subdata$conf==6&subdata$is_1==2&subdata$resp_1==2),
sum(subdata$ttype=='immediate'&subdata$conf==7&subdata$is_1==2&subdata$resp_1==2))

s2_del_vec=c(
sum(subdata$ttype=='delayed'&subdata$conf==7&subdata$is_1==2&subdata$resp_1==1),
sum(subdata$ttype=='delayed'&subdata$conf==6&subdata$is_1==2&subdata$resp_1==1),
sum(subdata$ttype=='delayed'&subdata$conf==5&subdata$is_1==2&subdata$resp_1==1),
sum(subdata$ttype=='delayed'&subdata$conf==4&subdata$is_1==2&subdata$resp_1==1),
sum(subdata$ttype=='delayed'&subdata$conf==3&subdata$is_1==2&subdata$resp_1==1),
sum(subdata$ttype=='delayed'&subdata$conf==2&subdata$is_1==2&subdata$resp_1==1),
sum(subdata$ttype=='delayed'&subdata$conf==1&subdata$is_1==2&subdata$resp_1==1),
sum(subdata$ttype=='delayed'&subdata$conf==1&subdata$is_1==2&subdata$resp_1==2),
sum(subdata$ttype=='delayed'&subdata$conf==2&subdata$is_1==2&subdata$resp_1==2),
sum(subdata$ttype=='delayed'&subdata$conf==3&subdata$is_1==2&subdata$resp_1==2),
sum(subdata$ttype=='delayed'&subdata$conf==4&subdata$is_1==2&subdata$resp_1==2),
sum(subdata$ttype=='delayed'&subdata$conf==5&subdata$is_1==2&subdata$resp_1==2),
sum(subdata$ttype=='delayed'&subdata$conf==6&subdata$is_1==2&subdata$resp_1==2),
sum(subdata$ttype=='delayed'&subdata$conf==7&subdata$is_1==2&subdata$resp_1==2))


submatrix[i,]=c(i,s1_imm_vec,s2_imm_vec,s1_del_vec,s2_del_vec)	
	
}

