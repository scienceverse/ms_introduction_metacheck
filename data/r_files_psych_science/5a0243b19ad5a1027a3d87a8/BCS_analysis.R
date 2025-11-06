#=====================================================================#
# Education-IQ meta-analysis: new analysis of the British Cohort Study#
#=====================================================================#

#By Stuart Ritchie, October 2017

#==========================#
#Data and packages required#
#==========================#

library(foreign); library(psych); library(Hmisc); library(tableone); library(plyr)

#Age 10 data
bcs10 <- read.spss("~/BCS1970/UKDA-3723-spss/spss/spss12/3723.sav", use.value.labels=T, to.data.frame=T, max.value.labels=Inf, trim.factor.names=F,add.undeclared.levels="no")

#Age 34 data
bcs34 <- read.spss("~/BCS1970/UKDA-5585-spss/spss/spss12/bcs_2004_adult_assessment_basic_skills.sav", use.value.labels=T, to.data.frame=T, max.value.labels=Inf, trim.factor.names=F, add.undeclared.levels="no")

bcs34a <- read.spss("~/BCS1970/UKDA-5585-spss/spss/spss12/bcs_2004_followup.sav", use.value.labels=T, to.data.frame=T, max.value.labels=Inf, trim.factor.names=F,add.undeclared.levels="no")

#=======================#
#Prepare the age 10 data#
#=======================#

#Cut down to only the variables we're interested in
vars10 <- c("bcsid", "i3504", "i3505", "i3506", "i3507", "i3508", "i3509", "i3510", "i3511", "i3512", "i3513", "i3514", "i3515", "i3516", "i3517", "i3518", "i3519", "i3520", "i3521", "i3522", "i3523", "i3524", "i3525", "i3526", "i3527", "i3528", "i3529", "i3530", "i3531", "i3532", "i3533", "i3534", "i3535", "i3536", "i3537", "i3538", "i3539", "i3540", "i3541", "i3542", "i3543", "i3544", "i3545", "i3546", "i3547", "i3548", "i3549", "i3550", "i3551", "i3552", "i3553", "i3554", "i3555", "i3556", "i3557", "i3558", "i3559", "i3560", "i3561", "i3562", "i3563", "i3564", "i3565", "i3566", "i3567", "i3568", "i3569", "i3570", "i3571", "i3572", "i3573", "i3574", "i4201", "i4202", "i4203", "i4204", "i4205", "i4206", "i4207", "i4208", "i4209", "i4210", "i4211", "i4212", "i4213", "i4214", "i4215", "i4216", "i4217", "i4218", "i4219", "i4220", "i4221", "i3617", "i3618", "i3619", "i3620", "i3621", "i3622", "i3623", "i3624", "i3625", "i3626", "i3627", "i3628", "i3629", "i3630", "i3631", "i3632", "i3633", "i3634", "i3635", "i3636", "i3637", "i3638", "i3639", "i3640", "i3641", "i3642", "i3643", "i3644", "i4101", "i4102", "i4103", "i4104", "i4105", "i4106", "i4107", "i4108", "i4109", "i4110", "i4111", "i4112", "i4113", "i4114", "i4115", "i4116", "i4117", "i4118", "i4119", "i4120", "i4121", "i4122", "i4123", "i4124", "i4125", "i4126", "i4127", "i4128", "i4129", "i4130", "i4131", "i4132", "i4133", "i4134", "i4135", "i4136", "i4137", "i4138", "i4139", "i4140", "i4141", "i4142", "i4143", "i4144", "i4145", "i4146", "i4147", "i4148", "i4149", "i4150", "i4151", "i4152", "i4153", "i4154", "i4001", "i4002", "i4003", "i4004", "i4005", "i4006", "i4007", "i4008", "i4009", "i4010", "i4011", "i4012", "i4013", "i4014", "i4015", "i4016", "i4017", "i4018", "i4019", "i4020", "i4021", "i4022", "i4023", "i4024", "i4025", "i4026", "i4027", "i4028", "i4029", "i4030", "i4031", "i4032", "i4033", "i4034", "i4035", "i4036", "i4037", "i4038", "i4039", "i4040", "i4041", "i4042", "i4043", "i4044", "i4045", "i4046", "i4047", "i4048", "i4049", "i4050", "i4051", "i4052", "i4053", "i4054", "i4055", "i4056", "i4057", "i4058", "i4059", "i4060", "i4061", "i4062", "i4063", "i4064", "i4065", "i4066", "i4067", "i4068", "i4069", "i4070", "i4071", "i4072", "i3815", "i3816", "i3817", "i3818", "i3819", "i3820", "i3821", "i3822", "i3823", "i3824", "i3825", "i3826", "i3827", "i3828", "i3829", "i3830", "i3831", "i3832", "i3833", "i3834", "i3835", "i3836", "i3837", "i3838", "i3839", "i3840", "i3841", "i3842", "i3843", "i3844", "i3845", "i3846", "i3847", "i3848", "i3849", "i3850", "i3851", "i3852", "i3853", "i3854", "i3855", "i3856", "i3857", "i3858", "i3859", "i3860", "i3861", "i3862", "i3863", "i3864", "c3.4", "c3.11", "d2", "d5.2", "sex10", "tc10", "j127", "j128", "j129", "j130", "j131", "j132", "j133", "j134", "j135", "j136", "j137", "j138", "j139", "j140", "j141", "j142", "j143", "j144", "j145", "j146", "j147", "j148", "j149", "j150", "j151", "j152", "j153", "j154", "j155", "j156", "j157", "j158", "j159", "j160", "j161", "j162", "j163", "j164", "j165", "j166", "j167", "j168", "j169", "j170", "j171", "j172", "j173", "j174", "j175", "j176", "j177", "j178a","j178b", "j058", "j059")

bcs10 <- bcs10[,vars10]

head(bcs10)

#IQ age 10 BAS IQ subtests (British Ability Scales)
#Def = Definitions; Dig = Recall of Digits; Sim = Similarities; Mat = Matrices

DefItems = c("i3504", "i3505", "i3506", "i3507", "i3508", "i3509", "i3510", "i3511", "i3512", "i3513", "i3514", "i3515", "i3516", "i3517", "i3518", "i3519", "i3520", "i3521", "i3522", "i3523", "i3524", "i3525", "i3526", "i3527", "i3528", "i3529", "i3530", "i3531", "i3532", "i3533", "i3534", "i3535", "i3536", "i3537", "i3538", "i3539", "i3540")
DigItems = c("i3541", "i3542", "i3543", "i3544", "i3545", "i3546", "i3547", "i3548", "i3549", "i3550", "i3551", "i3552", "i3553", "i3554", "i3555", "i3556", "i3557", "i3558", "i3559", "i3560", "i3561", "i3562", "i3563", "i3564", "i3565", "i3566", "i3567", "i3568", "i3569", "i3570", "i3571", "i3572", "i3573", "i3574")
SimItems = c("i4201", "i4202", "i4203", "i4204", "i4205", "i4206", "i4207", "i4208", "i4209", "i4210", "i4211", "i4212", "i4213", "i4214", "i4215", "i4216", "i4217", "i4218", "i4219", "i4220", "i4221")
MatItems = c("i3617", "i3618", "i3619", "i3620", "i3621", "i3622", "i3623", "i3624", "i3625", "i3626", "i3627", "i3628", "i3629", "i3630", "i3631", "i3632", "i3633", "i3634", "i3635", "i3636", "i3637", "i3638", "i3639", "i3640", "i3641", "i3642", "i3643", "i3644")

#Change everything to numeric

for(i in DefItems) {
	bcs10[,i] <- as.numeric(bcs10[,i])
}

for(i in DigItems) {
	bcs10[,i] <- as.numeric(bcs10[,i])
}

for(i in SimItems) {
	bcs10[,i] <- as.numeric(bcs10[,i])
}

for(i in MatItems) {
	bcs10[,i] <- as.numeric(bcs10[,i])
}

#Change so that correct answers = 1 and incorrect answers = 0 (for ease of later addition)

for(i in DefItems){
	bcs10[bcs10[,i] %in% c(-6, -3),i] <- NA
	bcs10[bcs10[,i] %in% c(2 , 3, 9),i] <- 0
}

for(i in DigItems){
	bcs10[bcs10[,i] %in% c(-6, -3),i] <- NA
	bcs10[bcs10[,i] %in% c(2 , 3, 9),i] <- 0
}

for(i in SimItems){
	bcs10[bcs10[,i] %in% c(1,3), i] <- 0
	bcs10[bcs10[,i] %in% c(2), i] <- 1
}

for(i in MatItems){
	bcs10[bcs10[,i] %in% c(-6, -3),i] <- NA
	bcs10[bcs10[,i] %in% c(2, 3, 9),i] <- 0
}

#Sum to get the total scores for each subtest

bcs10$BASdef <- 
((bcs10$i3504) + (bcs10$i3505) + (bcs10$i3506) + (bcs10$i3507) + (bcs10$i3508) +  (bcs10$i3509) + (bcs10$i3510) + (bcs10$i3511) + (bcs10$i3512) + (bcs10$i3513) + (bcs10$i3514) + (bcs10$i3515) + (bcs10$i3516) + (bcs10$i3517) + (bcs10$i3518) + (bcs10$i3519) + (bcs10$i3520) + (bcs10$i3521) + (bcs10$i3522) + (bcs10$i3523) + (bcs10$i3524) + (bcs10$i3525) + (bcs10$i3526) + (bcs10$i3527) + (bcs10$i3528) + (bcs10$i3529) + (bcs10$i3530) + (bcs10$i3531) + (bcs10$i3532) + (bcs10$i3533) + (bcs10$i3534) + (bcs10$i3535) + (bcs10$i3536) + (bcs10$i3537) + (bcs10$i3538) + (bcs10$i3539) + (bcs10$i3540))

bcs10$BASdig <- ((bcs10$i3541) + (bcs10$i3542) + (bcs10$i3543) + (bcs10$i3544) + (bcs10$i3545) + (bcs10$i3546) + (bcs10$i3547) + (bcs10$i3548) + (bcs10$i3549) + (bcs10$i3550) + (bcs10$i3551) + (bcs10$i3552) + (bcs10$i3553) + (bcs10$i3554) +(bcs10$i3555) + (bcs10$i3556) + (bcs10$i3557) + (bcs10$i3558) + (bcs10$i3559) + (bcs10$i3560) + (bcs10$i3561) + (bcs10$i3562) + (bcs10$i3563) + (bcs10$i3564) + (bcs10$i3565) + (bcs10$i3566) + (bcs10$i3567) + (bcs10$i3568) + (bcs10$i3569) + (bcs10$i3570) + (bcs10$i3571) + (bcs10$i3572) + (bcs10$i3573) + (bcs10$i3574))

bcs10$BASsim <- ((bcs10$i4201) + (bcs10$i4202) + (bcs10$i4203) + (bcs10$i4204) + (bcs10$i4205) + (bcs10$i4206) + (bcs10$i4207) + (bcs10$i4208) + (bcs10$i4209) + (bcs10$i4210) + (bcs10$i4211) + (bcs10$i4212) + (bcs10$i4213) + (bcs10$i4214) + (bcs10$i4215) + (bcs10$i4216) + (bcs10$i4217) + (bcs10$i4218) + (bcs10$i4219) + (bcs10$i4220) + (bcs10$i4221))

bcs10$BASmat <- ((bcs10$i3617) + (bcs10 $i3618) + (bcs10 $i3619) + (bcs10 $i3620) + (bcs10 $i3621) + (bcs10 $i3622) + (bcs10 $i3623) + (bcs10$i3624) + (bcs10$i3625) + (bcs10$i3626) + (bcs10$i3627) + (bcs10$i3628) + (bcs10$i3629) + (bcs10$i3630) +(bcs10$i3631) + (bcs10$i3632) + (bcs10$i3633) + (bcs10$i3634) + (bcs10$i3635) + (bcs10$i3636) + (bcs10$i3637) + (bcs10$i3638) + (bcs10$i3639) +(bcs10$i3640) + (bcs10$i3641) + (bcs10$i3642) + (bcs10$i3643) + (bcs10$i3644))

#Reduce everything down again to a smaller dataset with just these sum variables and other relevant covariates ("tc10" is the singleton variable, used below)

names(bcs10)

bcs10 <- bcs10[,c("bcsid", "sex10", "BASdef", "BASdig", "BASsim", "BASmat", "tc10")]

#=======================#
#Prepare the age 34 data#
#=======================#

names(bcs34); names(bcs34a)

#Numeracy test score

describe(bcs34$numall)

bcs34 <- bcs34[,c("bcsid", "numall")]

#Education

levels(as.factor(bcs34a$bd7hq5)); table(bcs34a$bd7hq5)
bcs34a$bd7hq5[bcs34a$bd7hq5 %in% c(-7, -8, -9)] <- NA

bcs34a$qual34 <- bcs34a$bd7hq5

bcs34a <- bcs34a[,c("bcsid", "qual34")]

#Merge the two age-34 datasets together with the age-10 dataset

bcs <- merge(bcs10, bcs34, by.x = c("bcsid"), by.y = c("bcsid"), all.x=T, all.y=T)
bcs <- merge(bcs, bcs34a, by.x = c("bcsid"), by.y = c("bcsid"), all.x=T, all.y=T)

names(bcs)

#Select only the singletons (exclude twins, triplets, etc.)

bcs <- subset(bcs, subset=(tc10 =="Singleton"))

#=====================================================#
#Descriptive statistics, correlations, and conversions#
#=====================================================#

#Descriptive stats

CreateTableOne(data=bcs[,-1])

#Make a correlation matrix on the non-converted data

with(bcs,{rcorr(
	cbind(
	BASdef,
	BASdig,
	BASsim,
	BASmat,
	numall	
	)
	)
})

#Convert the numerical variable onto IQ-type scale, after square-transforming it
	
bcs$num34s <- bcs$numall^2	
bcs$num34s <- ((scale(bcs$num34s)*15) + 100)
bcs$num34 <- ((scale(bcs$numall)*15) + 100)
hist(bcs$num34s)

#Change the educational qualification variable so that it reflects years of education

bcs$eduYears <- bcs$qual34

bcs$eduYears <- revalue(bcs$eduYears, c("None"=11, "CSE"=11, "GCE O Level/ GCSE"=11, "A Level/ SSCE/ A-S level"=13, "Degree/ Dip. H.Ed/ Oth teaching qual./ Nursing qual"=17, "Higher degree / PGCE"=21))

bcs$eduYears <- as.numeric(paste(bcs$eduYears))

#Make a compositive g-factor score from the four IQ subtest variables, and convert it to standardized IQ units

bcs$BASmat_s <- scale(bcs$BASmat)
bcs$BASdef_s <- scale(bcs$BASdef)
bcs$BASsim_s <- scale(bcs$BASsim)
bcs$BASdig_s <- scale(bcs$BASdig)

bcs$g10 <- (bcs$BASmat_s + bcs$BASdef_s + bcs$BASsim_s + bcs$BASdig_s)
bcs$g10 <- ((scale(bcs$g10)*15) + 100)

#Make a correlation matrix on the converted data (this is the one that's shown in the Supplementary Materials)

with(bcs,{rcorr(
	cbind(
	BASdef,
	BASsim,
	BASdig,
	BASmat,
	g10,
	eduYears,
	num34s,
	sex10
	)
	)
})

#============#
#Final models#
#============#
#Predict numerical ability from education and age-10 general intelligence, also correcting for sex

m1 <- lm(num34s ~ eduYears + g10 + sex10, data=bcs); summary(m1)

#Comparison model that does not control for age-10 general intelligence

m2 <- lm(num34s ~ eduYears + sex10, data=bcs); summary(m2)

