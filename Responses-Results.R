###Q1

######A.
library(statsr)
library(dplyr)
library(ggplot2)
z <- 1.960
N<-500
A = UniversityAdmissions %>% select(6, 10)
research_prop = sum((A$Research==1)/500)
prop_a = sum((A$`University Rating`=='a')/500)
prop_b = sum((A$`University Rating`=='b')/500)
prop_c = sum((A$`University Rating`=='c')/500)
prop_d = sum((A$`University Rating`=='d')/500)
prop_e = sum((A$`University Rating`=='e')/500)
ar_SE<- sqrt( ((prop_a * (1 - prop_a))/N) + ((research_prop * (1 - research_prop))/N) )
br_SE<- sqrt( ((prop_b * (1 - prop_b))/N) + ((research_prop * (1 - research_prop))/N)  )
cr_SE<- sqrt( ((prop_c * (1 - prop_c))/N) + ((research_prop * (1 - research_prop))/N)  )
dr_SE<- sqrt( ((prop_d * (1 - prop_d))/N) + ((research_prop * (1 - research_prop))/N)  )
er_SE<- sqrt( ((prop_e * (1 - prop_e))/N) + ((research_prop * (1 - research_prop))/N)  )
ar_ME <- ar_SE * z
br_ME <- br_SE * z
cr_ME <- cr_SE * z
dr_ME <- dr_SE * z
er_ME <- er_SE * z
ar_prop <- research_prop - prop_a
br_prop <- research_prop - prop_b
cr_prop <- research_prop - prop_c
dr_prop <- research_prop - prop_d
er_prop <- research_prop - prop_e
CI_ar_low <- ar_prop - ar_ME
CI_ar_high <- ar_prop + ar_ME
CI_br_low <- br_prop - br_ME
CI_br_high <- br_prop + br_ME
CI_cr_low <- cr_prop - cr_ME
CI_cr_high <- cr_prop + cr_ME
CI_dr_low <- dr_prop - dr_ME
CI_dr_high <- dr_prop + dr_ME
CI_er_low <- er_prop - er_ME
CI_er_high <- er_prop + er_ME

#####B.

ar_pool <- (prop_a + research_prop)/2
br_pool <- (prop_b + research_prop)/2
cr_pool <- (prop_c + research_prop)/2
dr_pool <- (prop_d + research_prop)/2
er_pool <- (prop_e + research_prop)/2

ar_pool_SE <- sqrt( ( 2 * (ar_pool * (1 - ar_pool))/N) )
br_pool_SE <- sqrt( ( 2 * (br_pool * (1 - br_pool))/N) )
cr_pool_SE <- sqrt( ( 2 * (cr_pool * (1 - cr_pool))/N) )
dr_pool_SE <- sqrt( ( 2 * (dr_pool * (1 - dr_pool))/N) )
er_pool_SE <- sqrt( ( 2 * (er_pool * (1 - er_pool))/N) )

ar_z <- ( ar_prop - 0 )/ar_pool_SE
br_z <- ( br_prop - 0 )/br_pool_SE
cr_z <- ( cr_prop - 0 )/cr_pool_SE
dr_z <- ( dr_prop - 0 )/dr_pool_SE
er_z <- ( er_prop - 0 )/er_pool_SE


2*pnorm(q=ar_z, lower.tail=FALSE)
2*pnorm(q=br_z, lower.tail=FALSE)
2*pnorm(q=cr_z, lower.tail=FALSE)
2*pnorm(q=dr_z, lower.tail=FALSE)
2*pnorm(q=er_z, lower.tail=FALSE)



###Q2
#####A

Q2_sample <- sample_n(UniversityAdmissions, 10)
Q2_df = Q2_sample %>% select(10)

s1 <- sample(0:1, 10, replace=TRUE)
s2 <- sample(0:1, 10, replace=TRUE)
s3 <- sample(0:1, 10, replace=TRUE)
s4 <- sample(0:1, 10, replace=TRUE)
s5 <- sample(0:1, 10, replace=TRUE)
s6 <- sample(0:1, 10, replace=TRUE)
s7 <- sample(0:1, 10, replace=TRUE)
s8 <- sample(0:1, 10, replace=TRUE)
s9 <- sample(0:1, 10, replace=TRUE)
s10 <- sample(0:1, 10, replace=TRUE)

s1_p <- sum(s1)/10
s2_p <- sum(s2)/10
s3_p <- sum(s3)/10
s4_p <- sum(s4)/10
s5_p <- sum(s5)/10
s6_p <- sum(s6)/10
s7_p <- sum(s7)/10
s8_p <- sum(s8)/10
s9_p <- sum(s9)/10
s10_p <- sum(s10)/10
sample_mean <- sum(s1_p,s2_p,s3_p,s4_p,s5_p,s6_p,s7_p,s8_p,s9_p,s10_p)/10
p_mean= sum(Q2_df)/10

SE_sim <- sqrt( ((p_mean * (1 - p_mean))/10) )
Z_sim <- ( p_mean - sample_mean)/SE_sim



####Q3

#####A

degf <- 5-1
prop_a = sum((A$`University Rating`=='a')/500)
prop_b = sum((A$`University Rating`=='b')/500)
prop_c = sum((A$`University Rating`=='c')/500)
prop_d = sum((A$`University Rating`=='d')/500)
prop_e = sum((A$`University Rating`=='e')/500)


size <-100

first_sample <- sample_n(UniversityAdmissions, 100)
newUA <- UniversityAdmissions[order(-UniversityAdmissions$`Chance of Admit`),]
second_sample <- newUA[1:100, ]

obs_a_1 <- sum(first_sample$`University Rating`=='a')
obs_a_2 <- sum(second_sample$`University Rating`=='a')

obs_b_1 <- sum(first_sample$`University Rating`=='b')
obs_b_2 <- sum(second_sample$`University Rating`=='b')

obs_c_1 <- sum(first_sample$`University Rating`=='c')
obs_c_2 <- sum(second_sample$`University Rating`=='c')

obs_d_1 <- sum(first_sample$`University Rating`=='d')
obs_d_2 <- sum(second_sample$`University Rating`=='d')

obs_e_1 <- sum(first_sample$`University Rating`=='e')
obs_e_2 <- sum(second_sample$`University Rating`=='e')

exp_a <- 15
exp_b <- 21
exp_c <- 32
exp_d <- 25
exp_e <- 7

x2_first_sample <- sum(((obs_a_1 - exp_a) **2 )/exp_a , ((obs_b_1 - exp_b) **2 )/exp_b , ((obs_c_1 - exp_c) **2 )/exp_c , ((obs_d_1 - exp_d) **2 )/exp_d , ((obs_e_1 - exp_e) **2 )/exp_e )
pchisq(x2_first_sample , degf , lower.tail = FALSE)

x2_second_sample <- sum(((obs_a_2 - exp_a) **2 )/exp_a , ((obs_b_2 - exp_b) **2 )/prop_b , ((obs_c_2 - exp_c) **2 )/exp_c , ((obs_d_2 - exp_d) **2 )/exp_d , ((obs_e_2 - exp_e) **2 )/exp_e )
pchisq(x2_second_sample , degf , lower.tail = FALSE)


#####B

a_sum <- sum(UniversityAdmissions$`University Rating`=='a')
b_sum <- sum(UniversityAdmissions$`University Rating`=='b')
c_sum <- sum(UniversityAdmissions$`University Rating`=='c')
d_sum <- sum(UniversityAdmissions$`University Rating`=='d')
e_sum <- sum(UniversityAdmissions$`University Rating`=='e')

no_intern <-sum(UniversityAdmissions$internship_abroad==0)
yes_intern <-sum(UniversityAdmissions$internship_abroad==1)

a_no <- sum(UniversityAdmissions$`University Rating`=='a' & UniversityAdmissions$internship_abroad==0)
b_no <- sum(UniversityAdmissions$`University Rating`=='b' & UniversityAdmissions$internship_abroad==0)
c_no <- sum(UniversityAdmissions$`University Rating`=='c' & UniversityAdmissions$internship_abroad==0)
d_no <- sum(UniversityAdmissions$`University Rating`=='d' & UniversityAdmissions$internship_abroad==0)
e_no <- sum(UniversityAdmissions$`University Rating`=='e' & UniversityAdmissions$internship_abroad==0)

a_yes <- sum(UniversityAdmissions$`University Rating`=='a' & UniversityAdmissions$internship_abroad==1)
b_yes <- sum(UniversityAdmissions$`University Rating`=='b' & UniversityAdmissions$internship_abroad==1)
c_yes <- sum(UniversityAdmissions$`University Rating`=='c' & UniversityAdmissions$internship_abroad==1)
d_yes <- sum(UniversityAdmissions$`University Rating`=='d' & UniversityAdmissions$internship_abroad==1)
e_yes <- sum(UniversityAdmissions$`University Rating`=='e' & UniversityAdmissions$internship_abroad==1)

a_no_exp <- ( a_sum * no_intern )/N
b_no_exp <- ( b_sum * no_intern )/N
c_no_exp <- ( c_sum * no_intern )/N
d_no_exp <- ( d_sum * no_intern )/N
e_no_exp <- ( e_sum * no_intern )/N


a_yes_exp <- ( a_sum * no_intern )/N
b_yes_exp <- ( b_sum * no_intern )/N
c_yes_exp <- ( c_sum * no_intern )/N
d_yes_exp <- ( d_sum * no_intern )/N
e_yes_exp <- ( e_sum * no_intern )/N

degf_b <- (2-1) * (5-1)
x2_intern_rank <- sum( ((a_no - a_no_exp)**2)/a_no_exp , ((b_no - b_no_exp)**2)/b_no_exp ,((c_no - c_no_exp)**2)/c_no_exp , ((d_no - d_no_exp)**2)/d_no_exp , ((e_no - e_no_exp)**2)/e_no_exp ,
                       ((a_yes - a_yes_exp)**2)/a_yes_exp , ((b_yes - b_yes_exp)**2)/b_yes_exp , ((c_yes - c_yes_exp)**2)/c_yes_exp ,((d_yes - d_yes_exp)**2)/d_yes_exp ,((e_yes - e_yes_exp)**2)/e_yes_exp )

pchisq(x2_intern_rank , degf_b , lower.tail = FALSE)



#######Q4
#######B
###A
LM_Toefl <- lm(UniversityAdmissions$`Chance of Admit` ~ UniversityAdmissions$`TOEFL Score`)
LM_GRE <- lm(UniversityAdmissions$`Chance of Admit` ~ UniversityAdmissions$`GRE Score`)
###C

ggplot(UniversityAdmissions, aes(x = UniversityAdmissions$`GRE Score`, y = UniversityAdmissions$`Chance of Admit`)) + 
geom_point(color = "#03e3fc", alpha = 0.2) +  
geom_smooth(formula = y ~ x, method = "lm", linetype = "dashed")

ggplot(UniversityAdmissions, aes(x = UniversityAdmissions$`TOEFL Score`, y = UniversityAdmissions$`Chance of Admit`)) + 
geom_point(color = "#fc7b03", alpha = 0.2) + 
geom_smooth(formula = y ~ x, method = "lm", linetype = "dashed")

#######D

summary(LM_Toefl)$adj.r.squared
summary(LM_GRE)$adj.r.squared

anova(LM_Toefl)
anova(LM_GRE)

#######F
###A


Q4_sample <- sample(1:nrow(UniversityAdmissions), 100, replace=FALSE)
sample_100 <- UniversityAdmissions[Q4_sample, ]
index90 <- sample(1:100, 90, replace=FALSE)
sample_90 <- sample_100[index90, ]
sample_10 <- sample_100[-index90, ]



LM_Toefl_90 <- lm(sample_90$`Chance of Admit` ~ sample_90$`TOEFL Score`)
LM_GRE_90 <- lm(sample_90$`Chance of Admit` ~ sample_90$`GRE Score`)

###B

LM_Toefl_100 <- lm(sample_100$`Chance of Admit` ~ sample_100$`TOEFL Score`)
LM_GRE_100 <- lm(sample_100$`Chance of Admit` ~ sample_100$`GRE Score`)

b1_T <- summary(LM_Toefl_100)$coefficients[2, 1]
b1_G <- summary(LM_GRE_100)$coefficients[2, 1]

se_T <- summary(LM_Toefl_100)$coefficients[2, 2]
se_G <- summary(LM_GRE_100)$coefficients[2, 2]

df_Q4 <- 100 - 2

T_low <- b1_T - qt(0.975, df = df_Q4) * se_T
T_high <- b1_T + qt(0.975, df = df_Q4) * se_T


G_low <- b1_G - qt(0.975, df = df_Q4) * se_G
G_high <- b1_G + qt(0.975, df = df_Q4) * se_G


###C
pred_Toefl <- summary(LM_Toefl_100)$coefficients[1, 1] + summary(LM_Toefl_100)$coefficients[2, 1]*sample_10$`TOEFL Score`
pred_GRE <- summary(LM_GRE_100)$coefficients[1, 1] + summary(LM_GRE_100)$coefficients[2, 1]*sample_10$`GRE Score`

###D

x2_Toefl <- sum( ((pred_Toefl - sample_10$`TOEFL Score`)**2)/ sample_10$`TOEFL Score` )
pchisq(x2_Toefl, 9, lower.tail = FALSE)

x2_GRE <- sum( ((pred_GRE - sample_10$`GRE Score`)**2)/ sample_10$`GRE Score` )
pchisq(x2_GRE, length(pred_GRE) - 1, lower.tail = FALSE)
 
##########5

###A

model.matrix(~0+., data=UniversityAdmissions) %>% 
       cor(use="pairwise.complete.obs") %>% 
       ggcorrplot(show.diag = FALSE, type="lower", lab=TRUE, lab_size=2)
###B
LM_multiple <- lm(UniversityAdmissions$`Chance of Admit` ~ UniversityAdmissions$`GRE Score` + UniversityAdmissions$`TOEFL Score` + UniversityAdmissions$SOP + UniversityAdmissions$CGPA + UniversityAdmissions$LOR)
summary(LM_multiple)




##########6
####A

library(oddsratio)

logest <- glm(UniversityAdmissions$Research ~ UniversityAdmissions$`GRE Score` + UniversityAdmissions$`TOEFL Score` + UniversityAdmissions$SOP + UniversityAdmissions$CGPA + UniversityAdmissions$LOR, family = binomial)
summary(logest)

####B

x <- seq(0, 1, length = 100)
log_OR <- summary(logest)$coefficients[4, 1]
OR <- exp(log_OR)
y <- (OR * x) / (((OR -1) * x) + 1)
df <- data.frame(x = x, y = y) 
ggplot(data = df, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  geom_line(y = x, color = "black", alpha = 1) +
  labs(x = "Research | no Internship", y = "Research | Internship") 




#########7


LM_multiple_final <- lm(UniversityAdmissions$`Chance of Admit` ~ UniversityAdmissions$`GRE Score` + UniversityAdmissions$`TOEFL Score` + UniversityAdmissions$SOP + UniversityAdmissions$CGPA + UniversityAdmissions$LOR + UniversityAdmissions$`University Rating` + UniversityAdmissions$internship_abroad + UniversityAdmissions$Research)

summary(LM_multiple_final)

