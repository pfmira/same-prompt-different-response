## SET UP ####
library(tidyverse)
library(misty)
library(tidyr)
library(lme4)
library(lmerTest)
library("MASS")
library(MuMIn)
library(emmeans)
library(lmerTest)

#read in data
terrible_vacation <- readr::read_csv("terrible_vacation_data.csv")

course_levels = c("SPA 1", "SPA 2","SPA 3", "SPA 21", "SPA 22", 
                  "SPA 23", "SPA 24")

# add tense categories
terrible_vacation <- terrible_vacation |>
  dplyr::rowwise()|>
  dplyr::mutate(
    past_tense = Imperfect_tense + Preterit_tense,
    
    percent_present = Present_tense / total_main_verbs * 100,
    percent_past = past_tense / total_main_verbs * 100,
    percent_future = Future_tense / total_main_verbs * 100,
    percent_conditional = Conditional_tense / total_main_verbs * 100
  )|>
  dplyr::mutate(
    Level = case_when(
      level == 1 ~ "SPA 1",
      level == 2 ~ "SPA 2",
      level == 3 ~ "SPA 3",
      level == 21 ~ "SPA 21",
      level == 22 ~ "SPA 22",
      level == 23 ~ "SPA 23",
      level == 24 ~ "SPA 24",
    )
  ) |>
  dplyr::mutate(Level = factor(Level, level = course_levels))


#just terrible
terrible_only <- terrible_vacation |>
  dplyr::filter(prompt == 'terrible')

#just vacation
vacation_only <- terrible_vacation |>
  dplyr::filter(prompt == 'vacation')


## ANALYSES ####


### RQ1 ####
# RQ 1: How similar (or dissimilar) is the distribution of verbal tenses used by
# learners when responding to two different narrative prompts: “A perfect vacation”
# and “A terrible story”?

#### Graphs ####

#percent past tense by level and prompt
past <- summarySE(data = terrible_vacation, 
                  measurevar = 'percent_past', 
                  groupvars = c('Level', 'prompt'),
                  na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
view(past)

#graph this
ggplot(past, aes(x=Level, y=percent_past, color=prompt)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=percent_past-ci, ymax=percent_past+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.9))+
  ylim(0,100)


#percent conditional tense by level and prompt
cond <- summarySE(data = terrible_vacation, 
                  measurevar = 'percent_conditional', 
                  groupvars = c('Level', 'prompt'),
                  na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
#graph this
ggplot(cond, aes(x=Level, y=percent_conditional, color=prompt)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=percent_conditional-ci, ymax=percent_conditional+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.9))+
  ylim(0,100)

#### Tables ####

# Vacation: mean and SD for every tense/text length by level
vacation_table<- vacation_only %>%
  dplyr::select(Level,
                pres_st = Present_tense / NumTokens,
                past_st = past_tense / NumTokens,
                fut_st = Future_tense / NumTokens,
                cond_st = Conditional_tense / NumTokens) |>
  dplyr::group_by(Level) |>
  dplyr::summarise(round(mean(pres_st),2), round(sd(pres_st),2),
                   round(mean(past_st),2), round(sd(past_st),2),
                   round(mean(fut_st),2), round(sd(fut_st),2),
                   round(mean(cond_st),2), round(sd(cond_st),2))

view(vacation_table)

# Terrible: mean and SD for every tense/text length by level
terrible_table<- terrible_only %>%
  dplyr::select(Level,
                pres_st = Present_tense / NumTokens,
                past_st = past_tense / NumTokens,
                fut_st = Future_tense / NumTokens,
                cond_st = Conditional_tense / NumTokens) |>
  dplyr::group_by(Level) |>
  dplyr::summarise(round(mean(pres_st),2), round(sd(pres_st),2),
                   round(mean(past_st),2), round(sd(past_st),2),
                   round(mean(fut_st),2), round(sd(fut_st),2),
                   round(mean(cond_st),2), round(sd(cond_st),2))

view(terrible_table)

#### Mixed-effects models ####

# Effect of prompt on percentage of present tense verbs to total main verbs
# two prompts: terrible or vacation
# terrible is reference level

rq1_present <- lmer(percent_present ~ prompt + (1 | ID), terrible_vacation)
summary(rq1_present)
r.squaredGLMM(rq1_present)
#significant, more present tense in Vacation

# Effect of prompt on percentage of past tense verbs to total main verbs
# two prompts: terrible or vacation
# terrible is reference level

rq1_past <- lmer(percent_past ~ prompt + (1 | ID), terrible_vacation)
summary(rq1_past)
r.squaredGLMM(rq1_past)
#significant, more past tense in Terrible

# Effect of prompt on percentage of future tense verbs to total main verbs
# two prompts: terrible or vacation
# terrible is reference level

rq1_future <- lmer(percent_future ~ prompt + (1 | ID), terrible_vacation)
summary(rq1_future)
r.squaredGLMM(rq1_future)
#significant, more future tense in Vacation

# Effect of prompt on percentage of conditional tense verbs to total main verbs
# two prompts: terrible or vacation
# terrible is reference level

rq1_conditional <- lmer(percent_conditional ~ prompt + (1 | ID), terrible_vacation)
summary(rq1_conditional)
r.squaredGLMM(rq1_conditional)
#significant, more conditional tense in Vacation


### RQ2 ####

##### Past - Terrible ####
mod10 <- lmer(percent_past ~ Level + (1|ID), 
             terrible_only,
             contrasts = list(Level = contr.sdif),
             control=lmerControl(optimizer="bobyqa",
                                 optCtrl=list(maxfun=2e5)))
summary(mod10)
r.squaredGLMM(mod10)
emmeans(mod10, specs = pairwise~ Level)
#effect sizes (d) here:
eff_size(emmeans(mod10, "Level"), sigma = sigma(mod10), edf = 130)

###### self-assessment ####
mod11 <- lmer(percent_past ~ self_assessment_score + (1|ID), 
              terrible_only,
              #contrasts = list(Level = contr.sdif),
              control=lmerControl(optimizer="bobyqa",
                                  optCtrl=list(maxfun=2e5)))
summary(mod11)
r.squaredGLMM(mod11)
emmeans(mod11, specs = pairwise~ self_assessment_score)

#plot
ggplot(terrible_only, aes(x=self_assessment_score, y=percent_past)) + 
  geom_point(size=2)+
  geom_smooth(method=lm)+
  xlim(0,20)


##### Conditional - Vacation ####
mod12 <- lmer(percent_conditional ~ Level + (1|ID), 
              vacation_only,
              contrasts = list(Level = contr.sdif),
              control=lmerControl(optimizer="bobyqa",
                                  optCtrl=list(maxfun=2e5)))
summary(mod12)
r.squaredGLMM(mod12)
emmeans(mod12, specs = pairwise~ Level)
#effect sizes (d) here:
eff_size(emmeans(mod12, "Level"), sigma = sigma(mod12), edf = 130)

###### self-assessment ####
mod13 <- lmer(percent_conditional ~ self_assessment_score + (1|ID), 
              vacation_only,
              #contrasts = list(Level = contr.sdif),
              control=lmerControl(optimizer="bobyqa",
                                  optCtrl=list(maxfun=2e5)))
summary(mod13)
r.squaredGLMM(mod13)
emmeans(mod13, specs = pairwise~ self_assessment_score)


#plot
ggplot(vacation_only, aes(x=self_assessment_score, y=percent_conditional)) + 
  geom_point(size=2)+
  geom_smooth(method=lm)+
  xlim(0,20)+
  ylim(0, 100)