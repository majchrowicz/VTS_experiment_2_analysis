# Libraries and settings --------------------------------------------------

{
  # load libraries, use install.packages('package_name') if library not yet installed
  library(tidyverse)
  library(easystats)
  library(rstatix)
  library(ggplot2)
  library(ggdist)
  library(cowplot)
  library(readbulk)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(Hmisc)
  library(RColorBrewer) # display.brewer.all()
  library(ggthemes)
  library(grid)
  library(gridExtra)
  library(scales)
  library(knitr)
  library(faux)
  library(sjPlot)
  library(janitor)
  library(beepr)
  library(tictoc)
  library(ggeffects)
  
  # some settings for convenience
  options(scipen=999, width = 150,
          pillar.neg = F)
  Sys.setenv(LANG = "en")
  theme_set(theme_bw(base_size = 14) +
              theme(panel.grid.minor = element_blank()))
  '%nin%' <- Negate("%in%")
  
  library(default)
  default(get_summary_stats) <- list(type = 'common') # get_summary_stats <- reset_default(get_summary_stats)
  default(kable) <- list(digits = 3, format = 'simple')
  default(tab_model) <- list(p.val = 'satterthwaite', show.re.var=F, show.obs=F, show.ngroups=F, 
                             dv.labels='', auto.label = FALSE, p.style = 'numeric')
  default(labs) <- list(title=element_blank())
  cperf <- compare_performance # just alias
}

# Load data ---------------------------------------------------------------

# VTS
d6 <- read_csv('d6.csv') %>% 
  select(-1)
d6

# BHT
b3 <- read_csv('b3.csv') %>% 
  select(-1, -3, -4, -c('frame_rate', 'trial_time', 'fb_delay_index'))
b3

# explicit VTS
ev3 <- read_csv('ev3.csv') %>% 
  select(-1, -ratings)

# Wrangle -----------------------------------------------------------------

# VTS
d7 <- d6 %>% 
  mutate(trial = case_when(block_nr == 1 ~ trial_nr,
                           block_nr == 2 ~ trial_nr+72,
                           block_nr == 3 ~ trial_nr+144),
         .after = trial_nr) %>% 
  mutate(blocks = case_when(block_nr == 1 ~ -1,
                            block_nr == 2 ~ 0,
                            block_nr == 3 ~ 1),
         .after = block_nr) %>% 
  mutate(trials = standardise(trial), .after = trial) %>% 
  mutate(nasas  = standardise(nasa),  .after = nasa) %>% 
  mutate(highdemand = as_factor(case_when(task == 1 ~ 0,
                                task == 2 ~ 1)),
         .after = task) %>% 
  mutate(condition = as_factor(condition),
         switch = as_factor(switch),
         task = as_factor(task),
         correct = as_factor(correct)) %>% 
  mutate(ibs  = standardise(ib),
         ib_sec = ib*1000,  
         .after = ib) %>% 
  mutate(agef1s= standardise(agef1),
         agef2s= standardise(agef2),
         agef3s= standardise(agef3),
         agef4s= standardise(agef4),
         .after = agef4) %>% 
  mutate(joa = standardise(agef1 + agef2),
         effort = standardise(agef3 + agef4),
         .after = agef4s) %>% 
  mutate(BHT_accs = standardise(BHT_acc))
  
contrasts(d7$condition) <- c(-0.5,0.5); contrasts(d7$condition)
contrasts(d7$switch) <- c(-0.5,0.5); contrasts(d7$switch)
contrasts(d7$task) <- c(-0.5,0.5); contrasts(d7$task) # 1 for locaction, 2 for shape
contrasts(d7$highdemand) <- c(-0.5,0.5); contrasts(d7$highdemand) # 0 for locaction, 1 for shape
contrasts(d7$correct) <- c(-0.5,0.5); contrasts(d7$correct) # 

de1 <- d7 %>% # early subset
  filter(block_nr == 1) %>% 
  mutate(trials = standardise(trial), .after = trial)

contrasts(de1$condition) <- c(-0.5,0.5); contrasts(de1$condition)

ev4 <- ev3 %>% 
  mutate(agef1s_vts = standardise(agef1),
         agef2s_vts = standardise(agef2),
         agef3s_vts = standardise(agef3),
         agef4s_vts = standardise(agef4),
         .after = agef4) %>% 
  mutate(nasas_vts = standardise(nasa),
         condition = as_factor(condition)) %>% 
  rename(agef1_vts = agef1,
         agef2_vts = agef2,
         agef3_vts = agef3,
         agef4_vts = agef4,
         nasa1_vts = nasa1,
         nasa2_vts = nasa2,
         nasa3_vts = nasa3,
         nasa4_vts = nasa4,
         nasa_vts = nasa
         )

d8 <- d7 %>% 
  left_join(ev4)

# stronger IB cleaning: ids with r > 0.5 between start and report point
out_ib <- c(203, 205, 209, 211, 213, 229, 235, 239, 258,  
            276, 286, 301, 308, 311, 318)

d7strongcleaning <- d7 %>% 
  mutate(ib = case_when(id %in% out_ib ~ NA,
                        TRUE ~ ib),
         ibs = case_when(id %in% out_ib ~ NA,
                        TRUE ~ ibs),
         ib_sec = case_when(id %in% out_ib ~ NA,
                        TRUE ~ ib_sec))

d7strongcleaning %>% select(id,ib) %>% filter(id >= 318) %>% tail()

# BHT
b4 <- b3 %>% 
  mutate(condition = as_factor(condition),
         problems = standardise(problem_nr), .after=problem_nr) %>% 
  mutate(ibs  = standardise(ib),
         ib_sec = ib*1000,  
         .after = ib) %>% 
  left_join(d7 %>% 
              group_by(id) %>% 
              slice(1) %>% 
              select(id, condition, BHT_acc, starts_with('agef'), joa, effort, nasa, nasas)) %>% 
  rename(problem = problem_nr) %>% 
  mutate(BHT_accs = standardise(BHT_acc),
         correct = as_factor(correct)) %>% 
  mutate(trial = case_when(problem == 1 ~ trial_nr+1,
                           problem == 2 ~ trial_nr+13,
                           problem == 3 ~ trial_nr+25,
                           problem == 4 ~ trial_nr+37,
                           problem == 5 ~ trial_nr+49,
                           problem == 6 ~ trial_nr+61),
         .after=trial_nr) %>% 
  mutate(trials = standardise(trial), .after=trial)

  
contrasts(b4$condition) <- c(-0.5,0.5); contrasts(b4$condition)
contrasts(b4$correct) <- c(-0.5,0.5); contrasts(b4$correct) # 


# Sample ------------------------------------------------------------------

demo <- d7 %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup

demo %>% 
  freq_table(condition, sex) %>% 
  htmlTable

demo %>% 
  freq_table(sex)

demo %>% 
  get_summary_stats(age)

# EDA ---------------------------------------------------------------------

d7 %>% 
  # select(switch)
  group_by(condition) %>%
  get_summary_stats(switch)

ib_trials <- d7 %>% 
  group_by(condition, trial) %>% 
  get_summary_stats(ib) 

ggplot(ib_trials,aes(x = trial, y = mean)) +
  # facet_wrap(~condition, labeller = label_both) +
  geom_point(alpha = .6) +
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se), alpha = .6, width = NA) +
  geom_smooth(aes(color = condition)) +
  labs(y = 'IB', x = 'Trial nr')

ib_problems_bht <- b4 %>% 
  group_by(condition, problem) %>% 
  get_summary_stats(ib) 

ggplot(ib_problems_bht, aes(x = problem, y = mean)) +
  facet_wrap(~condition, labeller = label_both) +
  geom_point(alpha = .6) +
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se), alpha = .6, width = NA) +
  geom_smooth(aes(color = condition), method = 'gam') +
  labs(y = 'IB', x = 'Problem nr')

ib_trials_bht <- b4 %>% 
  group_by(condition, trial_nr) %>% 
  get_summary_stats(ib) 

ggplot(ib_trials_bht, aes(x = trial_nr, y = mean)) +
  facet_wrap(~condition, labeller = label_both) +
  geom_point(alpha = .6) +
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se), alpha = .6, width = NA) +
  geom_smooth(aes(color = condition)) +
  labs(y = 'IB', x = 'Trial nr')


ib_trials_corr_bht <- b4 %>% 
  group_by(condition, correct, trial) %>% 
  get_summary_stats(ib) %>% 
  mutate(correct = as_factor(correct))

ggplot(ib_trials_corr_bht, aes(x = trial, y = mean, colour = correct)) +
  facet_wrap(~condition, labeller = label_both) +
  geom_point(alpha = .6) +
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se), alpha = .6, width = NA) +
  # geom_smooth() +
  geom_smooth(method = 'glm') +
  labs(y = 'IB', x = 'Trial nr')

# Correlations ------------------------------------------------------------

g2
cor <- correlation(g2 %>% select(-c(id,condition,joa)),
                   partial=F,
                   p_adjust='fdr')
cor
summary(cor) 
tbl <- kable(summary(cor), format = "html", escape = FALSE)
tbl %>% html_preview()
print(xtable(summary(cor) ), type = "html",file = 'myfile.html')

library(see) # for plotting
library(ggraph) # needs to be loaded

cors <- cor %>% plot() +
  scale_edge_color_continuous(low = "black", high = "orange")

ggsave('cors.pdf', device = cairo_pdf, w=8,h=6.5)

cor %>%
  summary(redundant = F) %>%
  plot() +
  scale_color_okabeito()

g3 <- d8 %>%
  group_by(id, condition) %>%
  summarise(ibs = mean(ibs, na.rm=T)) %>%
  left_join(d8 %>%
              group_by(id) %>%
              select(id, BHT_acc,
                     # nasa1, nasa2, nasa3, nasa4, nasa,
                     # nasa1_vts, nasa2_vts, nasa3_vts, nasa4_vts, nasa_vts,
                     nasa, nasa_vts,
                     agef1, agef2, agef3, agef4,
                     agef1_vts, agef2_vts, agef3_vts, agef4_vts,
                     ) %>% 
              # select(id, starts_with('agef'), starts_with('nasa'), BHT_acc) %>%
              slice(1)) %>% 
  ungroup()

cor2 <- correlation(g3 %>% select(-c(id,condition)),
                   partial=F,
                   p_adjust='fdr')
cor2
summary(cor2)

cor2 %>%
  summary(redundant = F) %>%
  plot()  + labs(title = element_blank())


# Explicit ----------------------------------------------------------------

soa <- g3 %>% 
  # select() %>% 
  pivot_longer(
    cols = nasa:agef4_vts,
    names_to = 'names',
    values_to = 'val'
  )

nasa <- g3 %>% 
  select(id,condition,ibs,BHT_acc,nasa,nasa_vts) %>% 
  pivot_longer(cols = nasa:nasa_vts, names_to = 'task', values_to = 'nasa') %>% 
  mutate(task = if_else(str_ends(task, '_vts'), 'vts', 'bht' ))

sense_control <- g3 %>% 
  select(id,condition,ibs,BHT_acc,agef1, agef1_vts) %>% 
  pivot_longer(cols = c(agef1, agef1_vts), names_to = 'task', values_to = 'sense_control') %>% 
  mutate(task = if_else(str_ends(task, '_vts'), 'vts', 'bht' ))

predictability <- g3 %>% 
  select(id,condition,ibs,BHT_acc,agef2, agef2_vts) %>% 
  pivot_longer(cols = c(agef2, agef2_vts), names_to = 'task', values_to = 'predictability') %>% 
  mutate(task = if_else(str_ends(task, '_vts'), 'vts', 'bht' ))

effort_task <- g3 %>% 
  select(id,condition,ibs,BHT_acc,agef3, agef3_vts) %>% 
  pivot_longer(cols = c(agef3, agef3_vts), names_to = 'task', values_to = 'effort_task') %>% 
  mutate(task = if_else(str_ends(task, '_vts'), 'vts', 'bht' ))

effort_put <- g3 %>% 
  select(id,condition,ibs,BHT_acc,agef4, agef4_vts) %>% 
  pivot_longer(cols = c(agef4, agef4_vts), names_to = 'task', values_to = 'effort_put') %>% 
  mutate(task = if_else(str_ends(task, '_vts'), 'vts', 'bht' ))

expl1 <- nasa %>% left_join(sense_control) %>% left_join(predictability) %>% left_join(effort_task) %>% left_join(effort_put)
expl1

expl1 %>%
  group_by(condition) %>%
  t_test(sense_control ~ task, paired = T) %>%
  left_join(expl1 %>%
              group_by(condition) %>%
              cohens_d(sense_control ~ task, paired = T))

expl1 %>% 
  group_by(task) %>% 
  t_test(sense_control ~ condition) %>% 
  left_join(expl1 %>% 
              group_by(task) %>%
              cohens_d(sense_control ~ condition))

expl1 %>% 
  anova_test(sense_control ~ task * condition)

ggplot(expl1, aes(y = sense_control, x = task)) +
  geom_boxplot() +
  geom_point(position = position_dodge2(w = 0.4), alpha = 0.5)  +
  facet_wrap(~condition, labeller = label_both) 
  
ggplot(expl1, aes(y = sense_control, x = condition)) +
  geom_boxplot() +
  geom_point(position = position_dodge2(w = 0.4), alpha = 0.5)  +
  facet_wrap(~task, labeller = label_both) +
  labs(x = 'Condition', y = 'Sense of control')

# Modelling ---------------------------------------------------------------


# Explicit control --------------------------------------------------------

ec1 <- lmer(agef1 ~ condition + (1|id), d7)
summary(ec1)

# Switching ---------------------------------------------------------------

s1 <- glmer(switch ~ condition + (1|id),
            d7, family = binomial,
            glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))
summary(s1) # pre-reg
tab_model(s1)

s2 <- glmer(switch ~ condition * blocks + (1|id),
            d7, family = binomial,
            glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))

s3 <- glmer(switch ~ condition * trials + (1|id),
            d7, family = binomial,
            glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))

s4 <- glmer(switch ~ condition * trials + (trials|id),
            d7, family = binomial,
            glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))

anova(s3,s4)
summary(s1)

ggpredict(s2, c("blocks","condition")) %>% 
  plot(alpha=0.3, line.size=1)

ggpredict(s4, c("trials","condition")) %>% 
  plot(alpha=0.3, line.size=1)

s5 <- glmer(switch ~ condition * trials * nasas + (1|id),
            d7, family = binomial,
            glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))

summary(s5)

s6 <- glmer(switch ~ condition * trials * nasas + (trials|id),
            d7, family = binomial,
            glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))

summary(s6) # best theoretical
tab_model(s6, df.method = 'wald', show.stat = T)
ggpredict(s6, c("trials","condition")) %>% 
  plot(alpha=0.3, line.size=1) + labs()

contrast(emtrends(s6, ~ condition, var = 'trials'), # comparison of slopes between levels
         "pairwise", simple = 'each', combine = TRUE, adjust = "none") %>%  
  as_tibble() %>% mutate_at("p.value", list(~round(.,4))) %>% select(-df,-SE)  %>% kable  

test(emtrends(s6, ~ condition, var = 'trials'), 
     adjust = "none") %>% # test slopes against 0, average factors
  as_tibble() %>% select(-df, -SE) %>% kable()

emmeans(s6, pairwise ~ condition | trials, 
        at = list(trials = c(-2,0,2)))$contrasts %>%  # focused effect of cond
  as_tibble %>% select(-df, -SE) %>% adjust_pvalue(p.col = 'p.value', output.col = 'adj.p', method = 'fdr') %>% kable  

s7 <- glmer(switch ~ condition * trials * ibs + (trials|id),
            d7, family = binomial,
            glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))

summary(s7)
tab_model(s7, df.method = 'wald', show.stat = T)
cperf(s4,s7)
ggpredict(s7, c("trials","condition")) %>% 
  plot(alpha=0.3, line.size=1) + labs()+ labs(x = 'Trials', y = 'Switch', colour = 'Condition')

s8 <- glmer(switch ~ condition * blocks * ib + (blocks|id),
            d7, family = binomial,
            glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))

summary(s8)
ggpredict(s8, c("blocks","condition")) %>% 
  plot(alpha=0.3, line.size=1)

cperf(s6,s7,s8)


s9 <- glmer(switch ~ condition * trials * ib * nasas + (trials|id),
            d7, family = binomial,
            glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))

summary(s9)
cperf(s6,s7,s8,s9) # s7
ggpredict(s9, c("ibs")) %>% 
  plot(alpha=0.3, line.size=1)

# BHT acc
s10 <- glmer(switch ~ BHT_acc * trials * ib + (trials|id),
            d7, family = binomial,
            glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))

cperf(s7,s10) 
summary(s10)
tab_model(s10, df.method = 'wald')

ggpredict(s10, c("trials","BHT_acc [0, 0.5, 1]")) %>% 
  plot(alpha=0.3, line.size=1) + labs(x = 'Trials', y = 'Switch', colour = 'BHT accuracy')

# explicit
s11 <- glmer(switch ~ BHT_acc * trials * agef1s + (trials|id),
             d7, family = binomial,
             glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))

cperf(s10,s11) 
summary(s11)

s12 <- glmer(switch ~ BHT_acc * trials * joa + (trials|id),
             d7, family = binomial,
             glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))

s13 <- glmer(switch ~ BHT_acc * trials * agef3s + (trials|id),
             d7, family = binomial,
             glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))
summary(s13)
ggpredict(s13, c("trials","BHT_acc [0, 0.5, 1]")) %>% 
  plot(alpha=0.3, line.size=1) + labs()
ggpredict(s13, c("trials",'agef3s [-1.5, 0, 1.5]')) %>% 
  plot(alpha=0.3, line.size=1) + labs()
d7 %>% get_summary_stats(agef3s)

s14 <- glmer(switch ~ BHT_acc * trials * agef4s + (trials|id),
             d7, family = binomial,
             glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))
summary(s14)

cperf(s11,s12,s13,s14)
cperf(s10,s13)
anova(s10,s14)

ggpredict(s11, c("trials","BHT_acc [0, 0.5, 1]", 'agef1')) %>% 
  plot(alpha=0.3, line.size=1) + labs(x = 'Trials', y = 'Switch', colour = 'BHT accuracy')

s15 <- glmer(switch ~ condition * trials * nasas_vts + (trials|id),
             d8, family = binomial,
             glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))
summary(s15)

s16 <- glmer(switch ~ condition * trials * nasas * nasas_vts + (trials|id),
             d8, family = binomial,
             glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))
summary(s16)

s17 <- glmer(switch ~ BHT_acc * trials * nasas_vts + (trials|id),
             d8, family = binomial,
             glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))
summary(s17)
tab_model(s17, df.method = 'wald')

s18 <- glmer(switch ~ BHT_acc * trials * nasas * nasas_vts + (trials|id),
             d8, family = binomial,
             glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))
summary(s18)
cperf(s17,s18) # s17


ggpredict(s17, c("trials","BHT_acc [0, 0.5, 1]")) %>% 
  plot(alpha=0.3, line.size=1) + labs(x = 'Trials', y = 'Switch', colour = 'BHT accuracy')

ggpredict(s17, c("nasas_vts","BHT_acc [0, 0.5, 1]")) %>% 
  plot(alpha=0.3, line.size=1) + labs(x = 'NASA VTS', y = 'Switch', colour = 'BHT accuracy')

# early subset
se1 <- glmer(switch ~ condition * trials + (trials|id),
            de1, family = binomial,
            glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))
summary(se1)

ggpredict(se1, c("trials","condition")) %>% 
  plot(alpha=0.3, line.size=1)

se2 <- glmer(switch ~ condition * trials * ib + (trials|id),
             de1, family = binomial,
             glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))
summary(se2)
tab_model(se2)

ggpredict(se2, c("trials","condition")) %>% 
  plot(alpha=0.3, line.size=1) + labs() + labs(x = 'Trials', y = 'Switch', colour = 'Condition')

plot(cperf(se1,se2))

se3 <- glmer(switch ~ BHT_acc * trials * ib + (trials|id),
             de1, family = binomial,
             glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))
summary(se3)
ggpredict(se3, c("trials","BHT_acc [0, 0.5, 1]")) %>% 
  plot(alpha=0.3, line.size=1) + labs(x = 'Trials', y = 'Switch', colour = 'BHT accuracy')

# IB VTS -------------------------------------------------------------------

i3 <- lmer(ib ~ condition * trials + (1|id), d7,
           control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))   
i4 <- lmer(ib ~ condition * trials + (trials|id), d7,
           control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))  
cperf(i3,i4)

summary(i4) # pre-reg
tab_model(i4)
i4check <- check_model(i4)
cperf(i3,i4)
test_performance(i3,i4)

ggpredict(i4, c("trials","condition")) %>% 
  plot(alpha=0.3, line.size=1) + labs(x = 'Trials', y = 'IB', colour = 'Condition')

i5 <- lmer(ib ~ condition * trials * agef1 + (trials|id), d7,
           control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))   

summary(i5)
cperf(i4,i5)

ggpredict(i5, c("trials","condition", "agef1 [1,4,7]")) %>% 
  plot(alpha=0.3, line.size=1)

ggpredict(i5, c("trials","condition")) %>% 
  plot(alpha=0.3, line.size=1)

i6 <- lmer(ib ~ condition * trials * agef1 * nasas + (trials|id), d7,
           control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))   

summary(i6)
cperf(i4,i5,i6) # i4


i7 <- lmer(ib ~ condition * trials * nasas + (trials|id), d7,
           control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))   

summary(i7) # theoretical best
cperf(i4,i7) # i4
ggpredict(i7, c("trials","condition", "nasas [-1,0,1]")) %>% 
  plot(alpha=0.3, line.size=1)

ggpredict(i7, c("trials")) %>% 
  plot(alpha=0.3, line.size=1) + labs(x = 'Trials', y = 'IB (s)', colour = 'Condition')

i8 <- lmer(ib ~ condition * trials * switch + (trials|id), d7,
           control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))  

summary(i8)
cperf(i4,i8) # i8

ggpredict(i8, c("trials","condition")) %>% 
  plot(alpha=0.3, line.size=1)
ggpredict(i8, c("trials","condition", "agef1 [1,4,7]", "nasas [-1,0,1]")) %>% 
  plot(alpha=0.3, line.size=1)

i9 <- lmer(ib ~ condition * trials * nasas * agef1 + (trials|id), d7,
           control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))  

summary(i9)
cperf(i8,i9) # i8

ggpredict(i9, c("switch")) %>% 
  plot(alpha=0.3, line.size=1)


i10 <- lmer(ib ~ condition * trials * nasas * highdemand + (trials|id), d7,
           control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))  

summary(i10)
tab_model(i10)
cperf(i8,i10) 

i11 <- lmer(ib ~ condition * trials * agef1s * highdemand + (trials|id), d7,
            control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))  

summary(i11)
cperf(i10,i11)

# contrasts
summary(i10)

facet_highdemand <- c('0' = 'Low demand task', '1' = 'High demand task')
ggpredict(i10, c("nasas","condition", "highdemand")) %>% # as_tibble()
  plot(alpha=0.3, line.size=1) + labs(y = 'IB', x = 'NASA', colour = 'Condition') +
  facet_grid(. ~ facet, labeller = as_labeller(facet_highdemand))


contrast(emtrends(i10, ~ condition * highdemand, var = 'nasas'), # comparison of slopes between levels
         "pairwise", simple = 'each', combine = TRUE, adjust = "none") %>%  
  as_tibble() %>% mutate_at("p.value", list(~round(.,4))) %>% select(-df,-SE)  %>% kable  

test(emtrends(i10, ~ condition * highdemand, var = 'nasas'), 
     adjust = "none") %>% # test slopes against 0, average factors
  as_tibble() %>% select(-df, -SE) %>% kable()

emmeans(i10, pairwise ~ condition | highdemand | nasas, 
        at = list(nasas = c(-1,0,1)))$contrasts %>%  # focused effect of cond
  as_tibble %>% select(-df, -SE) %>% adjust_pvalue(p.col = 'p.value', output.col = 'adj.p', method = 'fdr') %>% kable  

# BHT_acc

i14 <- lmer(ib ~ BHT_acc * trials * nasas + (trials|id), d7,
            control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))  
summary(i14)  

i15 <- lmer(ib ~ BHT_acc * trials * switch + (trials|id), d7,
           control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))  
summary(i15)  
tab_model(i15)
ggpredict(i15, c("trials","BHT_acc [0, 0.5, 1]")) %>% 
  plot(alpha=0.3, line.size=1) + labs(x = 'Trials', y = 'IB', colour = 'BHT accuracy')

i16 <- lmer(ib ~ BHT_acc * trials * switch * highdemand + (trials|id), d7,
            control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))  
summary(i16)  
cperf(i14,i15,i16)
anova(i15,i16)

i17 <- lmer(ib ~ BHT_acc * trials * nasas_vts + (trials|id), d8,
            control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))  
summary(i17) # reporting? interesting interactions
tab_model(i17)
cperf(i14,i17)

ggpredict(i17, c("trials","BHT_acc [-1,0,1]")) %>% 
  plot(alpha=0.3, line.size=1) + labs(x = 'Trial', y = 'IB', colour = 'BHT accuracy')

i18 <- lmer(ib ~ BHT_acc * trials * nasas * nasas_vts + (trials|id), d8,
            control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))  
summary(i18) 
tab_model(i18)
cperf(i14,i17, i18)

ggpredict(i18, c("trials","BHT_acc [-1,0,1]")) %>% 
  plot(alpha=0.3, line.size=1) + labs()

ggpredict(i18, c("nasas_vts","nasas [-1,0,1]")) %>% 
  plot(alpha=0.3, line.size=1) + labs(x = 'NASA VTS', y = 'IB', colour = 'NASA BHT')

ggpredict(i18, c("nasas_vts","nasas [-1,0,1]","BHT_acc [-1,0,1]")) %>% 
  plot(alpha=0.3, line.size=1) + labs(x = 'NASA VTS', y = 'IB', colour = 'NASA BHT')

ggpredict(i18, c("nasas_vts","BHT_acc [-1,0,1]" ,"nasas [-1,0,1]")) %>% 
  plot(alpha=0.3, line.size=1) + labs(x = 'NASA VTS', y = 'IB', colour = 'BHT accuracy')

ggpredict(i18, c("nasas_vts","nasas [-1,0,1]","BHT_acc [-1,0,1]", "trials [-1,0,1")) %>% 
  plot(alpha=0.3, line.size=1) + labs(x = 'NASA VTS', y = 'IB', colour = 'NASA BHT')

i19 <- lmer(ib ~ condition * trials * nasas_vts + (trials|id), d8,
            control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))  
summary(i19) 
cperf(i19, i18)

# early subset
ie1 <- lmer(ib ~ condition * trials + (trials|id), de1,
            control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))   

ie2 <- lmer(ib ~ condition * trials * agef1s + (trials|id), de1,
            control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))   

cperf(ie1,ie2)
summary(ie2)
tab_model(ie2, dv.labels = format(formula(.)))
tab_model(ie2)
ie2 %>% tab_model(dv.labels = format(formula(.)))

ggpredict(ie2, c("trials","condition", "agef1s [-1.5,0,1.5]")) %>% 
  plot(alpha=0.3, line.size=1) + labs()+ labs(x = 'Trials', y = 'IB', colour = 'Condition')

ggpredict(ie2, c("trials","condition")) %>% 
  plot(alpha=0.3, line.size=1) + labs()+ labs(x = 'Trials', y = 'IB', colour = 'Condition')

ie3 <- lmer(ib ~ BHT_acc * trials * switch + (trials|id), de1,
            control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))   
summary(ie3)


# subjective predictors
is1 <- lmer(ib ~ agef1 * agef2 * agef3 * agef4 + (1|id), de1,
            control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))   
is1 <- lmer(ib ~ agef1 * agef2 * condition + (1|id), de1,
            control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))   
is1 <- lmer(ib ~ agef1 * agef2 * nasas + (1|id), de1,
            control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))   
summary(is1)
cperf(i8,is1)

# stronger cleaning
io1 <- lmer(ib ~ condition * trials + (trials|id), d7strongcleaning,
           control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))   
summary(io1) # pre-reg

io2 <- lmer(ib ~ condition * trials * agef1 + (trials|id), d7strongcleaning,
           control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))   
summary(io2)

# contrasts with BHT acc
summary(i15)

ggpredict(i15, c("trials","BHT_acc [0, 0.5, 1]")) %>% 
  plot(alpha=0.3, line.size=1) + labs(x = 'Trials', y = 'IB', colour = 'BHT accuracy')


summary(i17) 
ggpredict(i17, c("trials","BHT_acc [-1,0,1]")) %>% 
  plot(alpha=0.3, line.size=1) + labs(x = 'Trial', y = 'IB', colour = 'BHT accuracy')


contrast(emtrends(i17, ~ BHT_acc, var = 'trials', at=list(BHT_acc=c(-1,0,1))), # comparison of slopes between levels
         "pairwise", simple = 'BHT_acc', combine = TRUE, adjust = "none") %>%  
  as_tibble() %>% mutate_at("p.value", list(~round(.,4))) %>% select(-df,-SE)  %>% kable  

test(emtrends(i17, ~ BHT_acc, var = 'trials', at=list(BHT_acc=c(-1,0,1))), 
     adjust = "none") %>% # test slopes against 0, average factors
  as_tibble() %>% select(-df, -SE) %>% kable()

emmeans(i17, pairwise ~ BHT_acc | trials, 
        at = list(BHT_acc = c(-1,0,1), trials = c(-1,0,1)))$contrasts %>%  # focused effect of BHT_acc
  as_tibble %>% select(-df, -SE) %>% adjust_pvalue(p.col = 'p.value', output.col = 'adj.p', method = 'fdr') %>% kable  

# valence

mv1 <- lmer(ib ~ condition * trials * nasas * correct + (trials + correct|id), d8)
summary(mv1)

ggpredict(mv1, c("correct")) %>% 
  plot(alpha=0.3, line.size=1) + labs() 

ggplot(d8, aes())

mv2 <- lmer(ib ~ condition * trials * nasas * correct + (trials|id), d8)
summary(mv2)

mv3 <- lmer(ib ~ (condition * trials * nasas) + 
             (condition * trials * correct) + 
             (nasas * trials * correct) + 
             (trials + correct|id), d8)
summary(mv3) # theoretical best

mv4 <- lmer(ib ~ (condition * trials * nasas_vts) + 
              (condition * trials * correct) + 
              (nasas_vts * trials * correct) + 
              (trials|id), d8)
summary(mv4) 
cperf(mv2,mv3,mv4) # mv3

mv5 <- lmer(ib ~ (BHT_acc * trials * nasas_vts) + 
              (BHT_acc * trials * correct) + 
              (nasas_vts * trials * correct) + 
              (trials|id), d8)
summary(mv5) 

mv6 <- lmer(ib ~ (BHT_acc * trials * nasas) + 
              (BHT_acc * trials * correct) + 
              (nasas * trials * correct) + 
              (trials + correct|id), d8)
summary(mv6) 

# IB BHT ------------------------------------------------------------------

ii1 <- lmer(ib ~ condition * problems + (problems|id), b4)

summary(ii1)
ggpredict(ii1, c("problems","condition")) %>% 
  plot(alpha=0.3, line.size=1)

ii2 <- lmer(ib ~ condition * problems * nasas + (problems|id), b4)
summary(ii2)
tab_model(ii2)
cperf(ii1,ii2)

ggpredict(ii2, c("problems","condition", "nasas")) %>% 
  plot(alpha=0.3, line.size=1) + labs()+
  labs(x = 'Problem', y='IB',color='Condition', title=element_blank())

ggpredict(ii2, c("problems","condition")) %>% 
  plot(alpha=0.3, line.size=1) + labs()+
  labs(x = 'Problem', y='IB',color='Condition', title=element_blank())

ii2b <- lmer(ib ~ condition * trials * nasas + (trials|id), b4)
summary(ii2b)
tab_model(ii2)
cperf(ii2,ii2b)

ggpredict(ii2b, c("trials","condition", "nasas")) %>% 
  plot(alpha=0.3, line.size=1) + labs()+
  labs(x = 'Trial', y='IB',color='Condition', title=element_blank())


ii3 <- lmer(ib ~ condition * problems * nasas * agef1s + (problems|id), b4)
summary(ii3)
cperf(ii2,ii3) # ii2
anova(ii2,ii3)

ii4 <- lmer(ib ~ BHT_acc * problems + (problems|id), b4)
ii4b <- lmer(ib ~ BHT_accs * problems + (problems|id), b4)
ii5 <- lmer(ib ~ BHT_acc * problems * nasas + (problems|id), b4)
ii5b <- lmer(ib ~ BHT_accs * problems * nasas + (problems|id), b4)
ii6 <- lmer(ib ~ BHT_acc * problems * nasas * agef3s + (problems|id), b4)
ii7 <- lmer(ib ~ BHT_acc * problems * agef3s + (problems|id), b4)
ii8 <- lmer(ib ~ BHT_acc * problems * agef4s + (problems|id), b4)
ii9 <- lmer(ib ~ BHT_acc * problems * agef2s + (problems|id), b4)
summary(ii4)
summary(ii4b)
summary(ii5)
summary(ii5b)
summary(ii6)
summary(ii7)
tab_model(ii7)
cperf(ii5,ii5b,ii6,ii7,ii8,ii9)  # ii7 >= 5

ggpredict(ii5, c("problems","BHT_acc [0, 0.5, 1]", 'nasas [-1,0,1]')) %>% 
  plot(alpha=0.3, line.size=1) + labs()
ggpredict(ii5b, c("problems","BHT_accs [-1,0,1]", 'nasas [-1,0,1]')) %>% 
  plot(alpha=0.3, line.size=1) + labs()

ggpredict(ii5b, c("problems", 'nasas')) %>% 
  plot(alpha=0.3, line.size=1) + labs()

ggpredict(ii7, c("problems","BHT_acc [0, 0.5, 1]", 'agef3s [-1.5,0,1.5]')) %>% 
  plot(alpha=0.3, line.size=1) + labs()

ii9 <- lmer(ib ~ agef1 * agef2 * agef3 * agef4 + (1|id), b4)
summary(ii9)

# contrasts
summary(ii2)

ggpredict(ii2, c("problems","condition", "nasas")) %>% 
  plot(alpha=0.3, line.size=1) + labs()+
  labs(x = 'Problem', y='IB',color='Condition', title=element_blank())


contrast(emtrends(ii2, ~ condition * nasas, var = 'problems', at=list(nasas=c(-1,0,1))), # comparison of slopes between levels
         "pairwise", simple = 'condition', combine = TRUE, adjust = "none") %>%  
  as_tibble() %>% mutate_at("p.value", list(~round(.,4))) %>% select(-df,-SE)  %>% kable  

test(emtrends(ii2, ~ condition * nasas, var = 'problems', at=list(nasas=c(-1,0,1))), 
     adjust = "none") %>% # test slopes against 0, average factors
  as_tibble() %>% select(-df, -SE) %>% kable()

emmeans(ii2, pairwise ~ condition | problems | nasas, 
        at = list(nasas = c(-1,0,1), problems = c(-1,0,1)))$contrasts %>%  # focused effect of cond
  as_tibble %>% select(-df, -SE) %>% adjust_pvalue(p.col = 'p.value', output.col = 'adj.p', method = 'fdr') %>% kable  


summary(ii5b) # ii7 fit better just by decimals, here we have theoretical argument (prev studies, prev analysis, etc)
tab_model(ii5b)
ggpredict(ii5, c("problems","BHT_acc [0, 0.5, 1]", 'nasas [-1,0,1]')) %>% 
  plot(alpha=0.3, line.size=1) + labs()
ggpredict(ii5, c("problems", 'nasas [-1,0,1]')) %>% 
  plot(alpha=0.3, line.size=1) + labs()

# contrast(emtrends(ii5, ~ BHT_acc * nasas, var = 'problems', at=list(nasas=c(-1,0,1), BHT_acc=c(0,0.5,1))), # comparison of slopes between levels
#          "pairwise", simple = 'BHT_acc', combine = TRUE, adjust = "none") %>%  
#   as_tibble() %>% mutate_at("p.value", list(~round(.,4))) %>% select(-df,-SE)  %>% kable  

contrast(emtrends(ii5, ~ nasas, var = 'problems', at=list(nasas=c(-1,0,1))), # comparison of slopes between levels
         "pairwise", simple = 'nasas', combine = TRUE, adjust = "none") %>%  
  as_tibble() %>% mutate_at("p.value", list(~round(.,4))) %>% select(-df,-SE)  %>% kable  

test(emtrends(ii5, ~ nasas, var = 'problems', at=list(nasas=c(-1,0,1))), 
     adjust = "none") %>% # test slopes against 0, average factors
  as_tibble() %>% select(-df, -SE) %>% kable()

emmeans(ii5, pairwise ~ nasas | problems, 
        at = list(nasas = c(-1,0,1), problems = c(-1,0,1)))$contrasts %>%  # effect at points
  as_tibble %>% select(-df, -SE) %>% kable  

# valence

v1 <- lmer(ib ~ condition * trials * nasas + (trials|id), b4)
summary(v1)

ggpredict(v1, c("trials","condition", "nasas")) %>% 
  plot(alpha=0.3, line.size=1) + labs()+
  labs(x = 'Trial', y='IB',color='Condition', title=element_blank())

v2 <- lmer(ib ~ condition * trials * nasas * correct + (trials + correct|id), b4)
summary(v2)

ggpredict(v2, c("trials","condition", "nasas")) %>% 
  plot(alpha=0.3, line.size=1) + labs()+
  labs(x = 'Trial', y='IB',color='Condition')

ggpredict(v2, c("correct")) %>% 
  plot(alpha=0.3, line.size=1) + labs() 

v3 <- lmer(ib ~ condition * problems * nasas * correct + (problems|id), b4)
summary(v3)
cperf(v2,v3)
cperf(ii2, v2,v3)

v4 <- lmer(ib ~ (condition * trials * nasas) + 
             (condition * trials * correct) + 
             (nasas * trials * correct) + 
             (trials + correct|id), b4)
summary(v4) # best
tab_model(v4)
cperf(v2,v3,v4)
anova(v2,v4)

ggpredict(v4, c("trials","condition", "nasas")) %>% 
  plot(alpha=0.3, line.size=1) + labs()+
  labs(x = 'Trial', y='IB (s)',color='Condition')

ggpredict(v4, c("correct"), x.as.factor = TRUE) %>% 
  plot(alpha=0.3, line.size=1) + labs(y = 'IB (s)', x = 'Feedback') +
  scale_x_discrete(labels = c('0' = "Uncontrollable", '1' = "Controllable"))

cperf(ii2, ii2b, v4) # v4

contrast(emtrends(v4, ~ condition * nasas, var = 'trials', at=list(nasas=c(-1,0,1))), # comparison of slopes between levels
         "pairwise", simple = 'condition', combine = TRUE, adjust = "none") %>%  
  as_tibble() %>% mutate_at("p.value", list(~round(.,4))) %>% select(-df,-SE)  %>% kable  

test(emtrends(v4, ~ condition * nasas, var = 'trials', at=list(nasas=c(-1,0,1))), 
     adjust = "none") %>% # test slopes against 0, average factors
  as_tibble() %>% select(-df, -SE) %>% kable()

emmeans(v4, pairwise ~ condition | trials | nasas, 
        at = list(nasas = c(-1,0,1), trials = c(-1,0,1)))$contrasts %>%  # focused effect of cond
  as_tibble %>% select(-df, -SE) %>% adjust_pvalue(p.col = 'p.value', output.col = 'adj.p', method = 'fdr') %>% kable  

# BHT acc

va1 <- lmer(ib ~ (BHT_acc * trials * nasas) + 
              (BHT_acc * trials * correct) + 
              (nasas * trials * correct) + 
              (trials|id), b4)

summary(va1)
tab_model(va1)

ggpredict(va1, c("trials","BHT_acc [-1,0,1]", "nasas")) %>% 
  plot(alpha=0.3, line.size=1) + labs()+
  labs(x = 'Trial', y='IB (s)',color='BHT accuracy\n(SD from the mean)')

test(emtrends(va1, ~ BHT_acc * nasas, var = 'trials', at=list(nasas=c(-1,0,1), BHT_acc = c(-1,0,1))), 
     adjust = "none") %>% # test slopes against 0, average factors
  as_tibble() %>% select(-df, -SE) %>% kable()

ggpredict(va1, c("trials","BHT_acc [-1,0,1]", "correct")) %>% 
  plot(alpha=0.3, line.size=1) + labs()+
  # facet_wrap(~., labeller = label_both) +
  labs(x = 'Trial', y='IB (s)',color='BHT accuracy\n(SD from the mean)')

test(emtrends(va1, ~ BHT_acc * correct, var = 'trials', at=list(BHT_acc = c(-1,0,1))), 
     adjust = "none") %>% # test slopes against 0, average factors
  as_tibble() %>% select(-df, -SE) %>% kable()
# Task selection ----------------------------------------------------------

t1 <- glmer(highdemand ~ condition * trials + (trials|id),
            d7, family = binomial,
            glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))

t2 <- glmer(highdemand ~ condition * trials * switch + (trials|id),
            d7, family = binomial,
            glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))

summary(t2)
ggpredict(t2, c("switch")) %>% 
  plot(alpha=0.3, line.size=1)
ggpredict(t1, c("trials","condition")) %>% 
  plot(alpha=0.3, line.size=1)

t3 <- glmer(highdemand ~ condition * trials * switch * ib + (trials|id),
            d7, family = binomial,
            glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))
summary(t3)
cperf(t1,t2,t3)

t4 <- glmer(highdemand ~ condition * trials * switch * agef1 + (trials|id),
            d7, family = binomial,
            glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))
summary(t4)
cperf(t3,t4) # t3

tab_model(t3)
ggpredict(t3, c("condition")) %>% 
  plot(alpha=0.3, line.size=1) + labs()
ggpredict(t3, c("trials", 'switch')) %>% 
  plot(alpha=0.3, line.size=1) + labs()
d7 %>% get_summary_stats(ib)
ggpredict(t3, c("trials", 'switch', 'ib')) %>% 
  plot(alpha=0.3, line.size=1) + labs(x = 'Trials', y = 'High deman task selection', colour = 'Switch')


# Explicit SoA ------------------------------------------------------------

d7 %>% get_summary_stats(as.integer(condition))

d7s <- d7 %>% 
  mutate(agef1 = standardise(agef1),
         agef2 = standardise(agef2),
         agef3 = standardise(agef3),
         agef4 = standardise(agef4),
         joa = standardise (joa),)


e1 <- lmer(agef1 ~ ibs * nasas + (1|id), d7s,
           control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))   
summary(e1)

e2 <- lmer(agef1 ~ condition * ibs * nasas + (1|id), d7s,
           control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))   
summary(e2)

g1 <- d7s %>%
  group_by(id, condition, block_nr) %>%
  summarise(ibs = mean(ibs, na.rm=T)) %>%
  left_join(d7s %>%
              group_by(id, block_nr) %>%
              select(id, agef1:agef4, joa, nasas) %>%
              slice(1))

e2 <- lm(agef1 ~ ibs * nasas * block_nr, g1)   
summary(e2)


g2 <- d7 %>%
  group_by(id, condition) %>%
  summarise(ibs = mean(ibs, na.rm=T)) %>%
  left_join(d7 %>%
              group_by(id) %>%
              select(id, agef1s:agef4s, joa, nasas, BHT_acc) %>%
              slice(1)) %>% 
  ungroup()

e3 <- lm(agef1 ~ ibs * nasas, g2)   
summary(e3)

cperf(e2,e3)

ggpredict(e3, c('nasas','ibs')) %>% 
  plot(alpha=0.3, line.size=1)

e4 <- lm(agef1 ~ condition * ibs * nasas, g2)   
summary(e4)
cperf(e3,e4)


# RTs ---------------------------------------------------------------------

r1 <- lmer(log(rt) ~ condition * trials + (trials|id), d7,
           control=lmerControl(optCtrl=list(maxfun=1e5), optimizer = 'bobyqa'))   
summary(r1)
ggpredict(r1, c('trials','condition')) %>% 
  plot(alpha=0.3, line.size=1)


# Accuracy ----------------------------------------------------------------

a1 <- glmer(correct ~ condition * trials + (trials|id),
              d7, family = binomial,
              glmerControl(optCtrl=list(maxfun=2e4), optimizer = 'bobyqa'))

summary(a1)
ggpredict(a1, c('trials','condition')) %>% 
  plot(alpha=0.3, line.size=1)


# PEAB --------------------------------------------------------------------

# bht
b5 <- b4 %>% 
  group_by(id, problem) %>% 
  mutate(prevoutc = lag(correct), .after = correct)

contrasts(b5$prevoutc) <- c(-0.5,0.5); contrasts(b5$prevoutc)


mp1 <- lmer(ib ~ (condition * trials * nasas) + 
             (condition * trials * prevoutc) + 
             (nasas * trials * prevoutc) + 
             (trials|id), b5)
summary(mp1)  

cperf(mp1,v4)

# vts
d9 <- d8 %>% 
  group_by(id, block_nr) %>% 
  mutate(prevoutc = lag(correct), .after = correct)

contrasts(d9$prevoutc) <- c(-0.5,0.5); contrasts(d9$prevoutc)

mpv1 <- lmer(ib ~ (condition * trials * nasas) + 
              (condition * trials * prevoutc) + 
              (nasas * trials * prevoutc) + 
              (trials|id), d9)

mpv2 <- lmer(ib ~ (condition * trials * nasas) + 
               (condition * trials * correct) + 
               (nasas * trials * correct) + 
               (trials|id), d9) # like mv3

mpv3 <- lmer(ib ~ (condition * trials * nasas_vts) + 
               (condition * trials * prevoutc) + 
               (nasas_vts * trials * prevoutc) + 
               (trials|id), d9)

summary(mpv1) 
tab_model(mpv1)
cperf(mpv1,mpv2,mpv3)


ggpredict(mpv1, c('prevoutc')) %>% 
  plot(alpha=0.3, line.size=1) + labs()
ggpredict(mpv1, c("trials", 'condition')) %>% 
  plot(alpha=0.3, line.size=1) + labs()
ggpredict(mpv1, c("condition", 'prevoutc')) %>% 
  plot(alpha=0.3, line.size=1) + labs()
ggpredict(mpv1, c("prevoutc", 'condition')) %>% 
  plot(alpha=0.3, line.size=1) + labs()

facet1 <- c('0' = 'Previous error', '1' = 'Previous correct')

ggpredict(mpv1, c("trials","condition", "prevoutc")) %>% 
  plot(alpha=0.3, line.size=1) + labs() + facet_grid(~ facet, labeller = as_labeller(c('0' = 'Previous error', '1' = 'Previous correct')))

ggpredict(mpv1, c("trials","prevoutc", "condition")) %>% 
  plot(alpha=0.3, line.size=1) + labs()  + facet_grid(~ facet, labeller = as_labeller(c('0' = 'Uncontrollable', '1' = 'Controllable')))

ggpredict(mpv1, c("trials","condition", "prevoutc")) %>% 
  ggplot(., aes(x = x, y = predicted, colour = group)) +
  facet_wrap(~facet) +
  geom_line() +
  geom_ribbon(aes(ymin=predicted-conf.low, ymax=predicted+conf.high, fill = group), alpha = 0.15, colour = NA) 
  