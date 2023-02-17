# INTRODUCTION TO STATISTICS -------

# PACKAGES -------
library(tidyverse)
library(here)
library(kableExtra)

library(GGally)
library(emmeans)
library(performance)

# DATA -------
darwin <- read.csv(here("data", "darwin.csv"))

# Part I (Ch-12) -------
# DATA CHECK/ANALYSIS ---

# Structure of the data
glimpse(darwin)

# Tidy format
head(darwin)

# Variable names
colnames(darwin)

# Clean up column names
darwin <- janitor::clean_names(darwin)

# Check for duplication
darwin %>% 
  duplicated() %>% 
  sum()

# Typos - by looking at impossible values
darwin %>% 
  summarise(min=min(height, na.rm=TRUE), 
            max=max(height, na.rm=TRUE))

# Typos - by looking at distinct characters/values
darwin %>% 
  distinct(pair)

darwin %>% 
  distinct(type)

# Missing values
darwin %>% 
  is.na() %>% 
  sum()

# Quick summary
summary(darwin)

# --
darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_point()

# --
darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# New object
darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# Summary plot
darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()

# Kable extra function used to make a nice table (could be replaced with kable() if needed)
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

# Differences --
# Pivot data to wide format then subtract Selfed plant heights from Crossed plant heights

darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>% 
  mutate(difference = Cross - Self)

difference_summary <- darwin_wide %>% 
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

difference_summary %>%
  mutate(se=sd/sqrt(n))

# Uncertainty --
# Null hypothesis: There is no difference in the mean height of self vs crossed plants
# Alternate hypothesis: Inbreeding reduces the fitness of the selfed plants, observed as selfed plants on average being smaller than crossed plants

# Sequence of 100 - equally spaced numbers between -4 and 4
x <- seq(-4, 4, length=100)

# Create a vector of values that shows the height of the probability distribution for each value in x
y <- dnorm(x)

# Plot x and y as a scatterplot with connected lines (type = "l") and add an x-axis with custom labels
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))

# --
lowerCI <- 2.62-(2*1.22)
upperCI <- 2.62+(2*1.22)

lowerCI
upperCI

# --
# Part II (Ch-13) -------
# INTRODUCTION TO LINEAR MODELS ---
lsmodel0 <- lm(formula = height ~ 1, data = darwin)

# Summaries for models
summary(lsmodel0)
broom::tidy(lsmodel0)

mean(darwin$height)

lsmodel1 <- lm(height ~ type, data=darwin)
# lsmodel1 <- lm(height ~ 1 + type, data=darwin)

#--
broom::tidy(lsmodel1)

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height))

#--
summary(lsmodel1)

#--
darwin%>%
  ggplot(aes(x=type,
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()

# Confidence intervals
broom::tidy(lsmodel1, conf.int=T)

# Answer the question
GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95)

# Setting the confidence level to 99%
broom::tidy(lsmodel1, conf.int=T, conf.level=0.99)

# --
darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()

# Emmeans --
means <- emmeans::emmeans(lsmodel1, specs = ~ type)

means

means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))

# Assumption checking
# Base R --
performance::check_model(lsmodel1)
# tidyverse -- 
plot(lsmodel1)

# --
performance::check_model(lsmodel1, check=c("normality","qq"))
plot(lsmodel1, which=c(2,2))

# --
performance::check_model(lsmodel1, check="homogeneity")

# --
plot(lsmodel1, which=c(1,3))

# --
performance::check_model(lsmodel1, check="outliers")

# --
plot(lsmodel1, which=c(4,4))

# Summary --
darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)



