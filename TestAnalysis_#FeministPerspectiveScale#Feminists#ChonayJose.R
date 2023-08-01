##########################################################
# Test/questionnaire evaluation and adaptation           #
# Test/questionnaire: Feminist Perspective Scale        #
# Author name:       José Chonay                       #
# Date of last update: 30.10.2022                        #                     #
##########################################################

##########################################################
# List of Packages
library(car)
library(Gifi)
library(psych)
library(carData)
library(psych)
library(MatchIt)
library(Rcpp)
library(stats4)
library(lordif)
library(MPsychoR)
library(rms)
library(Hmisc)
library(survival)
library(Formula)
library(ggplot2)
library(eRm)
library(Gifi)

##########################################################
# 1. Read in the data and set working directory
##########################################################
path <- "/Users/josechonay/Library/CloudStorage/OneDrive-CarlvonOssietzkyUniversitätOldenburg/Summer Semester 22/Test Construction Applied/FPS"
setwd(path)

file <- '/Users/josechonay/Library/CloudStorage/OneDrive-CarlvonOssietzkyUniversitätOldenburg/Summer Semester 22/Test Construction Applied/FPS/data.csv'
dat <- read.csv(file,sep = '\t')

# check data dimensions and summary
head(dat)
dim(dat)

#################################################
# 2. Cleaning data
#################################################

# Delete columns that don't work for our analysis.
# should there be irrelevant columns, select only the relevant ones

myvars <- names(dat) %in% c("source","country")
dat_c <- dat[!myvars]
head(dat_c)

# In this data set there is no need to correct item responses as all items follow the same values ranging from 1 Disagree, 3 Neutral and 5 Agree.
# 0 response is when the question was not answered.

# Check covariates for plausible values
hist(dat$age)
range(dat$age)

# Remove all the participants who are older than 60 
dat_c <- dat[ which(dat$age < 60),]
hist(dat_c$age)
range(dat_c$age)

# Remove gender responses that are not plausible, like 0 or bigger than 3.
dat_c <- dat[which(dat$gender > 0),]
dat_c <- dat[which(dat$gender < 4),]

hist(dat_c$gender)
range(dat_c$gender)

# Create an id variable to apply to the whole data set.
id <- c(1:13477)
dat_c <- cbind(dat_c,id)

############################################################################
# 3. Plot item response distributions and check data quality and structure
############################################################################

varnames <-colnames(dat_c)

v <- length(varnames) # Number of variables
n <- length(dat_c[,1]) # n

# Univariate Distributions

for (i in 1:v ) { 
 png(paste("His",varnames[i],".png"))
 hist(dat_c[,i], breaks=20)
dev.off() }

# There are no implausible values, I just need to delete all the 0 responses as they just represent missing.

#2. Which levels of the covariates are you interested in to analyze? 
# I am interested in analyzing if the responses are loading into the factors of different feminist perspectives.
# Also I am interested if there any other groupings or loading that are not part of the theory of the test.
# And finally to see if there is a second level factor of behaviors and attitudes. 

# Removing all mising values in answers that are 0.
# Transform the 0 values to NAN values.
dat_c[dat_c == 0] <- NA
dat_c <- na.omit(dat_c)

head(dat_c)

# Delete all unnecessary variables.
myvars2 <- names(dat_c) %in% c("source","country","gender","id","age")
dat_sts <- dat_c[!myvars2]

# Delete the answers that could represent a bad response pattern, meaning that they have the same answer for every question.
dat_sts$AllSame = rowMeans(dat_sts[,1:60]) == dat_sts[,1]

dat_sts = dat_sts[ which(dat_sts$AllSame == 0), ]

myvars3 = names(dat_sts) %in% c("AllSame")
dat_sts <- dat_sts[!myvars3]

############################################################################
# 4. Exploratory factor analysis
############################################################################

# EFA on the polychoric correlation matrix
EFA <- polychoric(dat_sts[,1:60])$rho

# Eigenvalue criterion
evals <- eigen(EFA)$values

# Create scree plot and save it in working directory
png("Scree plot.png")
scree(EFA, factors = FALSE)
dev.off()

# Parallel analysis to examine the scree plot
png("Parallel Analysis.png")
PA <- fa.parallel(dat_sts[,1:60], fa = "both", cor = "poly",fm = "ml", main = "Parallel Analysis")
dev.off()

# Parallel analysis suggests that the number of factors =  5 and the number of components =  5 
# This result is different with the literature which suggests 6 factors.

# We can run a very simple structure (VSS) to interpret matrix loadings of varying number of factors.
png("Very Simple Structure.png")
resvss <- vss(EFA, fm = "ml", n.obs = nrow(dat_sts), plot = TRUE)
dev.off()

# The MAP criterion suggest 5 factors

# Factor analysis for 6 factors as suggested by the literature
FA_six <- fa(dat_sts[,1:60], 6, cor = "poly", fm = "ml", rotate = "oblimin")
summary(FA_six)

png("FA Diagram six")
fa_diagram_six <- fa.diagram(FA_six, main = "Factor Analysis 6 Factors")
dev.off()

print(FA_six$loadings, cutoff = 0.3)

# Factor analysis for 5 factors as suggested by the analysis
FA_five <- fa(dat_sts[,1:60], 5, cor = "poly", fm = "ml", rotate = "oblimin")
summary(FA_five)

png("FA Diagram five")
fa_diagram_five <- fa.diagram(FA_five, main = "Factor Analysis 5 Factors")
dev.off()

print(FA_five$loadings, cutoff = 0.3)

# Exploratory Multidimensional Item Factor Analysis

# Calculate mirt for 6 factors
fitifa6 <- mirt(dat_sts[,1:60], 6, verbose = FALSE, TOL = 0.001, method ="QMCEM")

# Show factor loadings and correlations for 6 factors 
summary(fitifa6)

# Compute M2 statistics for model fit
M2(fitifa6, QMC = TRUE)

# Results in short:
# RMSEA:0.0282803 , TLI:0.9860549 , CFI:0.9890807 , p = 0

# Calculate mirt for 5 factors
fitifa5 <- mirt(dat_sts[,1:60], 5, verbose = FALSE, TOL = 0.001, method = "QMCEM")

# Show factor loadings and correlations for 5 factors 
summary(fitifa5)

# Compute M2 statistics for model fit
M2(fitifa5, QMC = TRUE)

# Results in short:
# RMSEA:0.03199807 , TLI:0.9821474 , CFI:0.9854036 , p = 0

# Item Fit

# Explore item fit 
misfit2d <- mirt::itemfit(fitifa6)
misfit2d[misfit2d[,4] < .08,]
# RMSEA values: p-values: . 

misfit2d_5 <- mirt::itemfit(fitifa5)
misfit2d_5[misfit2d_5[,4] < .05,]

# Evaluating the fit of both models
anova(fitifa6, fitifa5, verbose = FALSE)

############################################################################
# 5. Confirmatory factor analysis
############################################################################

# Assign items to the factors 
modFPS <- mirt.model('
            cons = 1,4,13,17,23,36,38,47,53,59
            
            lib = 5,6,7,22,24,27,33,42,52,60
            
            soc = 10,20,25,31,39,41,45,54,56,58
            
            rad = 2,15,16,18,19,29,34,46,48,55
            
            cul = 9,11,14,28,30,32,35,37,44,50
            
            woc = 3,8,12,21,26,40,43,49,51,57
            
            COV = cons*lib*soc*rad*cul*woc
                      ')
## Confirmatory Multidimensional IRT

CFA <- mirt(dat_sts, model = modFPS, itemtype = 'graded', method = 'MHRM', SE.type = 'MHRM', verbose = TRUE)

# Show correlation matrix
CFA_sum1 <- summary(CFA, verbose = FALSE)                             
round(CFA_sum1$fcor, 3)
CFA_sum1

M2(CFA, QMC = TRUE)

# Results in short:
# p = 0, RMSEA = 0.0846275, TLI = 0.8751248, CFI = 0.8810151
# Indicators provide that this is a fair/good fit however, p is significant.
