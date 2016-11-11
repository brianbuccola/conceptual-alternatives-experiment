library(IBEX.to.R)
library(ggplot2)
library(plyr)
library(sciplot)
library(reshape2)
library(lme4)
library(parallel)
library(loo)
library(rstan)


# Set the right path:
FOLDER <- "https://raw.githubusercontent.com/brianbuccola/conceptual-alternatives-experiment/master/results/"


##############################################################################
#
# DEMOGRAPHIC SURVEY
#
##############################################################################


d_demographics <- get.results(paste0(FOLDER, "results"), controller=c("Form"))

# Language
table(tolower(subset(d_demographics, Col8=="language")$Col9))
nonnativespeakers <- as.character(subset(d_demographics, Col8=="language" & !(tolower(Col9) %in% c("english")))$subject)


##############################################################################
#
# LOAD AND ORGANIZE THE DATA
#
##############################################################################

d_response <- get.results(paste0(FOLDER, "results"), controller=c("Boxes"), del.col=c())

# Remove nonnativespeakers
d_response <- subset(d_response, !(subject %in% nonnativespeakers))

# Remove super-fast participants
with(d_response, plot(Col11, col=subject, 
                      log="y", cex=.5,
                      xlab="Items in collection order, color=participant",
                      ylab="RT (ms, log scale)")) # Quick plot of the reponse times
superfast <- subset(aggregate(Col11~subject, median, data=d_response), Col11<500)$subject
d_response <- subset(d_response, !(subject %in% superfast))

# Code the responses
d_response$response <- d_response$Col9=="Satisfies the rule"
head(d_response)



d_response$pair <- paste0(d_response$Quantifier.1,"-",d_response$Quantifier.2)

d_response$pair[d_response$pair=="AtLeast3-Exactly3"] <- "AtLeast"
d_response$pair[d_response$pair=="AtLeast4-Exactly4"] <- "AtLeast"
d_response$pair[d_response$pair=="AtMost3-Exactly3"] <- "AtMost"
d_response$pair[d_response$pair=="AtMost4-Exactly4"] <- "AtMost"



##############################################################################
#
# Plot of the results
#
##############################################################################

 
### Set theme

my.colours <- c("#E69F00","#56B4E9","#009E73")

theme_mk <- theme_grey() +
  theme(
    plot.margin=unit(x=c(1,1,1,1),units="mm"),
    axis.title.y=element_text(angle=90,vjust=.3,size=8),
    axis.title.x=element_blank(),
    axis.text.x=element_text(angle=90, vjust=0.5, hjust=1,size=7), 
    axis.text.y=element_text(size=7),
    strip.background =  element_rect(fill = "white", colour = "white"),
    strip.text.x = element_text(size = 8, vjust=0.5, colour="grey30"),
    panel.background =  element_rect(fill = "white", colour = "black"),
    panel.grid.major.y = element_line(colour = 'gray', linetype = 'solid', size=.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=8, vjust=0.5,colour="grey30"),
    legend.text=element_text(size=8),
    legend.title = element_text(size=8),
    complete=FALSE
  )

theme_set(theme_mk)


### Quick overall plot of the results

d_plot <- ddply(d_response, c("subject", "Condition", "pair"), 
                function(df)c(response=mean(df$response)))
d_plot <- ddply(d_plot, c("Condition", "pair"),
                function(df)c(response=mean(df$response),se=se(df$response)))

d_plot$Condition <- factor(d_plot$Condition, levels=c("false","target","true"), labels=c("NO","TARGET","YES"))

d_plot$Pivot <- "NUM"
d_plot$Pivot[d_plot$pair=="Some-All"|d_plot$pair=="Some-SBNA"] <- "SOME"
d_plot$Pivot[d_plot$pair=="NotAll-No"|d_plot$pair=="NotAll-SBNA"] <- "NOT.ALL"
d_plot$pair <- factor(d_plot$pair,levels=c("AtLeast","AtMost","NotAll-No","NotAll-SBNA","Some-All","Some-SBNA"),labels=c("(at least, exactly)","(at most, exactly)","(not all, no)","(not all, SBNA)","(some, all)","(some, SBNA)"))

p <- ggplot(d_plot, aes(x=Condition, y=response, fill= Condition)) + 
  geom_bar(stat="identity", colour="black") +
  facet_wrap(Pivot ~ pair) +
  scale_y_continuous(breaks=c(0,.25,.50,.75,1), minor_breaks=c(), limits=c(0,1)) +
  scale_fill_manual(values=my.colours) +
  geom_errorbar(aes(ymin=response-se, ymax=response+se), width=.05) +
  #theme(axis.text.x=element_text(angle=90)) +
  ylab("Roughly: Preference for weaker element")

print(p)

ggsave(filename=paste0(FOLDER,"Plot_ALL.eps"), plot=p.all, width=6, height=4)


### Plots by paradigm



d_some <- subset(d_plot,pair=="(some, all)"|pair=="(some, SBNA)")
d_notall <- subset(d_plot,pair=="(not all, no)"|pair=="(not all, SBNA)")
d_num <- subset(d_plot,pair=="(at least, exactly)"|pair=="(at most, exactly)")

p.some <- ggplot(d_some, aes(x=Condition, y=response, fill= Condition)) + 
  geom_bar(stat="identity", colour="black") +
  facet_grid(. ~ pair) +
  scale_y_continuous(breaks=c(0,.25,.50,.75,1), minor_breaks=c(), limits=c(0,1)) +
  scale_fill_manual(values=my.colours) +
  geom_errorbar(aes(ymin=response-se, ymax=response+se), width=.05) +
  #theme(axis.text.x=element_text(angle=90)) +
  ylab("Rate of acceptance") + guides(fill=FALSE)

print(p.some)

ggsave(filename="Plot_SOME.pdf", plot=p.some, width=5.6, height=5,units="cm")

p.notall <- ggplot(d_notall, aes(x=Condition, y=response, fill= Condition)) + 
  geom_bar(stat="identity", colour="black") +
  facet_grid(. ~ pair) +
  scale_y_continuous(breaks=c(0,.25,.50,.75,1), minor_breaks=c(), limits=c(0,1)) +
  scale_fill_manual(values=my.colours) +
  geom_errorbar(aes(ymin=response-se, ymax=response+se), width=.05) +
  #theme(axis.text.x=element_text(angle=90)) +
  ylab("Rate of acceptance")+
  guides(fill=FALSE)

print(p.notall)

ggsave(filename="Plot_NOTALL.pdf", plot=p.notall, width=5.6, height=5,units="cm")

p.num <- ggplot(d_num, aes(x=Condition, y=response, fill= Condition)) + 
  geom_bar(stat="identity", colour="black") +
  facet_grid(. ~ pair) +
  scale_y_continuous(breaks=c(0,.25,.50,.75,1), minor_breaks=c(), limits=c(0,1)) +
  scale_fill_manual(values=my.colours) +
  geom_errorbar(aes(ymin=response-se, ymax=response+se), width=.05) +
  #theme(axis.text.x=element_text(angle=90)) +
  ylab("Rate of acceptance")+
  guides(fill=FALSE)

print(p.num)

ggsave(filename="Plot_NUM.pdf", plot=p.num, width=5.8, height=5,units="cm")

####################################################################
#
# MCMC MODELS
#
####################################################################

# Constructing the data sets
####################################################################

data <- d_response[c("subject","response","pair","Condition","item.number","Property")]

for (PAIR in names(table(data$pair))) {
  data[paste0(PAIR,"-","nottrue")] <- as.integer(data$pair==PAIR & data$Condition!="true")
  data[paste0(PAIR,"-","true")] <- as.integer(data$pair==PAIR & data$Condition=="true")
  data[paste0(PAIR,"-","target")] <- as.integer(data$pair==PAIR & data$Condition=="target")	
}

data.some <- subset(data,(pair=="Some-All"|pair=="Some-SBNA"))
data.notall <- subset(data,(pair=="NotAll-No"|pair=="NotAll-SBNA"))
data.num <- subset(data,(pair=="AtLeast"|pair=="AtMost"))

# Functions for the models
####################################################################

preparedata <- function(data) {	
  # prepare the data into a list for stan models
  jags.data <- list(
    'N' 		= nrow(data), 							# number of data points
    'response'	= data$response,						# responses
    'K' 		= length(unique(data$subject)), 		# number of subjects
    'subj' 		= as.integer(factor(data$subject)), 	# subjects
    'X' 		= subset(data, select=
                     do.call(mapply, 
                             c(paste, 
                               expand.grid(names(table(data$pair)), 
                                           c("nottrue", "true", "target")), 
                               sep="-"
                             ) ) )				   	# predictor matrix
  )
  return(jags.data)
}





writestanmodel <- function(n_gamma=1, r_subject=FALSE, n_slopes=0) {
  # write the code for a stan model depending on the number of parameters
  
  set_subject_slopes <- "
    for (k in 1:K) {
      s[k] ~ normal(0,sigma_s);
    }
    "
  
  if (n_slopes==0) {
    # no varying slopes
    alpha_beta <- "alpha[1]*X[n,1] + alpha[2]*X[n,2] + beta[1]*X[n,3] + beta[2]*X[n,4]"
    if (n_gamma==2) { 
      fixed_gamma <- " + gamma[1]*(beta[1]-alpha[1])*X[n,5] + gamma[2]*(beta[2]-alpha[2])*X[n,6]" 
      par_gamma <- "
      vector<lower=0,upper=1>[2] gamma;
      "
      }
    else { 
      fixed_gamma <- " + gamma*(beta[1]-alpha[1])*X[n,5] + gamma*(beta[2]-alpha[2])*X[n,6]"	 
      par_gamma <- "
      real<lower=0,upper=1> gamma;
      "
      }
    set_subject_slopes <- ""
    par_s <- ""
  }
  if (n_slopes==3) {
    # one slope for alpha/beta/gamma each
    alpha_beta <- "(alpha[1]+s[subj[n],1])*X[n,1] + (alpha[2]+s[subj[n],1])*X[n,2] + (beta[1]+s[subj[n],2])*X[n,3] + (beta[2]+s[subj[n],2])*X[n,4]"
    if (n_gamma==2) { 
      fixed_gamma <- " + (gamma[1]+s[subj[n],3])*(beta[1]+s[subj[n],2]-alpha[1]-s[subj[n],1])*X[n,5] + (gamma[2]+s[subj[n],3])*(beta[2]+s[subj[n],2]-alpha[2]-s[subj[n],1])*X[n,6]" 
      par_gamma <- "
      vector<lower=0,upper=1>[2] gamma;
      "
      } else { 
      fixed_gamma <- " + (gamma+s[subj[n],3])*(beta[1]+s[subj[n],2]-alpha[1]-s[subj[n],1])*X[n,5] + (gamma+s[subj[n],3])*(beta[2]+s[subj[n],2]-alpha[2]-s[subj[n],1])*X[n,6]"	 
      par_gamma <- "
      real<lower=0,upper=1> gamma;
      "
      }
    par_s <- "
    vector[3] s[K];
    real<lower=0> sigma_s;
    "
  }
  if (n_slopes==5) {
    # not keeping random effects for the one-gamma model
    alpha_beta <- "(alpha[1]+s[subj[n],1])*X[n,1] + (alpha[2]+s[subj[n],2])*X[n,2] + (beta[1]+s[subj[n],3])*X[n,3] + (beta[2]+s[subj[n],4])*X[n,4]"
    if (n_gamma==2) { 
      fixed_gamma <- " + (gamma[1]+s[subj[n],5])*(beta[1]+s[subj[n],3]-alpha[1]-s[subj[n],1])*X[n,5] + (gamma[2]+s[subj[n],6])*(beta[2]+s[subj[n],4]-alpha[2]-s[subj[n],2])*X[n,6]" 
      par_gamma <- "
      vector<lower=0,upper=1>[2] gamma;
      "
      par_s <- "
    vector[6] s[K];
      real<lower=0> sigma_s;
      "
    } else { 
      fixed_gamma <- " + (gamma+s[subj[n],5])*(beta[1]+s[subj[n],3]-alpha[1]-s[subj[n],1])*X[n,5] + (gamma+s[subj[n],5])*(beta[2]+s[subj[n],4]-alpha[2]-s[subj[n],2])*X[n,6]"	 
      par_gamma <- "
      real<lower=0,upper=1> gamma;
      "
par_s <- "
    vector[5] s[K];
    real<lower=0> sigma_s;
    "
    }
  }  
  if (n_slopes==6) {
    # keeping random effects for the one-gamma model
    alpha_beta <- "(alpha[1]+s[subj[n],1])*X[n,1] + (alpha[2]+s[subj[n],2])*X[n,2] + (beta[1]+s[subj[n],3])*X[n,3] + (beta[2]+s[subj[n],4])*X[n,4]"
    if (n_gamma==2) { 
      fixed_gamma <- " + (gamma[1]+s[subj[n],5])*(beta[1]+s[subj[n],3]-alpha[1]-s[subj[n],1])*X[n,5] + (gamma[2]+s[subj[n],6])*(beta[2]+s[subj[n],4]-alpha[2]-s[subj[n],2])*X[n,6]" 
      par_gamma <- "
      vector<lower=0,upper=1>[2] gamma;
      "
    } else { 
      fixed_gamma <- " + (gamma+s[subj[n],5])*(beta[1]+s[subj[n],3]-alpha[1]-s[subj[n],1])*X[n,5] + (gamma+s[subj[n],6])*(beta[2]+s[subj[n],4]-alpha[2]-s[subj[n],2])*X[n,6]"	 
      par_gamma <- "
      real<lower=0,upper=1> gamma;
      "
    }
    par_s <- "
    vector[6] s[K];
    real<lower=0> sigma_s;
    "
  }
  
  if (r_subject) { 
    randomfactor_subject <- " + w[subj[n]]" 
    set_randomfactor_subject <- "
    w ~ normal(0,sigma_w); # subject intercepts
    "
    par_w <- "
    vector[K] w;
    real<lower=0> sigma_w;
    "
  } else { 
    randomfactor_subject <- ""
    set_randomfactor_subject <- ""
    par_w <- ""
  }
  
  
  textData <- "data {
    int<lower=1> N; // number of data points
    int<lower=1> K; // number of subjects
    int<lower=1,upper=K> subj[N];
    int<lower=0,upper=1> response[N];
    int<lower=0,upper=1> X[N,6];
  }
"
  
  textParams <- paste0("parameters {
  vector[2] alpha;
  vector[2] beta;",par_gamma,par_u,par_w,par_s
                        ,"}")
  
  textFormula <- paste0(alpha_beta,fixed_gamma,randomfactor_subject)
  
  textModel <- paste0("
  model {
  ",set_randomfactor_subject,set_subject_slopes,"
    for (n in 1:N) {
      response[n] ~ bernoulli_logit(",textFormula,");
    }
  }
  ")
    
  textGenerated <- paste0("
  generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(response[n]|",textFormula,");
  }
  }")
    
  
  stanText = paste0(textData,textParams,textModel,textGenerated)

  
  return(stanText)
  
}



stanmodel <- function(data, n_gamma=1, r_subject=FALSE, n_slopes=0) {
  
  # wrapper for fitting a stan model and drawing samples
  
  # put the data in a list to pass to stan
  stan.data <- preparedata(data)
  
  # set inits for the Markov chains
  if (n_gamma==2) { stan.inits = function() {list("alpha" = c(0, 1),"beta" = c(1, 0),"gamma"=c(0.5,0.5))} }
  else { stan.inits = function() {list("alpha" = c(0, 1),"beta" = c(1, 0),"gamma"=c(0.5))} }
  
  # specify model code
  modelText = writestanmodel(n_gamma=n_gamma,r_subject=r_subject,n_slopes=n_slopes)
  
  # initiate the model
  stanfit <- stan(model_code=modelText,data=stan.data,init=stan.inits,iter=10000,chains=4)
  return(stanfit)
}

# Doing LOO-CV
####################################################################

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# make a list of the data for the three paradigms
data.list <- list("some"=data.some,"notall"=data.notall,"num"=data.num)


# array to store the outcomes of the LOO-CV, depending on the paramters
# these outcomes are lists themselves, so we have an array of lists
m <- array(list(),dim=c(3,2,4,2),dimnames=list(c("some","notall","num"),c("1","2"),c("0","3","5","6"),c("TRUE","FALSE")))
# pivot, n_gamma, n_slopes, r_subject

loo.cv <- function(DATA,N_GAMMA,N_SLOPES,R_SUBJECT) {
  stan.model <- stanmodel(data.list[[DATA]], n_gamma=N_GAMMA, r_subject=R_SUBJECT, n_slopes=N_SLOPES)
  log_liks <- extract_log_lik(stan.model)
  loo1 <- loo(log_liks)
  return(loo1)
}


### Compute actual models

### WARNING: THIS TAKES A LONG TIME !!!!!!

for (DATA in c("some","notall","num")) {
  loo1 <- loo.cv(DATA,1,0,TRUE)
  m[[DATA,"1","0","TRUE"]] <- loo1
}

save(m,file="loo_array.rda")


for (DATA in c("some","notall","num")) {
  loo1 <- loo.cv(DATA,1,0,FALSE)
  m[[DATA,"1","0","FALSE"]] <- loo1
}

save(m,file="loo_array.rda")


for (DATA in c("some","notall","num")) {
  loo1 <- loo.cv(DATA,2,0,FALSE)
  m[[DATA,"2","0","FALSE"]] <- loo1
}

save(m,file="loo_array.rda")

for (DATA in c("some","notall","num")) {
  loo1 <- loo.cv(DATA,1,3,FALSE)
  m[[DATA,"1","3","FALSE"]] <- loo1
}

save(m,file="loo_array.rda")

for (DATA in c("some","notall","num")) {
  loo1 <- loo.cv(DATA,1,6,FALSE)
  m[[DATA,"1","6","FALSE"]] <- loo1
}

save(m,file="loo_array.rda")

for (DATA in c("some","notall","num")) {
  loo1 <- loo.cv(DATA,2,3,FALSE)
  m[[DATA,"2","3","FALSE"]] <- loo1
}

save(m,file="loo_array.rda")

for (DATA in c("some","notall","num")) {
  loo1 <- loo.cv(DATA,2,6,FALSE)
  m[[DATA,"2","6","FALSE"]] <- loo1
}

save(m,file="loo_array.rda")



for (DATA in c("some","notall","num")) {
  loo1 <- loo.cv(DATA,1,3,TRUE)
  m[[DATA,"1","3","TRUE"]] <- loo1
}

save(m,file="loo_array.rda")



for (DATA in c("some","notall","num")) {
  loo1 <- loo.cv(DATA,1,6,TRUE)
  m[[DATA,"1","6","TRUE"]] <- loo1
}

save(m,file="loo_array.rda")

for (DATA in c("some","notall","num")) {
  loo1 <- loo.cv(DATA,2,3,TRUE)
  m[[DATA,"2","3","TRUE"]] <- loo1
}

save(m,file="loo_array.rda")



for (DATA in c("some","notall","num")) {
  loo1 <- loo.cv(DATA,2,6,TRUE)
  m[[DATA,"2","6","TRUE"]] <- loo1
}


save(m,file="loo_array.rda")


for (DATA in c("some","notall","num")) {
  loo1 <- loo.cv(DATA,2,0,TRUE)
  m[[DATA,"2","0","TRUE"]] <- loo1
}

save(m,file="loo_array.rda")

for (DATA in c("some","notall","num")) {
  loo1 <- loo.cv(DATA,2,5,TRUE)
  m[[DATA,"2","5","TRUE"]] <- loo1
}

save(m,file="loo_array.rda")

for (DATA in c("some","notall","num")) {
  loo1 <- loo.cv(DATA,1,5,TRUE)
  m[[DATA,"1","5","TRUE"]] <- loo1
}

save(m,file="loo_array.rda")




# extract the elpds from the loo-cv into an array
elpds <- apply(m,c(1,2,3,4),function(l){l[[1]][[1]]})

# look at models with subject intercepts
some.elpds <- elpds["some",,,"TRUE"]
num.elpds <- elpds["num",,,"TRUE"]
notall.elpds <- elpds["notall",,,"TRUE"]

# look at models without subject intercepts
some.elpds2 <- elpds["some",,,"FALSE"]
num.elpds2 <- elpds["num",,,"FALSE",]
notall.elpds2 <- elpds["notall",,,"FALSE"]

# compare two-gamma and one-gamma models
compare(m[["some","2","3","TRUE"]],m[["some","1","3","TRUE"]])
compare(m[["notall","2","3","TRUE"]],m[["notall","1","3","TRUE"]])
compare(m[["num","2","3","TRUE"]],m[["num","1","3","TRUE"]])





