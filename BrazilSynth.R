library(readr)
library(tidyr)
library(dplyr)
library(Synth)

########################################
####### DATA PRE-PREPROCESSING #########
########################################

# Importing data
WDI <- read_csv("C:/Users/Vinic/Research/BrazilSynth/WDI v2.csv",
               na = c("", "NA", ".."))
BL <- read_csv("C:/Users/Vinic/Research/BrazilSynth/BL v1.csv",
               na = c("", "NA", ".."))

# Column names should be the same for rbind
names(BL)[3] <- names(WDI)[3]
df <- rbind(WDI, BL)

# Adjusting year column names
names(df) <- c(names(df)[1:4], 1995:2017)

# Changing country codes to a number
codes <- unique(df$`Country Code`)
for (ii in 1:length(codes)) {
  df$`Country Code`[which(df$`Country Code` == codes[ii])] = ii 
}

# Gather the year, drop NAs, and spread the indicators to columns
df_tidy <- df %>% 
           gather("year", "value", 5:27) %>%
           drop_na() %>%
           select(-c("Series Code")) %>%
           spread("Series Name", "value") 
# View(df_tidy)

# Casting the dataype of columns as numeric
for (ii in 2:ncol(df_tidy)) {
  df_tidy[[ii]] <- as.numeric(df_tidy[[ii]])
}

########################################
####### TRANSFORMING COLUMNS ###########
########################################

# Exports of goods and services (constant 2010 US$) to
# Exports of goods and services (constant 2010 US$, per capita)
df_tidy$`Exports of goods and services (constant 2010 US$, per capita)` <-
  df_tidy$`Exports of goods and services (constant 2010 US$)` /
  df_tidy$`Population, total`

# Exports of goods and services (BoP, current US$) to
# Exports of goods and services (BoP, current US$, per capita)
df_tidy$`Exports of goods and services (BoP, current US$, per capita)` <-
  df_tidy$`Exports of goods and services (BoP, current US$)` /
  df_tidy$`Population, total`

# Imports of goods and services (constant 2010 US$) to
# Imports of goods and services (constant 2010 US$, per capita)
df_tidy$`Imports of goods and services (constant 2010 US$, per capita)` <-
  df_tidy$`Imports of goods and services (constant 2010 US$)` /
  df_tidy$`Population, total`

# Imports of goods and services (BoP, current US$) to
# Imports of goods and services (BoP, current US$, per capita)
df_tidy$`Imports of goods and services (BoP, current US$, per capita)` <-
  df_tidy$`Imports of goods and services (BoP, current US$)` /
  df_tidy$`Population, total`

# Trade (% of GDP) to Trade (constant 2010 US$, per capita)
df_tidy$`Trade (constant 2010 US$, per capita)` <-
  df_tidy$`Trade (% of GDP)` * df_tidy$`GDP (constant 2010 US$)` / df_tidy$`Population, total`

# Dropping auxiliary columns
df_transformed <- df_tidy %>%
  select(-c("Exports of goods and services (constant 2010 US$)",
            "Exports of goods and services (BoP, current US$)",
            "Imports of goods and services (constant 2010 US$)",
            "Imports of goods and services (BoP, current US$)",
            "GDP (constant 2010 US$)",
            "Trade (% of GDP)",
            "Population, total"))

########################################
#### QUICK EXPLORATORY ANALYSIS ########
########################################
df_quick <- df_transformed %>% 
  group_by(`Country Name`) %>%
  summarise_if(.predicate = function(x) is.numeric(x),
               .funs = c(mean = "mean"), na.rm = TRUE)
# View(df_quick)

########################################
####### BRANCHING THE DATA #############
########################################

# Analysis 1 contains the metrics
# - Tax revenue (% of GDP)
# - Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)
# - Revenue, excluding grants (% of GDP)
# and excludes India, Ecuador, and Venezuela
df_analysis1 <- df_transformed %>%
  filter(!(`Country Name` %in% c("Ecuador", "India", "Venezuela, RB")))

# Analysis 2 excludes the metrics and retains the countries mentioned above
df_analysis2 <- df_transformed %>%
  select(-c("Tax revenue (% of GDP)",
            "Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)",
            "Revenue, excluding grants (% of GDP)")) %>%
  filter(`Country Name` != "Venezuela, RB")

# Analysis x.BoP contains Exports and Imports in BoP, current US$
df_analysis1.BoP <- df_analysis1 %>%
  select(-c("Imports of goods and services (constant 2010 US$, per capita)",
            "Exports of goods and services (constant 2010 US$, per capita)"))
df_analysis2.BoP <- df_analysis2 %>%
  select(-c("Imports of goods and services (constant 2010 US$, per capita)",
            "Exports of goods and services (constant 2010 US$, per capita)"))

# whereas x.2 containts Exports and Imports in constant 2010 US$
df_analysis1.2010 <- df_analysis1 %>%
  select(-c("Imports of goods and services (BoP, current US$, per capita)",
            "Exports of goods and services (BoP, current US$, per capita)"))
df_analysis2.2010 <- df_analysis2 %>%
  select(-c("Imports of goods and services (BoP, current US$, per capita)",
            "Exports of goods and services (BoP, current US$, per capita)"))

# dplyr turns the dataframes into tbl. Referring back to data.frame.
df_analysis1.BoP <- as.data.frame(df_analysis1.BoP) 
df_analysis1.2010 <- as.data.frame(df_analysis1.2010) 
df_analysis2.BoP <- as.data.frame(df_analysis2.BoP) 
df_analysis2.2010 <- as.data.frame(df_analysis2.2010) 

# Logging exports
# df_analysis2.2010$`Exports of goods and services (constant 2010 US$, per capita)` <- 
#   log(df_analysis2.2010$`Exports of goods and services (constant 2010 US$, per capita)`)

########################################
####### SYNTHETIC CONTROLS #############
########################################

### Specification Search - STEP 1
### We try each of the four datasets, for each possible
### time window of time.optimize.ssr.

dfs_list <- list(df_analysis1.2010, df_analysis1.BoP, df_analysis2.2010, df_analysis2.BoP)

storage <- list()
for (i in 1:length(dfs_list)) {
  
  storage[[i]] <- list()
  
  for (j in 1:23) {
    
    print(sprintf("Trying data set %d with optimize.ssr starting at %d", i, 1994 + j))
    sink("NUL")
    
    # Data setup
    dataprep.out <-
      dataprep(
        foo = dfs_list[[i]],
        predictors    = (1:ncol(dfs_list[[i]]))[-(1:3)],
        dependent     = 7,
        unit.variable = 2,
        time.variable = 3,
        treatment.identifier = 4,
        controls.identifier = sort(unique(dfs_list[[i]]$`Country Code`))[-4],
        time.predictors.prior = 1995:2017,
        time.optimize.ssr = (1994+j):2017,
        unit.names.variable = 1,
        time.plot = 1995:2017
      )
    
    synth.out <- 
      synth(
        data.prep.obj=dataprep.out,
        Margin.ipop=.01,Sigf.ipop=7,Bound.ipop=6
      )
    
    # Data prep for main model
    dataprep.out <-
      dataprep(
        foo = dfs_list[[i]],
        predictors    = (1:ncol(dfs_list[[i]]))[-(1:3)],
        dependent     = 7,
        unit.variable = 2,
        time.variable = 3,
        treatment.identifier = 4,
        controls.identifier = sort(unique(dfs_list[[i]]$`Country Code`))[-4],
        time.predictors.prior = 1995:2017,
        time.optimize.ssr = 1995:2017,
        unit.names.variable = 1,
        time.plot = 1995:2017
      )
    
    # fit main model with v from training model
    synth.out <- synth(
      data.prep.obj=dataprep.out,
      Margin.ipop=.01,Sigf.ipop=7,Bound.ipop=6, 
      custom.v=as.numeric(synth.out$solution.v)
    )
    
    storage[[i]][[1994+j]] <- synth.out$loss.v
    sink()
    
  }
}


# Data setup
dataprep.out <-
  dataprep(
    foo = df_analysis1.2010,
    predictors    = 4:22,
    dependent     = 7,
    unit.variable = 2,
    time.variable = 3,
    # special.predictors = list(
    #   list(4, 2008:2017, c("mean")), # Agriculture share
    #   list(10, 2008:2017, c("mean")), # Industry share
    #   list(9, 2008:2017, c("mean")) # Gross capital formation
    # ),
    treatment.identifier = 4,
    controls.identifier = sort(unique(df_analysis1.2010$`Country Code`))[-4],
    time.predictors.prior = 1995:2017,
    time.optimize.ssr = 1995:2017,
    unit.names.variable = 1,
    time.plot = 1995:2017
  )

system.time(
synth.out <- 
  synth(
    data.prep.obj=dataprep.out,
    Margin.ipop=.01,Sigf.ipop=7,Bound.ipop=6
  ))

# Data prep for main model
dataprep.out <-
  dataprep(
    foo = df_analysis1.2010,
    predictors    = 4:22,
    dependent     = 7,
    unit.variable = 2,
    time.variable = 3,
    # special.predictors = list(
    #   list(4, 2008:2017, c("mean")), # Agriculture share
    #   list(10, 2008:2017, c("mean")), # Industry share
    #   list(9, 2008:2017, c("mean")) # Gross capital formation
    # ),
    treatment.identifier = 4,
    controls.identifier = sort(unique(df_analysis1.2010$`Country Code`))[-4],
    time.predictors.prior = 2007:2017,
    time.optimize.ssr = 1995:2017,
    unit.names.variable = 1,
    time.plot = 1995:2017
  )

# fit main model with v from training model
synth.out <- synth(
  data.prep.obj=dataprep.out,
  custom.v=as.numeric(synth.out$solution.v)
)

#### Table 2
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
); synth.tables

#### Figure 1: Trends in Per-Capita GDP
Text.height <- 8000
Cex.set <- .8
synthY0 <- (dataprep.out$Y0%*%synth.out$solution.w)
plot(1995:2017,dataprep.out$Y1plot,
     type="l",ylim=c(0,20000),col="black",lty="solid",
     ylab ="per-capita GDP (PPP, 2010 USD)",
     xlab ="year",
     xaxs = "i", yaxs = "i",
     lwd=2
)
lines(1995:2017,synthY0,col="black",lty="dashed",lwd=2)
abline(v=2016.8,lty="dotted")
legend(x="bottomright",
       legend=c("Brazil","Synthetic Brazil")
       ,lty=c("solid","dashed"),col=c("black","black")
       ,cex=.8,bg="white",lwd=c(2,2))
arrows(2014,Text.height,2016,Text.height,col="black",length=.1)
text(2013,Text.height,"Election",cex=Cex.set)

### Figure 3: Per-Capita GDP Gap 
gap <- dataprep.out$Y1-(dataprep.out$Y0%*%synth.out$solution.w)
plot(1995:2017,gap,
     type="l",ylim=c(-8000,8000),col="black",lty="solid",
     ylab =c("gap in per-capita GDP (PPP, 2010 USD)"),
     xlab ="year",
     xaxs = "i", yaxs = "i",
     lwd=2
)
abline(v=2016.8,lty="dotted")
abline(h=0,lty="dotted")
arrows(2014,1000,2016,1000,col="black",length=.1)
text(2013,1000,"Election",cex=Cex.set)


