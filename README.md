# iMLGAM: integrated Machine Learning and Genetic Algorithm-driven Multiomics analysis for pan-Cancer immunotherapy response prediction

## Overview

This repository contains data and code related to the study of predictive models for pan-cancer immunotherapy. The purpose of this study is to construct a signature based on RNA sequencing data to predict the outcomes of immunotherapy in patients with pan-cancer.

## Directory Structure

- `Figure1`, `Figure2`, ...: Data and original code for saving generated figures.
- The source code of the iMLAGA package.

## Prerequisites

Make sure you have R and the following packages installed: iMLAGA. Install "iMLAGA" via this [GitHub page](https://github.com/Yelab1994/iMLAGA) or by running the code:

```R
devtools::install_github("Yelab1994/iMLGAM")
```
## Step 1: Constructing Base Learners
Download sample files via this [Baidu Netdisk page](https://pan.baidu.com/s/1DmL1MBvDnn4JT798wFRoSQ?pwd=h527) and load the necessary R packages.

```R
#load the necessary R packages
library(qs)
library(abess)
library(iMLGAM)
#Read the example file
dat <- qread("ICI cohort gene pairs.qs")
#the first column is the outcome (with row names as 'res'), and the subsequent columns are features.
for (i in names(dat)) {
  dat[[i]] <- dat[[i]][,-c(1,3,4)]
  colnames( dat[[i]])[1] <- "res"
}
#Determine the training set
trainset <- dat$trainset_7
#Determine the validation set
validationset <- dat$trainset_3
#Determine the test set 
testset <- dat$testset 
```
