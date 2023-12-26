# Overview

This repository contains the results for the experiments of the paper "Enhanced Component-Wise Natural Gradient Descent Training Method for Deep Neural Networks".

Authors: Sang Van Tran, Toshiyuki Nakata, Rie Shigetomi Yamaguchi, Mhd Irvan, and Yoshihide Yoshimoto.

The source code of the experiments is available at https://github.com/tranvansang/cw-ngd

The experiment results are available at https://github.com/tranvansang/cw-ngd-results

# Results

Extract the raw data with:
```bash
(
cd logs
tar xvf exp1.tar.xz
tar xvf exp2.tar.xz
tar xvf exp3.tar.xz
)
```

## Experiment 1
```bash
Rscript 1-cross-hyperparameters.R
```

Graph is saved at [1-cross-hyperparameters.png](./1-cross-hyperparameters.png).

![1-cross-hyperparameters.png](1-cross-hyperparameters.png)

## Experiment 2
```bash
Rscript 2-strategies.R
```

Graphs are saved at [2-strategies.png](./2-strategies.png) and [2-strategies-zoom.png](./2-strategies-zoom.png).

![2-strategies.png](./2-strategies.png)

![2-strategies-zoom.png](./2-strategies-zoom.png)

## Experiment 3
```bash
Rscript 3-all-algorithms.R | tee 3-all-algorithms.txt
```

Output is saved at [3-all-algorithms.txt](./3-all-algorithms.txt).

```txt
Skip logs/stat-exp3-stl10-kfac-0-test.csv
Skip logs/stat-exp3-stl10-kfac-0-step.csv
Skip logs/stat-exp3-stl10-kfac-0-train.csv
Skip logs/stat-exp3-stl10-kfac-1-test.csv
Skip logs/stat-exp3-stl10-kfac-1-step.csv
Skip logs/stat-exp3-stl10-kfac-1-train.csv
Skip logs/stat-exp3-stl10-kfac-2-test.csv
Skip logs/stat-exp3-stl10-kfac-2-step.csv
Skip logs/stat-exp3-stl10-kfac-2-train.csv
Skip logs/stat-exp3-stl10-kfac-3-test.csv
Skip logs/stat-exp3-stl10-kfac-3-step.csv
Skip logs/stat-exp3-stl10-kfac-3-train.csv
Skip logs/stat-exp3-stl10-kfac-4-test.csv
Skip logs/stat-exp3-stl10-kfac-4-step.csv
Skip logs/stat-exp3-stl10-kfac-4-train.csv
Dataset	Label	CWNGD	variance	Adam	variance	SGD	variance	KFAC	variance 

Average 
mnist	End train acc	99.8355 %		99.8026 %		99.7368 %		99.8026 %	
mnist	Steps until 90%	18.2 / 5900	'+3.2	28.6 / 5900	'+3.6	35.6 / 5900	'+3	317 / 5900	'+72.6
mnist	End test acc	99.2340 %		99.3060 %		99.3080 %		99.3620 %	
mnist	Steps until 90%	10.6 / 5900	'+0	14 / 5900	'+0	21.8 / 5900	'+0.4	182.8 / 5900	'+0

fmnist	End train acc	94.3750 %		94.5724 %		93.6184 %		97.5329 %	
fmnist	Steps until 90%	103.8 / 5900	'+336.2	133.4 / 5900	'+347	102 / 5900	'+372.2	658.8 / 5900	'+237.2
fmnist	End test acc	89.1760 %		89.3100 %		89.1800 %		87.9220 %	
fmnist	Steps until 90%	42.4 / 5900	'+0.8	71.8 / 5900	'+2.6	56.4 / 5900	'+5.6	413.4 / 5900	'+0

cifar10	End train acc	96.8396 %		97.9245 %		95.4717 %		98.3726 %	
cifar10	Steps until 90%	1069.8 / 4900	'+621.6	1040.4 / 4900	'+573.2	1555.2 / 4900	'+754	1525.8 / 4900	'+499
cifar10	End test acc	84.9860 %		85.3620 %		83.2700 %		88.1980 %	
cifar10	Steps until 90%	648.4 / 4900	'+48.4	652.2 / 4900	'+2234	1148.2 / 4900	'+1671	1330.6 / 4900	'+126

stl10	End train acc	98.6504 %		98.4513 %		95.6859 %		-	
stl10	Steps until 90%	158.4 / 500	'+21.6	171 / 500	'+30.2	330.4 / 500	'+40	-	-
stl10	End test acc	71.6825 %		67.5475 %		65.2825 %		-	
stl10	Steps until 90%	132.4 / 500	'+172.4	20.4 / 500	'+47.2	51.8 / 500	'+33.2	-	-

```
