# bayesian_analysis_R
university project

Introduction 

Parkinson’s disease is a neurodegenerative disorder characterized by a loss of dopaminergic cells in the substantia nigra pars compacta, which leads to a decline in motor functioning. Patients with Parkinson's disease also experience difficulties in learning novel motor sequences, which is attributed to abnormal activity in the basal ganglia circuits responsible for sequence learning. The aim of this study is to investigate whether there are group differences in performance tempo during sequence learning in healthy young, healthy old, and Parkinson’s disease patients using Bayes factor analysis.

Method

Participants: The study included three groups of participants: healthy older adults (group 1), healthy young adults (group 2), and Parkinson's disease patients (group 3). There were 36 healthy older adults, 36 healthy young adults and 20 Parkinson’s disease patients.
Materials: Participants performed a sequence learning task, which involved learning and performing a novel motor sequence. The dependent variable was the performance tempo during sequence learning.
Experiment: Participants played a short novel piano sequence of 4 notes across 180 trials. The logmIKI (logarithm of the mean inter-keystroke-interval recorded in log-ms) variable represents the trial-wise average performance tempo.
Data analysis: The logmIKI dataset was chosen for a one-way ANOVA with the factor group (1, 2, 3) based on the non-significant result of the Levene's test, indicating that the dataset meets the assumption of homogeneity of variance. Moreover, an examination of the histogram for logmIKI revealed a distributions that are close to normal distributions (fig. 1).


![histogram](https://github.com/OliviaJayeola/bayesian_analysis_R/assets/114793837/69c7b7b8-a0b7-4463-abfe-351047f2b216)

Figure 1. histograms of logmIKI and mIKI, respectively



Results


![timecourselogmiki](https://github.com/OliviaJayeola/bayesian_analysis_R/assets/114793837/af1ea3d7-d239-4326-9108-135ed4c7587b)

Figure 2. time course of logmIKI with error bars


![timecoursemiki](https://github.com/OliviaJayeola/bayesian_analysis_R/assets/114793837/6c1e2929-dc94-447a-a806-57081839967a)

Figure 3. time course of mIKI with error bars


A one-way analysis of variance (ANOVA) was conducted to examine the effect of Parkinson’s disease on a piano based motor learning task. The analysis revealed a significant main effect of group on the motor learning task was revealed, (F(2, 89) = 32.44, p < 0.001. 
A Bonferroni-corrected independent samples t-test was conducted to examine differences in performance tempo during sequence learning between healthy older adults and healthy young adults, healthy older adults and Parkinson’s disease patients, and healthy young adults and Parkinson’s disease patients. The Bonferroni correction was applied to control for 3 comparisons and the significance level was set at p < 0.017 (0.05/3). The t-test revealed a significant difference in performance tempo between healthy older adults and Parkinson’s disease patients, p =0.015, healthy young adults and Parkinson’s disease patients, p < 0.001 and healthy older adults and healthy young adults, p < 0.001. Healthy young adults performed significantly better than healthy older adults and Parkinson’s disease patients. Healthy older adults also performed significantly better than Parkinson’s disease patients 
The effect of Parkinson’s disease on a piano based motor learning task values was also examined using a Bayes factor analysis. The model including the variable 'group' provided strong evidence over the intercept-only model (BF = 296,395,966, ±0%), indicating substantial support for the hypothesis of an effect.

The Bayes factor of 296,395,966 suggests that the inclusion of the 'group' variable significantly improves the model's fit compared to the null hypothesis. The negligible uncertainty (±0%) associated with the Bayes factor indicates precise estimation.
These results provide robust evidence for group differences in the piano based motor learning task.


Discussion


The results of this study suggest that Parkinson's disease has a significant effect on performance during a piano-based motor learning task as both healthy young adults and healthy older adults outperformed Parkinson's disease patients in the motor learning task. The findings of the ANOVA analysis revealed a significant main effect of group, with post-hoc tests indicating significant differences between all pairwise comparisons. Notably, the comparison between healthy older adults and Parkinson's disease patients showed a marginally significant difference, as the p-value was very close to the adjusted significance level compared to the other comparisons which where closer to 0.

