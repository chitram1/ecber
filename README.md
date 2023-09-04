ecber: Expansion of ccber's Behavioral Entropy Rate Estimation and Reliability Assessment
============================================================================================================
ecber is an R package developed to estimate behavioral entropy rate and assess coding reliability between two coders. **Building upon the foundation of the original ccber package**, our expanded version introduces new features to improve the accuracy and flexibility of behavioral entropy rate estimation. **ecber** was created by Dr. Elisa Ugarte and Chitra Mukherjee at the University of California, Davis. 


Acknowledgements
------------------

**ecber** builds upon the foundation laid by the **ccber** package developed for the Conte Center at the University of California Irvine. Our package extends the capabilities of **ccber** by integrating new functions and features. The main Markov matrix calculations, among other key functions, remain unchanged from **ccber**. For more details on these functions and their functionalities, please refer to the original **ccber** repository: ccber on [GitHub](https://pages.github.com/). 

Please see the below references:

- Vegetabile, B. G., Stout-Oswald, S. A., Davis, E. P., Baram, T. Z., & Stern, H. S. (2019). Estimating the Entropy Rate of Finite Markov Chains With Application to Behavior Studies. Journal of Educational and Behavioral Statistics, 44(3), 282–308. https://doi.org/10/gjqwr3

- Davis, E. P., Stout, S. A., Molet, J., Vegetabile, B., Glynn, L. M., Sandman, C. A., Heins, K., Stern, H., & Baram, T. Z. (2017). Exposure to unpredictable maternal sensory signals influences cognitive development across species. Proceedings of the National Academy of Sciences, 114(39), 10390–10395. https://doi.org/10/gb44vr


What is ecber?
------------------
The primary goal of **ecber** is to estimate the behavioral entropy rate by analyzing event-coded data files or directories of files. This estimation provides insights into the complexity and unpredictability of behavioral sequences. Notably, **ecber** expands upon the capabilities of the original ccber package by introducing additional event codes and functions that allow the calculation of reliability (similarity) between two coders' annotations, among other features.


Key Features and Enhancements
------------------
- **Affect and Behavior Integration**: **ecber** incorporates affect and autonomy-granting behaviors in addition to **ccber’s** sensory signals. This addition enables a deeper analysis of the interplay between affective states, behavior, and sensory signals. The package currently calculates behavioral entropy rate of (1) sensory signals, (2) sensory signals and affect, and (3) affect and autonomy-granting behaviors. See the codebook for a description of how to set up files and record observations: [codebook](https://github.com/chitram1/ecber/blob/main/_U%20Denver%20Sensory%20Coding%20and%20A%26B%20Manual-%20Boris.pdf).
- **Adjustable Markov Matrix Lag**: Users can manually change the Markov matrix lag (Markov model order) in the entropy rate calculation. This adaptation allows for considering different numbers of previous states when predicting the new state.
- **Co-occurrence states**: We've introduced additional pairwise counts to quantify co-ocurrence for all combinations of behavioral types.
- **Plotting Enhancements**: **ecber** includes new plotting functions that visualize transition count matrices and transitions between states and can be saved into PDFs. These visualizations aid in comprehending the dynamics of behavioral sequences. 
- **Reliability function**: **ecber** includes reliability functions designed to measure the level of agreement in duration between different coders. These functions can adapt to custom-defined codes.
- **Kappa Values**: provides functions for calculating kappa values, which offer a numerical assessment of agreement between coders based on user-defined codes.
- **Expanded Tolerance Values**: **ecber** allows for increased tolerance values, including adjustable buffers (e.g., 1, 2, 3 seconds) to calculate reliability between coders. 
- **BORIS File Format Conversion**: Conversion functions are included to transform individual files and directories from the [BORIS](https://www.boris.unito.it/) format to Excel files that can be read by **ecber**, streamlining data preprocessing.


Installation Instructions
------------------

The installation instructions for this package are similar to those of ccber. You will need `devtools` in order to install this package from the Github Repo.

```{r, echo=TRUE, eval=FALSE}
install.packages('devtools', dependencies = TRUE)
```

```{r, echo=TRUE, eval=FALSE}
devtools::install_github('chitram/ecber')
```
Now that the package has been installed, we need to load it in with the library() function.

```{r, echo=TRUE, eval=TRUE}
library('ecber')
```

While the R files within this package include the required packages, you might encounter errors when using the functions. In such instances, it's possible that certain packages need to be reloaded. If this situation arises, we recommend installing the following packages as a precaution:
- tidyr
- dplyr
- vcd
- readxl
- writexl
- tidyverse

    
Example: Working With an Input File
------------------

For ecber to function correctly and produce the expected output, it requires a specific format of input file. This input file is generated using BORIS video coding software. You can find more information about [BORIS](https://www.boris.unito.it/) here. The output from BORIS is in the form of a CSV file.

Below are the initial lines of a sample CSV file from BORIS:


```
Observation id,Observation date,Description,Media file,Total length,FPS,Start time,Stop time,Duration,Test status,Subject,Behavior,Behavioral category,Modifiers,Behavior type,Start (s),Stop (s),Duration (s),Comment start,Comment stop
6008BECNR,2021-12-06 13:26:24,,T:/Unpredictability Video Coding Project/Montreal Videos/6008.mp4,1381.010,29.97,2000-01-01T00:00:00,2000-01-01T00:00:00,,,No focal subject,NotHoldingBaby,Mom Holding,,STATE,689.000,989.023,300.023,,
6008BECNR,2021-12-06 13:26:24,,T:/Unpredictability Video Coding Project/Montreal Videos/6008.mp4,1381.010,29.97,2000-01-01T00:00:00,2000-01-01T00:00:00,,,No focal subject,NoObjectInHand,Mom Manipulation,,STATE,689.000,750.421,61.421,,
```

To convert this input file into the expected Excel format, we provide the `ConvertCSVtoEXCEL function`. As an illustration, we've included a test file named **6008BECNR.csv**. See input files [here](https://github.com/chitram1/ecber/tree/main/test_files).

While the column names in the CSV file can vary, the conversion function requires specific columns including:
- Observation id: Name of the file with the encodings -- in the above case, the observation id is 6008BECNR.
- Start: Stands for the starting time in seconds for a given code, where start is 0.
- Stop: Stands for the stop time in seconds for a given code.
- Duration: Duration (in seconds) between the start and stop time for any given code.
- Behavioral category
- Behavior: Event and duration codes.
- Behavior type: Type of code (state/point).
  
The output of the CSV conversion function will be saved in a new directory named `ConvertedToExcels`. An example of the resulting Excel file is provided as a test file named **6008BECNR.xlsx**. This Excel file showcases the format after conversion.

```
Time_Relative_sf Duration_sf Observation Behavior Event_Type	
0	300.023	6008BECNR	NotHoldingBaby	State start	
0	61.421	6008BECNR	NoObjectInHand	State start	
0	60.798	6008BECNR	NotLookAtMomActivity	State start	
0	0	6008BECNR	Start	State point	
0	3.419	6008BECNR	positive	State start	
```

All of the functions for calculating entropy, reliability, and kappa values take in excel files as formatted above.


Calculating Entropy Rate
------------------

The function `ber_analyze_file` calculates the entropy rate from an input Excel file that either (1) has gone through the file conversion function described in the previous section or (2) is exported by Noldus Observer. For additional in-depth detail about the parameters and output of this function, please refer to **ccber's** SDD folder and their README file. 

**Of Note:**
In the entropy functions above, we have examples with set event codes included. The functions are hardcoded with these written into the variables. However, the functions written for affect and autonomy are the most flexible for user defined codes (with a maximum of 6 categories where there is no limit for codes contained in each one). If one of the signals written in our behavior_types list is not in your codebook, such as negative affect, it needs to stay in the list as a placeholder but it won't impact any of the calculations.

Here is the original behavior types list as written in ecber functions.

```
 behavior_types=list("positive" = c('positive'),
                     "negative" = c('negative'),
                     "neutral" = c('neutral'),
                     "autonomy_support" = c('AutonomySupport'),
                     "intrusiveness" = c('Intrusiveness'),
                     "neither" = c('Neither'),
                     "missing_types" = c('CantTellAffect', 'CantTellBehavior'))
                                                 
```

Below is an example of what it could look like with custom codes for 6 categories ("apple", "orange", "Pascal", "banana", "grape", "lemon", "NotAFruit", 'NotPascal'). If there are fewer than 6 categories in your research project, you can leave placeholders for that list such as "Intrusiveness", and since that code is not contained in your files, it won't affect any of the resulting calculations.

```
 behavior_types=list("positive" = c('apple'),
                     "negative" = c('banana'),
                     "neutral" = c('grape'),
                     "autonomy_support" = c('lemon'),
                     "intrusiveness" = c('orange'),
                     "neither" = c('Pascal'),
                     "missing_types" = c('NotAFruit', 'NotPascal'))
                                                 
```

- `f_loc` file location
- `plot_all` logical: Plot the data to observe the sequence of behaviors
- `plots_to_file` logical: send all plots to a file
- `tactile_padding` right padding adjustment to tactile events
- `auditory_padding` right padding adjustment to auditory events
- `behavior_types` dictionary of behavior types.  The required sections will depend on the type of entropy function this is. For example, if we are just using the following 3 sensory signals (tactile, auditory, and visual), the following are required: mom_auditory_types, mom_tactile_types, mom_visual_types, baby_visual_types, missing_types
- `missing_threshold` proportion of acceptable missing time
- `order` Markov model order value for how many states we go back in order to predict the subsequent state. This value always defaults to 1, and this means that each subsequent state depends directly on the preceding state.

```
ber_analyze_file('6008BECNR_t.xlsx', missing_threshold = 0.15, order = 1) --> The entropy is 0.4789043
ber_analyze_file('6008BECNR_t.xlsx', missing_threshold = 0.15, order = 2) --> The entropy is 0.4499737
ber_analyze_file('6008BECNR_t.xlsx', missing_threshold = 0.15, order = 2) --> The entropy is 0.4348698
```
As the order parameter increases, the entropy value decreases. This is because we have a larger gap between the states for predicting future states in the markov model and we lose information.

Our package builds upon the existing function by introducing novel behavior signals encompassing **affect** and **autonomy-granting behaviors**. These new signals encompass a range of actions and behaviors relevant to their respective categories. To facilitate the analysis of these signals, we've introduced additional functions:  `ber_analyze_file_affect`, `ber_analyze_file_affect_and_autonomy`.

Originally, the package incorporated three sensory signals: Tactile, Auditory, and Visual. With our expansion, we've seamlessly integrated **affect** and **autonomy-granting behaviors**.

For Affect, the following signals are included:
- Positive
- Negative
- Neutral

Similarly, for Autonomy Granting, the following signals are integrated:
- Autonomy Support
- Intrusiveness
- Neither


These new functions operate in a similar fashion to `ber_analyze_file` but they accept additional behavior types that enable the creation of new co-occurrence states. This expansion allows for a more nuanced understanding of the interplay between different behavior types, contributing to comprehensive behavior analysis.

```
ber_analyze_file('6008BECNR.xlsx') -->

SubjectID CanEstimateEntropy EntropyRate CombinedVideoDuration PercentMissing
6008BECNR              FALSE         NA                 300.126     0.1228284
```
In the above example, the percent missing threshold is higher than the default missing value of 0.1 (10% of the data) so we are unable to calculate the entropy rate for the file.

```
ber_analyze_file('6008BECNR.xlsx', missing_threshold = 0.15) -->

SubjectID CanEstimateEntropy EntropyRate TotalNumberOfTransitions
6008BECNR               TRUE   0.4789043                      194
```

Now that we have increased the missing threshold to a value of 15%, we are now able to estimate the entropy value.

```
ber_analyze_file_affect('6008BECNR.xlsx', missing_threshold = 0.15) -->

SubjectID CanEstimateEntropy EntropyRate TotalNumberOfTransitions
6008BECNR               TRUE    1.071117                      246

CombinedVideoDuration PercentMissing
300.126                    0.1078447             
```

Lastly, to provide a comprehensive analysis, our final function seamlessly combines autonomy-granting behaviors with affect. This integrated approach further enriches the understanding of the relationships between behavior types, paving the way for more insightful behavioral insights.

```
ber_analyze_file_affect_and_autonomy('6008BECNR.xlsx', missing_threshold = 0.15) -->

SubjectID CanEstimateEntropy EntropyRate TotalNumberOfTransitions
6008BECNR               TRUE    1.360337                      91

CombinedVideoDuration PercentMissing
300.126                    	0.1078447             
```

**Comprehensive State Combinations and Output** 

With the integration of affect and the computation of various state combinations, including total counts, total time, and average time for each collection, the output of these functions takes the form of a data frame.


| Column 1  | Column 2 | Column 3 | Column 4 | Column 5 | Column 6 |
| ------------- | ------------- | ------------- | ------------- | ------------- | ------------- |
| Total Count Tactile-Affect-Positive  | Total Time Tactile-Affect-Positive | Average Time Tactile-Affect-Positive | Total Count Tactile-Affect-Negative  | Total Time Tactile-Affect-Negative | Average Time Tactile-Affect-Negative  |

**Plotting Functions**

The original **ccber** has the following plotting functions: `plot_counts`, `plot_files`, and `plot_orig`, `plot_sequence`, `plot_transformed`, and `plot_transitions`. Our package expands upon these functions by including the additional co-occurrences with the new codes we added to the entropy functions to provide further information about the transitions.

- `plot_counts_w_affect`, `plot_files_w_affect`, and `plot_orig_w_affect`, `plot_sequence_w_affect`, `plot_transformed_w_affect`, and `plot_transitions_w_affect`
  
- `plot_counts_w_affect_autonomy`, `plot_files_w_affect_autonomy`, and `plot_orig_w_affect_autonomy`, `plot_sequence_w_affect_autonomy`, `plot_transformed_w_affect_autonomy`, and `plot_transitions_w_affect_autonomy`

You can call all of these functions while finding the entropy in the ber_analyze_file functions by setting parameters `plot_all = T` and `plots_to_file = T`. The `plot_all` parameter will display the plots in the plot window in R studio, but it needs to be large enough in order for the plots to render. If you get the following error [Error in plot.new() : figure margins too large], you will need to expand the window size. `plots_to_file` saves all of the 6 plots into a single pdf in a folder named after the specific entropy function you are calling in your current directory.

```
ber_analyze_file_affect('6008BECNR.xlsx', missing_threshold = 0.15, plots_to_file = T)
```

Upon invoking this function, a new directory named `TAV_AffectPlots` is generated within the current directory. Within this folder, all the generated plot files are consolidated into a single PDF named `6008BECNR_all_plots_w_affect.pdf`. This reference PDF file is conveniently accessible in the `test_files` folder for your convenience.

The plotting functions are thoughtfully designed to operate seamlessly within the context of the specified entropy functions. However, they can also be utilized independently if the necessary parameters are computed and supplied separately.


For example,
```
plot_counts_w_affect_autonomy(transition_counts, id_number)
```

The parameters essential for these plotting functions are primarily derived from intermediate calculations performed within the entropy functions. While these parameters are intricately linked to these calculations, it is advisable to generate the plots by adjusting the `plots_to_file` or `plot_all` parameters within the entropy functions.


Observation Coding Tools
------------------

Our package comprises functions dedicated to comparing reports from two coders and quantifying the similarity between their annotations. To offer flexibility in assessing agreement, we provide options to adjust the tolerance for similarity, allowing for a buffer of 1, 2, or n seconds between the code reports. This adaptive approach enables a comparison with increased leniency.
To compute the percent agreement, we construct a confusion matrix encompassing all the relevant events of interest. Each cell in the matrix represents the duration (in seconds) for which both coders concurred on that specific pair of events.

Relevant functions pivotal in computing reliability values encompass:
1. `createToleranceMatrix(filepath1, filepath2, eventlist, tolerance, file_seconds = 300)`
2. `buildMatrix(filepath1, filepath2, eventlist, file_seconds)` --> this function calculates the percent agreement and confusion matrix for a fixed tolerance value of 0
3. `createToleranceAndKappaValsFromMatrix(output_matrix)` --> this function takes in the confusion matrix and returns a data frame of the percent agreement, percent by chance, along with kappa values
4. `createToleranceAndKappaValsFromFiles(filepath1, filepath2, eventlist, tolerance, file_seconds)` --> this function does the same as the above function except that it takes in the excel file paths of both coders and the desired tolerance value for the confusion matrix

```
eventlist <- list(
  c("LookAtMomActivity","NotLookAtMomActivity","CantTellLooking"),
  c("positive","neutral","negative","CantTellAffect"),
  c("AutonomySupport","Neither","Intrusiveness","CantTellBehavior")
)

tolerance_0 = createToleranceMatrix(filepath1 = '6008BECEU.xlsx', filepath2 = '6008BECNR.xlsx', eventlist = eventlist, tolerance = 0, file_seconds = 300)
tolerance_0_v2 = buildMatrix('6008BECEU.xlsx', '6008BECNR.xlsx', eventlist, 300) #this is the exact same matrix as tolerance_0 since the buildMatrix is hardcoded for a tolerance of 0

tolerance_1 = createToleranceMatrix(filepath1 = '6008BECEU.xlsx', filepath2 = '6008BECNR.xlsx', eventlist = eventlist, tolerance = 1, file_seconds = 300)

#tolerance_0$percent_agreement_raw is equal to 89.23582
#tolerance_1$percent_agreement_raw is equal to 93.93792

createToleranceAndKappaValsFromMatrix(tolerance_0) #this function can either take in tolerance_0 or tolerance_1 since both are confusion matrices
```

As the tolerance increases, the percent agreement increases since we are more lenient in what we consider to be equal recorded codes. When the tolerance is 0 (the default value for the tolerance parameter), both coders need to have recorded the exact same code at the exact same time. If the codes are off by 1 second, they won't be recorded unless we use a tolerance of 1.

**Kappa Calculations**

We also have functions for calculating Cohen's omnibus kappa, percent agreement, percent by chance, weighted kappa, unweighted kappa standard error, and weighted kappa standard error. These metrics are useful for research projects where we have codes or ratings of behaviors. Using the kappa value, we can gauge inter-observer agreement. The kappa metric is useful since it corrects for percent chance.

These are the two functions we use: `createToleranceAndKappaValsFromMatrix(output_matrix)` and `createToleranceAndKappaValsFromFiles(filepath1, filepath2, eventlist, tolerance, file_seconds)`.

The only difference between the two functions is that the first function only takes in the confusion matrix of the two files which is an intermediate step calculated in the second function. Both functions output the same data frame as shown in the example below. Suppose the event list is the same as was defined previously.

```
eventlist <- list(
  c("LookAtMomActivity","NotLookAtMomActivity","CantTellLooking"),
  c("positive","neutral","negative","CantTellAffect"),
  c("AutonomySupport","Neither","Intrusiveness","CantTellBehavior")
)
createToleranceAndKappaValsFromFiles('6008BECEU.xlsx', '6008BECNR.xlsx', eventlist, 0, 300)

CohensOmnibusKappa PercentAgreement PercentByChance
         0.8726312        0.8913865       0.1472518
weightedKappa kappaUnweightedStdError
    0.9632845              0.01206674
kappaWeightedStdError
          0.003878426
```

In order to save the output of the reliability functions, we have a function that saves out all the percent agreement, percent by chance, and kappa values into an excel file.

1. `createReliabilityExcel(save_dir, filename1, filename2, eventlist, file_seconds)` --> saves out the values for fixed tolerance values of 0 and 1
2. `createReliabilityExcel2(save_dir, save_as_fname, filename1, filename2, eventlist, tolerance = c(0,1), file_seconds)` --> saves out the values for tolerance values that you can customize in a vector input

Below we will show an example of using these reliability functions. The eventlist will remain the same throughout all examples and the save_dir is the current directory the user is in which is denoted by ('./').

```
eventlist <- list(
  c("LookAtMomActivity","NotLookAtMomActivity","CantTellLooking"),
  c("positive","neutral","negative","CantTellAffect"),
  c("AutonomySupport","Neither","Intrusiveness","CantTellBehavior")
)

createReliabilityExcel('./', '6008BECEU.xlsx', '6008BECNR.xlsx', eventlist, 300)

createReliabilityExcel2('./', 'reliability_tolerance_vals.xlsx', '6008BECEU.xlsx', '6008BECNR.xlsx', eventlist, tolerance = c(0,1,2), 300)
```

Integrating BORIS with ecber
------------------

Video coding for this project was done in BORIS (Behavioral Observation Research Interactive Software). BORIS is an easy-to-use event-logging software for video/audio coding and live observations. BORIS is also a **free and open-source software** available for GNU/Linux, Windows, and MacOS. For more information, go to the BORIS official website and the following paper:

Friard, O., & Gamba, M. (2016). BORIS: a free, versatile open‐source event‐logging software for video/audio coding and live observations. Methods in ecology and evolution, 7(11), 1325-1330.

Our package equips you with the ability to seamlessly transform a directory of CSV files derived from BORIS output into **ecber** compatible files. This task is achieved by utilizing the `ConvertCSVtoEXCEL(csv_dir)` function. Simply provide the path to the directory containing the CSV files as input, and this function generates new files compatible with **ecber's** data format.

Additionally, we offer the `Boris2NoldusFileReShape(csv_file)` function, designed to convert an individual CSV file into a corresponding data frame that aligns with **ecber's** requirements. Unlike the previous function, this one doesn't save the output to an Excel file. Instead, it serves as a valuable intermediate tool, enabling you to visualize the structure of **ecber** compatible data frames directly.

`Boris2NoldusFileReShape(csv_file)` requires the csv_file to be read into with read.csv().

```
Boris2NoldusFileReShape(read.csv('6008BECNR.csv'))

Time_Relative_sf Duration_sf Observation Behavior       Event…¹
              <dbl>       <dbl> <chr>       <chr>          <chr>  
 1             0         300.   6008BECNR   NotHoldingBaby State …
 2             0          61.4  6008BECNR   NoObjectInHand State …
 3             0          60.8  6008BECNR   NotLookAtMomA… State …
 4             0           0    6008BECNR   Start          State …
 5             0           3.42 6008BECNR   positive       State …
 6             0           9.51 6008BECNR   Neither        State …
 7             3.42        0    6008BECNR   positive       State …
 8             3.42        1.09 6008BECNR   neutral        State …
 9             3.65        0    6008BECNR   Vocal          State …
10             4.51        0    6008BECNR   neutral        State …

```

The output above is cut-off, but this is what the function output would look like if called in the R console.

**Thank you for your interest in ccber and ecber! If you have any questions regarding the package, please email us at eugarte@ucdavis.edu or cmukherjee@ucdavis.edu.**
