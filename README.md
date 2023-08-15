ecber: an expansion of the ccber `R` Package used for the HERD Lab at UC Davis 
============================================================================================================
ccber was developed for the Conte Center @ UCI and ecber was edited by Dr. Elisa Ugarte and Chitra Mukherjee from UC Davis.

The goal of this package is to estimate the behavioral entropy rate, and our package includes additional features such as 
increasing the tolerance values (including a buffer of 1, 2, 3, seconds etc.) for calculating the reliability (similarity) between two coders.


Acknowledgements
------------------

Our package is based off of the code written in the ccber repo: (https://github.com/bvegetabile/ccber/blob/master/README.md). We used the entropy rate code as a foundation and expanded upon the capabilities with additional functions and features. Many of the functions used to calculate the entroppy rate use ccber's unchanged functions including the main markov matrix calculations. Please refer to ccber to see the original functions and the explanations included for their functionality.

ccber's package includes:
- markov matrix functions for first, second, and n-order markov models in order to calculate entropy rate
- creating transition count matrixes for the number of transitions between states
- entropy rate calculation functions from event coded data files or directory of files
- plotting functions to display transition count matrices and transitions between sensory signal states (the main sensory signals between auditory, visual, and tactile)
  
ecber's package additions:
- adding affect to sensory signals 
-   additional pairwise counts for all behavioral type combinations when adding affect and autonomy granting
-   new plotting functions to display additional pairwise counts
-   reliability functions which calculate the duration agreement between coders for sensory signal codes
-   increased tolerance of 1+ seconds between events when calculating reliability of two different coder files
-   kappa value correlation calculation functions
-   conversion functions for individual files and directories from BORIS file format to excel files
-   option to increase markov matrix lag (markov model order) when calculating entropy; this increases the number of previous states that are used for the new state prediction

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

Although the necessary packages are installed within the R files written for this package, if you get errors when using the functions, they may be because some packages need to be loaded in again. If this is the case, please install the following packages as a precaution:
1. tidyr
2. dplyr
3. vcd
4. readxl
5. writexl
6. tidyverse
    
Example: Working With an Input File
------------------

ecber requires a certain form of Input File in order to use the functions and get the expected output. The input file comes from BORIS which outputs a CSV file. Included below are the first few lines of a CSV file from BORIS. Using the `ConvertCSVtoEXCEL` function, we convert this input file into the expected excel format. This file is included as a test file: 6008BECNR.csv

```
Observation id,Observation date,Description,Media file,Total length,FPS,Start time,Stop time,Duration,Test status,Subject,Behavior,Behavioral category,Modifiers,Behavior type,Start (s),Stop (s),Duration (s),Comment start,Comment stop
6008BECNR,2021-12-06 13:26:24,,T:/Unpredictability Video Coding Project/Montreal Videos/6008.mp4,1381.010,29.97,2000-01-01T00:00:00,2000-01-01T00:00:00,,,No focal subject,NotHoldingBaby,Mom Holding,,STATE,689.000,989.023,300.023,,
6008BECNR,2021-12-06 13:26:24,,T:/Unpredictability Video Coding Project/Montreal Videos/6008.mp4,1381.010,29.97,2000-01-01T00:00:00,2000-01-01T00:00:00,,,No focal subject,NoObjectInHand,Mom Manipulation,,STATE,689.000,750.421,61.421,,
```

There can be many different column names, but the required ones for the conversion function include Observation.id, Start..s., Stop..s., Duration..s., Behavioral.category, Behavior, Behavior.type.

The output of the CSV conversion function will be in a new directory called ConvertedToExcels. An example of what the excel file looks like is shown below. This excel file is also included as a test file: 6008BECNR.xlsx

```
Time_Relative_sf Duration_sf Observation Behavior Event_Type	
0	300.023	6008BECNR	NotHoldingBaby	State start	
0	61.421	6008BECNR	NoObjectInHand	State start	
0	60.798	6008BECNR	NotLookAtMomActivity	State start	
0	0	6008BECNR	Start	State point	
0	3.419	6008BECNR	positive	State start	
```

- The observation ID is the name of the file with the encodings -- in the above case, the observation id is 6008BECNR. - The Start..s. stands for the starting time, and Stop..s. stands for the stopping time, both of which are in seconds. - The Duration..s. is the duration of seconds between the start and stop time.
- Behavioral.category and Behavior will be one of the event code states.
- Behavior.type indicates whether or not it is a point or state event.

All of the functions for calculating entropy, reliability, and kappa values take in excel files as formatted above.

Calculating Entropy Rate
------------------

The function `ber_analyze_file` calculates the entropy rate from an input excel file which has gone through the file conversion function described in the previous section. For additional in-depth detail about the parameters and output of this function, please refer to ccber's SDD folder and their README file. 

- `f_loc` file location
- `plot_all` logical: Plot the data to observe the sequence of behaviors
- `plots_to_file` logical: send all plots to a file
- `tactile_padding` right padding adjustment to tactile events
- `auditory_padding` right padding adjustment to auditory events
- `behavior_types` dictionary of behavior types.  The required sections will depend on the type of entropy function this is. For example, if we are just using the following 3 sensory signals (tactile, auditory, and visual), the following are required: mom_auditory_types, mom_tactile_types, mom_visual_types, baby_visual_types, missing_types
- `missing_threshold` proportion of acceptable missing time
- `order` markov model order value for how many states we go back in order to predict the subsequent state. This value always defaults to 1, and this means that each subsequent state depends directly on the preceding state.

```
ber_analyze_file('6008BECNR_t.xlsx', missing_threshold = 0.15, order = 1) --> The entropy is 0.4789043
ber_analyze_file('6008BECNR_t.xlsx', missing_threshold = 0.15, order = 2) --> The entropy is 0.4499737
ber_analyze_file('6008BECNR_t.xlsx', missing_threshold = 0.15, order = 2) --> The entropy is 0.4348698
```
As the order parameter increases, the entropy value decreases. This is because we have a larger gap between the states for predicting future states in the markov model and we lose information.

Our package expands upon this function by incorporating new behavior signals which include Affect and Autonomy Granting, each of which containing actions/behaviors that fall within this category. We have additional functions to include affect and autonomy granting: `ber_analyze_file_affect`, `ber_analyze_file_affect_and_autonomy`.

Originally, we have 3 sensory signals: Tactile, Auditory, and Visual. We have added affect and autonomy granting.

For Affect, we have the following sensory signals:
1. Positive
2. Negative
3. Neutral

For Autonomy Granting, we have the following sensory signals:
1. Autonomy Support
2. Intrusiveness
3. Neither

These functions behave in the same manner as `ber_analyze_file` except that they take in addional behavior types which we will use to create new co-occurrence states. 

```
ber_analyze_file('6008BECNR.xlsx') -->

SubjectID CanEstimateEntropy EntropyRate CombinedVideoDuration PercentMissing
6008BECNR              FALSE         NA                 300.126     0.1228284
```
In the above example, the percent missing threshold is higher than the default missing value of 0.1 so we are unable to calculate the entropy rate for the file.

```
ber_analyze_file('6008BECNR.xlsx', missing_threshold = 0.15) -->

SubjectID CanEstimateEntropy EntropyRate TotalNumberOfTransitions
6008BECNR               TRUE   0.4789043                      194
```

Now that we have increased the missing threshold to a value greater than the percent missing in the file, showing the first few columns of output, we see that we are now able to estimate the entropy value.

```
ber_analyze_file_affect('6008BECNR.xlsx', missing_threshold = 0.15) -->

SubjectID CanEstimateEntropy EntropyRate TotalNumberOfTransitions
6008BECNR               TRUE    1.071117                      246

CombinedVideoDuration PercentMissing
300.126                    0.1078447             
```
Now that we have included affect as a sensory signal and are finding all possible combinations of states including the total count, total time, and average time for each of these collections, the final output of these functions will be a data frame of 1 row and 96 columns. We show the first 6 columns above.

Lastly, our final function incorporates the autonomy granting sensory signals with the affect sensory signals.
```
ber_analyze_file_affect_and_autonomy('6008BECNR.xlsx', missing_threshold = 0.15) -->

SubjectID CanEstimateEntropy EntropyRate TotalNumberOfTransitions
6008BECNR               TRUE    1.360337                      91

CombinedVideoDuration PercentMissing
300.126                    	0.1078447             
```

We are finding all pairs of the 6 sensory signals, finding the unique pairs between affect signals and autonomy granting signals. This gives us 9 pairs, and since we also count the occurrence of these signals alone, we get 15 total occurrence states that we find the count, total time, and average time for. Therefore, we are expecting 51 total columns when including the additional information that is produced in the output data frame. 

**Plotting Functions**
The original ccber has the following plotting functions: `plot_counts`, `plot_files`, and `plot_orig`, `plot_sequence`, `plot_transformed`, and `plot_transitions`. Our package expands upon these functions by including the additional co-occurrences with the new sensory signals that we added in the entropy functions to provide further information about the transitions.

- `plot_counts_w_affect`, `plot_files_w_affect`, and `plot_orig_w_affect`, `plot_sequence_w_affect`, `plot_transformed_w_affect`, and `plot_transitions_w_affect`
  
- `plot_counts_w_affect_autonomy`, `plot_files_w_affect_autonomy`, and `plot_orig_w_affect_autonomy`, `plot_sequence_w_affect_autonomy`, `plot_transformed_w_affect_autonomy`, and `plot_transitions_w_affect_autonomy`

You can call all of these functions while finding the entropy in the ber_analyze_file functions by setting parameters `plot_all = T` and `plots_to_file = T`. The `plot_all` parameter will display the plots in the plot window in R studio, but it needs to be large enough in order for the plots to render. If you get the following error [Error in plot.new() : figure margins too large], you will need to expand the window size. `plots_to_file` saves all of the 6 plots into a single pdf in a folder named after the specific entropy function you are calling in your current directory.

```
ber_analyze_file_affect('6008BECNR.xlsx', missing_threshold = 0.15, plots_to_file = T)
```
This function call creates a new folder called TAV_AffectPlots in the current directory I was in, and all of the files are saved in one pdf called 6008BECNR_all_plots_w_affect.pdf. This file is also uploaded in the test_files folder for reference.

The plotting functions were written so that they could be called within these specific entropy functions, but they can also be invocated individually as long as the parameters they require are calculated and inputted separately.

For example,
```
plot_counts_w_affect_autonomy(transition_counts, id_number)
```

The parameters required by these plotting functions are largely intermediate calculations done in the entropy functions, however, so it is recommended to produce these plots by changing the `plots_to_file` or `plot_all` parameters in the entropy functions.


Observation Coding Tools
------------------

Our package includes functions for comparing 2 coders and finding the similarity in their reports. We include options to increase the tolerance for similarity with a 1, 2, or n-second buffer between both code reports. This allows us to compare reports without having agreement requirements which are too strict. We find the percent agreement by creating a confusion matrix with all the included events that we are looking for, and each entry in the matrix is the amount of seconds that both coders agreed for that pair of events.

Relevant functions we will be using for calculating reliability values include:
1. `createToleranceMatrix(filepath1, filepath2, eventlist, tolerance = 0, file_seconds = 300)`


```
eventlist <- list(
  c("LookAtMomActivity","NotLookAtMomActivity","CantTellLooking"),
  c("positive","neutral","negative","CantTellAffect"),
  c("AutonomySupport","Neither","Intrusiveness","CantTellBehavior")
)

tolerance_0 = createToleranceMatrix(filepath1 = '6008BECEU_t.xlsx', filepath2 = '6008BECNR_t.xlsx', eventlist = eventlist, tolerance = 0, file_seconds = 300)

tolerance_1 = createToleranceMatrix(filepath1 = '6008BECEU_t.xlsx', filepath2 = '6008BECNR_t.xlsx', eventlist = eventlist, tolerance = 1, file_seconds = 300)

#tolerance_0$percent_agreement_raw is equal to 89.23582
#tolerance_1$percent_agreement_raw is equal to 93.93792
```

As the tolerance increases, the percent agreement increases since we are more lenient in what we consider to be equal recorded codes. When the tolerance is 0 (the default value for the tolerance parameter), both coders need to have recorded the exact same signal at the exact same time. If the codes are off by 1 second, they won't be recorded unless we use a tolerance of 1.

Integrating BORIS with ecber
------------------

If you have any questions regarding the package, please email us at eugarte@ucdavis.edu or cmukherjee@ucdavis.edu.
