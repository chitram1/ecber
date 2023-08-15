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

The observation ID is the name of the file with the encodings -- in the above case, the observation id is 6008BECNR. The Start..s. stands for the starting time, and Stop..s. stands for the stopping time, both of which are in seconds. The Duration..s. is the duration of seconds between the start and stop time. Behavioral.category and Behavior will be one of the event code states. Lastly, Behavior.type indicates whether or not it is a point or state event.

All of the functions for calculating entropy, reliability, and kappa values take in excel files as formatted above.

Calculating Entropy Rate
------------------


Observation Coding Tools
------------------

Integrating BORIS with ecber
------------------

If you have any questions regarding the package, please email us at eugarte@ucdavis.edu or cmukherjee@ucdavis.edu.
