# Project Brain-Area analysis

This project is about data preparation and some kinds of analysis mainly for morphometrical dataframes. In the first part, the given dataframes get read and organized for the further analysis. Secondly, structural covariance networks get calculated. Finally, The Brain measurements get compared between the <i>healthy controls</i> and the <i>patients</i>.

Download this Repository or clone it with:
```
git clone https://github.com/MaximilianLoessl/data_analysis.git
```

# File preparation
Launch the file [file_prep.R](file_prep.R) in the Terminal:         (basic just read option)

```
Rscript --vanilla /path/to/Rscript/file_prep.R -y /path/to/health_controls -p /path/to/patients
```
Get help with the -h flag:
```
Rscript --vanilla /home/user/PD_analysis/file_prep.R -h
```

<code>
Options:
        -y CHARACTER, --healthy=CHARACTER
                Give the path tho the healthy controls.
     Should be a dataframe containing all the subjects.  [example: /path/to/HC]

        -p CHARACTER, --patient=CHARACTER
                Give the path tho the patients.
     Should be a dataframe containing all the subjects.  [example: /path/to/PT]

        -o CHARACTER, --out=CHARACTER
                output file name [default "/mnt/c/Users/maxil/Documents/data_analysis/files"]

        -g CHARACTER, --group=CHARACTER
                add a grouping variable for both dataframes. In case you want to import multiple couples of HC/Pat.
    NOT POSSIBLE YET!!

        -f, --full
                Full analysis

        -k KEEP, --keep=KEEP
                Columnname you want to keep

        -j CHARACTER, --joke=CHARACTER
                build-in jokes

        -h, --help
                Show this help message and exit

</code>

The files of the <i>Health Controls HC</i> and the <i>Patients (PT)</i> should be in seperate folders. It is also possible to directly provide the path to the file. Furthermore, 2 files, containing for example the left and right hemisphere, can be used as input. They will be put together. If the input file consists only one hemisphere, it gets processed nevertheless

The file should come in the form of .txt or .csv. The first row should be the names of the Subjects, containing this somewhere in the string:

<code>sex_age</code>

For example:

<code>Patient_007_m_45_something</code>

This is important to get the age column if the age isn't already contained in the dataframe in the form of "age", "Age" or "AGE"

Based on the input as HC or PT, the <i>group</i> column gets computed and filled with either <i>hc</i> or <i>pt</i> 

The structure of the file depends on the given Atlas. The Atlas can be found and modified in the <b>[region.csv](region.csv)</b> file. As default, it contains the <b>Desikan-Killiany Atlas</b> (can be changed)

The modified Dataframes will get stored in the <b>files</b> directory, which is the default output directory. For the structure of the output, see below. 

### Output directory
As default, the output directory will be the files folder. But this can be changed via the `-o` flag.

### Group variable
It is possible to add a grouping variable with the `-g` flag. This option is given to keep the overview of the analysis of multiple groups.

### Full analysis
If the `-f` flag is added in the command line, the files [scn.R](scn.R) and [comparisons.R](comparisons.R) get launched. If the `-f` flag is not added, the dataframes only get read into the files folder.

### Keep a column
In the case you don't want to loose a column, because the dataframe gets reduced by the Atlas, you can add the `-k` flag to keep a certain column.

### Build-in jokes
Just because I've never seen a flag like this. Run it with the `-j` flag

---


# Analysis

The files for further analysis are taken from the files folder, where they were saved either by preprocessing using [file_prep.R](file_prep.R), or manually.

The dataframes should be saved as healthy_frame.csv and patient_frame.csv and look like this:

| Subject                     | lh_banksssts | lh_entorhinal |  ...  | age  |
| :---                        |    :----:    |      :---:    | :---: | ---: |
| Patient_007_m_45_something  | 4.28         | 3.78          |  ...  |  45  |
| Patient_008_f_33_else       | 2.25         | 3.12          |  ...  |  33  |

This is the format of the [file_prep.R](file_prep.R) standard-output. It may look different, when the [region](region.csv) gets changed



## Structural covariance networks (SCN)
The file [scn.R](scn.R) computes structural covariance networks. It calculates as default the <i>global efficiency</i> based on a 1000x sampled 80% fraction of the given dataframes (uses smallest group for sampling what could lead to larger variance if the size differs between the HC and PT frames). The mesurements get controlled for age by traking the residuals from linear models (`Roi ~ age`).

The output is the global efficiency (default) as a function of the threshold for the correlation matrix. The 0.5 threshold gets highlighted in the output plot.

If this file runs automatically by using the `-f` flag beforehand, the default measurement for the structural covariance network is the <i>global efficiency</i> and the default output folder is the <b>out</b> folder. This can be changed either by changing the code in the bottom of the [file_prep.R file](file_prep.R) or by running the files individually:

```
Rscript --vanilla /path/to/Rscript/file_prep.R -y /path/to/health_controls -p /path/to/patients
Rscript --vanilla /path/to/Rscript/scn.R -h

```
### Output folder
The output folder can be changed with the `-o` flag.

### SCN measurements
As default, the measurement is the <i>global efficiency</i>. But this can be changed into <i>number of edges</i>, <i>mean pathlength</i> or <i>global transitivity</i>. Check the correct writing via:
```
Rscript --vanilla /path/to/Rscript/scn.R -h
```

## Comparisons
The [comparisons.R file](comparisons.R) compares the healthy frame with the patients frame by fitting region wise a linear model on the healthy controls frame, applying the model to the patients frame and calculating the d-values by comparing the region wise mean of the residuals. Afterwards the resulting d-values get plotted on a brain to see the diverging regions between the healthy controls and the patients.

## Problems

- Sometimes, you have to manually install the packages with <code>install.packages("packagename")</code>. This can cause the file to stop even before the help option. In that case do this:

```R
install.packages("tidyverse")
install.packages("optparse")
library(tidyverse)
library(optparse)
```
    After succesfully running these commands you should be able to get the help option.

## What is still missing?

1. The opportunity to read multiple separate subjects from a folder

2. A special analysis for the BIDS format

3. The plotting of the comparisons is still only set for the Desikan-Killiany Atlas. Should be adapted to automatically chose the atlas based on the input atlas

4. The grouping variable should be implemented better. Mainly in the names of the output plots. And does it even work??

5. Are the commands looking different in CMD?