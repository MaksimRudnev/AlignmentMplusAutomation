# Automation of alignment in Mplus

Three functions that facilitate setting up (frequentist) alignmnet in Mplus, extracting, and summarizing its results.

Installation: 
```
source('https://raw.githubusercontent.com/MaksimRudnev/AlignmentMplusAutomation/master/source_repo.R')

```

## runAlignment

<pre>
runAlignment(
  model,                           # Formula in Mplus format
  group,                           # Grouping variable
  dat,                             # Data object 
  sim.samples = c(100, 500, 1000), # Group sample sizes for simulation, 
                                   #  the length of this vector also determines 
                                   #  a number of simulation studies.
  sim.reps = 500,                  # A number of simulated datasets in each simulation.
  Mplus_com = "Mplus",             # Sometimes you don't have a direct access to Mplus, so this 
                                   #  argument specifies what to send to a system command line.
  path = getwd(),                  # Where all the .inp, .out, and .dat files should be stored?
  summaries = F                    # If the <strong>extractAlignment() </strong>
                                   #   and <strong>extractAlignmentSim() </strong>should
                                   #   be run after all the Mplus work is done.
  )
</pre>
This function will set up and run free alignment, then it will set up and run fixed alignment using the smallest mean from the free alignmnet output. optionally, it will also set up and run simulations as recommended by Muthen & Asparouhov (2014).

## extractAlignment
```
extractAlignment("fixed.out")
```

It has a single argument which is a filename of the Mplus output.
This function extracts alignment-related information from the Mplus output file. The most valueable us summary which includes all the info usually reported about about alignment.

## extractAlignmentSim

```
extractAlignmentSim(c("sim500.out", "sim100.out", "sim1000.out"))
```

It has a single argument which is a vector of Mplus output files containing results of alignment simulations. It extracts only one portion that is directly related to alignment, namely correlations of true and estimated means.

***

See the tutorial on measurement invariance alignmnet in Mplus : https://maksimrudnev.com/2019/04/20/alignment-tutorial/ 

Use freely and credit as 
> Rudnev M.(2019) Alignment measurement invariance: Tutorial. URL: https://maksimrudnev.com/2019/04/20/alignment-tutorial/ 


---

See [example](Example.md)