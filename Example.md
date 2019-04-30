## Example
<pre>
<strong>> runAlignment(model = "Moral1 BY prostit homosex abortion divorce;
                      Moral2 BY benefits taxes bribes;", 
             group = "country",
             dat = wvs.s1,
             sim.samples = c(50, 100, 500),
             Mplus_com = "wine mplus",
             summaries = F
             )</strong>
# creates a bunch of files in your working directory


<strong>> extractAlignment("free.out")</strong>
           Row.names AlignedParameter    R2         invariant.gr non.invar.gr Fit.contribution Factor
 Intercepts ABORTION             1.28 0.818      1 2 3 4 5 6 7 9         10 8          -21.301 MORAL1
  Intercepts DIVORCE            3.358  0.91           3 5 6 10 7    2 4 8 9 1          -29.399 MORAL1
  Intercepts HOMOSEX            2.074 0.867        1 2 3 5 6 7 9       4 10 8          -22.674 MORAL1
  Intercepts PROSTIT             1.11 0.864 1 2 3 4 5 6 10 7 8 9                       -18.385 MORAL1
   Loadings ABORTION            2.079 0.403          1 3 5 6 8 9     2 4 10 7          -21.210 MORAL1
    Loadings DIVORCE            1.629 0.468          1 2 3 4 6 7     5 10 8 9          -19.872 MORAL1
    Loadings HOMOSEX            2.041 0.626     1 3 4 6 10 7 8 9          2 5          -17.178 MORAL1
    Loadings PROSTIT            2.033 0.365              1 2 4 7 3 5 6 10 8 9          -23.144 MORAL1
 Intercepts BENEFITS            1.458     0           1 4 5 6 10    2 3 7 8 9          -40.034 MORAL2
   Intercepts BRIBES            1.053     0    1 2 3 4 5 6 7 8 9           10          -15.512 MORAL2
    Intercepts TAXES            1.233 0.583        1 2 5 6 7 8 9       3 4 10          -21.566 MORAL2
   Loadings BENEFITS            1.138 0.314   1 2 3 4 6 10 7 8 9            5          -21.387 MORAL2
     Loadings BRIBES            1.083 0.446        2 3 4 5 6 7 9       10 8 1          -20.835 MORAL2
      Loadings TAXES            1.419 0.622 1 2 3 4 5 6 10 7 8 9                       -17.890 MORAL2


<strong>> fixed <- extractAlignment("fixed.out") </strong>
           Row.names AlignedParameter    R2         invariant.gr   non.invar.gr Fit.contribution Factor
 Intercepts ABORTION            2.744 0.695                3 6 9 2 4 5 10 7 8 1          -30.047 MORAL1
  Intercepts DIVORCE            4.498 0.971     1 3 5 6 10 7 8 9            2 4          -20.798 MORAL1
  Intercepts HOMOSEX            3.522 0.954         1 4 6 10 7 9        2 3 5 8          -19.092 MORAL1
  Intercepts PROSTIT            2.108  0.89     2 3 4 5 6 10 7 9            8 1          -23.416 MORAL1
   Loadings ABORTION            1.584 0.403          1 3 5 6 8 9       2 4 10 7          -21.210 MORAL1
    Loadings DIVORCE            1.241 0.468          1 2 3 4 6 7       5 10 8 9          -19.872 MORAL1
    Loadings HOMOSEX            1.562 0.626   1 2 3 4 6 10 7 8 9              5          -17.178 MORAL1
    Loadings PROSTIT            1.549 0.365              1 2 4 7   3 5 6 10 8 9          -23.144 MORAL1
 Intercepts BENEFITS            1.714 0.022           3 4 5 6 10      2 7 8 9 1          -38.364 MORAL2
   Intercepts BRIBES            1.281     0          1 2 4 5 7 9       3 6 10 8          -18.526 MORAL2
    Intercepts TAXES             1.61 0.677     1 2 5 6 10 7 8 9            3 4          -21.044 MORAL2
   Loadings BENEFITS            0.902 0.314   1 2 3 4 6 10 7 8 9              5          -21.387 MORAL2
     Loadings BRIBES            0.858 0.446        2 3 4 5 6 7 9         10 8 1          -20.835 MORAL2
      Loadings TAXES            1.124 0.622 1 2 3 4 5 6 10 7 8 9                         -17.890 MORAL2


<strong>> fixed$ranking.table </strong>

$MORAL1
   1 3 2 6 5 7 4 9 10 8
1  X > > > > > > >  > >
3  < X > > > > > >  > >
2  < < X   > > > >  > >
6  < <   X     > >  > >
5  < < <   X   > >  > >
7  < < <     X > >  > >
4  < < < < < < X >  > >
9  < < < < < < < X  > >
10 < < < < < < < <  X >
8  < < < < < < < <  < X

$MORAL2
   4 1 5 8 7 9 6 3 2 10
4  X   > > > > > > >  >
1    X   > > > > > >  >
5  <   X > > > > > >  >
8  < < < X   > > > >  >
7  < < <   X > > > >  >
9  < < < < < X        >
6  < < < < <   X      >
3  < < < < <     X    >
2  < < < < <       X   
10 < < < < < < < <    X


<strong>> print(t(fixed$non.invariant.pars), quote=F) </strong>
                    2 3 4 5 6 10 7 9 8 1
Intercepts PROSTIT                   X X
Intercepts HOMOSEX  X X   X          X  
Intercepts ABORTION X   X X   X  X   X X
Intercepts DIVORCE  X   X               
Intercepts BENEFITS X            X X X X
Intercepts TAXES      X X               
Intercepts BRIBES     X     X X      X  
Loadings PROSTIT      X   X X X    X X  
Loadings HOMOSEX          X             
Loadings ABORTION   X   X     X  X      
Loadings DIVORCE          X   X    X X  
Loadings BENEFITS         X             
Loadings TAXES                          
Loadings BRIBES               X      X X


<strong>> extractAlignmentSim(c("sim50.out", "sim100.out", "sim500.out"))</strong>

⎯⎯⎯⎯⎯⎯⎯⎯⎯ MORAL1 ⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
                              sim50.out sim100.out sim500.out
Correlations Mean Average          0.82      0.890      0.966
Correlations Variance Average      0.73      0.857      0.969
Correlations Mean SD               0.12      0.102      0.021
Correlations Variance SD           0.18      0.083      0.020
MSE Mean Average                   1.71      1.903      2.145
MSE Variance Average               1.29      0.864      0.833
MSE Mean SD                        0.42      0.401      0.117
MSE Variance SD                    5.36      0.193      0.098
                                           sim50.out sim100.out sim500.out
Correlation of average means with true          0.90       0.94       0.97
Correlation of average variances with true      0.74       1.00       1.00
MSE of average means with true                  1.65       1.87       2.14
MSE of average variances with true              0.77       0.82       0.83

⎯⎯⎯⎯⎯⎯⎯⎯⎯ MORAL2 ⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
                              sim50.out sim100.out sim500.out
Correlations Mean Average          0.62       0.70      0.747
Correlations Variance Average      0.27       0.53      0.896
Correlations Mean SD               0.22       0.19      0.091
Correlations Variance SD           0.40       0.35      0.107
MSE Mean Average                   0.67       0.41      0.300
MSE Variance Average             489.41       9.54      0.613
MSE Mean SD                        4.33       0.14      0.076
MSE Variance SD                 9781.23      55.78      0.191
                                           sim50.out sim100.out sim500.out
Correlation of average means with true          0.78       0.91       0.81
Correlation of average variances with true      0.68      -0.23       0.99
MSE of average means with true                  0.45       0.35       0.28
MSE of average variances with true            455.51       4.92       0.55

</pre>