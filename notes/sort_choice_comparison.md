When replacing the insertion sort with the inline quick sort in the package, I took some timings to grow and predict trees and forests on the `low_high_earners.rda` dataset. These are the times elapsed.


|                          |    Grow Tree | Predict Tree |   Grow Forest | Predict Forest |
|:-------------------------|-------------:|-------------:|--------------:|--------------: |
| Insertion Sort           | 127.292 sec  |   0.427 sec  | 67.191 sec    |   0.348 sec    |
| Inline Quick Sort        |   2.059 sec  |   0.395 sec  |  1.422 sec    |   0.377 sec    |

The insertion sort was as of commit 15cf596b724cf5e6ffa51bb53263f393d6d4c2c5, and the inline quick sort was as of commit 4b1528d36081f275d47975d8178ef2921902cd62.

<!--
INSERTION SORT
at commit 15cf596b724cf5e6ffa51bb53263f393d6d4c2c5

tree fit
   user  system elapsed 
112.856  13.895 127.292 

tree predict
   user  system elapsed 
  0.356   0.070   0.427 

forest fit
   user  system elapsed 
 99.097  10.037  67.191 

forest predict

   user  system elapsed 
  0.342   0.040   0.348 


QSORT
at commit 4b1528d36081f275d47975d8178ef2921902cd62

tree fit
   user  system elapsed 
  1.735   0.325   2.059 

tree predict

   user  system elapsed 
  0.338   0.057   0.395 


forest fit

   user  system elapsed 
  2.006   0.248   1.422 


forest predict

   user  system elapsed 
  0.358   0.049   0.377 


-->
