Baseball Players Batting Statistics Visualization
========================================================
author: Brian Morge
date: July 26, 2015
transition: linear


This app was created as part of the John's Hopkins [Data Science Specialization](http://www.coursera.org/specialization/jhudatascience/) on Coursera

Introduction
========================================================
- View Career Batting Statistics for any in MLB history (AL or NL only).  
- Compare 2 player's stats in the same year.
- Or view the MLB league leaders (Top 10's) of a given year.
- Great for exploring different eras:
 + Start with a favorite player e.g. Hank Aaron
 + Find his best years on the Career Statistics Page
 + Look at the league top 10's for those years to find other players of interest.


<div class="page">2</div>

Career Summaries and Top10s
========================================================
left: 60%
![plot of chunk summary](Presentation-figure/summary-1.png) 
***
<font size="7">
<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Player </th>
   <th style="text-align:right;"> Homeruns </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Bautista, Jose </td>
   <td style="text-align:right;"> 54 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pujols, Albert </td>
   <td style="text-align:right;"> 42 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Konerko, Paul </td>
   <td style="text-align:right;"> 39 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cabrera, Miguel </td>
   <td style="text-align:right;"> 38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dunn, Adam </td>
   <td style="text-align:right;"> 38 </td>
  </tr>
</tbody>
</table>
</font>
<div class="page">3</div>


Player Comparisons
========================================================
![plot of chunk comparisons](Presentation-figure/comparisons-1.png) 
<div class="page">4</div>

Conclusion
========================================================
This application is a simple tool for visualizing baseball offensive statistics.
It does not attempt to perform any predictions or sophisticated statistical
analysis.  
The purpose is to provide some easy to digest charts that could be
used to compare players, determine who was the best in a given statistic in a
season, and to look a player's career arc at a glance.

Links:
* [Shiny application](http://uncarvedblock.shinyapps.io/baseballapp)
* [R Presentation](http://rpubs.com/uncarvedblock64/baseballapp) (this document)
* [Github repository](https://github.com/uncarvedblock64/baseballapp)
<div class="page">5</div>
