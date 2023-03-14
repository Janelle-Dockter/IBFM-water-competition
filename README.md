# IBFM-water-competition

This cyber forest mimics the growth of a stand with both shade-tolerant and shade-intolerant species over thousands of years. Each year, the forest cycles through reproduction, above- and below-ground competition, growth, and mortality. There are two parts to the simulation and anaylsis as a whole: the C++ code and the R code. 
The C++ code is a mix of Python and C++, running the core simulation with varying forest characteristics like water limitation. The output is a set of data files that can be run with the R code and a set of images that are produced every 10 years showing the aerial-view canopy of the forest, the aerial-view understory, and the available water in the system after simulated rainfall and below-ground competition.
The R code produces a significant number of plots, allowing the comparison of forest data over time across varying water availability. The first series of plots map different soil aridity indexes across stand-level characteristics (population, age, basal area, understory basal area, overstory basal area, understory/overstory age, ratio of understory to overstory, etc) in two different root systems (implicit and explicit). The second set of graphs plots stand level characteristics for both the implicit and explicit root systems against a range of soil aridities. The third set graphs basal area, shade tolerance index, and mean age of both root systems over 5,000 years. The last set of graphs measures the water consumed in both root systems based on varying soil aridity levels, compared to the water available in the system.
