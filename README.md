# deepchirp-diagnostics
Evaluates performance of model in deepchirp by comparing the predicted results to the test set

The repository comes with two example files that show how this program works. 

To run, just call

Rscript <yhat file> <edit file> 

Then in the terminal it would output diagnostics on words that were false negatives, false positives, mislabled words (non z, as in one sylable was labled as another), overlabling (when one syllable is counted as 2 or more), and interval overlaps (including the false positive and false negative percentages when to syllables overlap).

It will save these outputted diagnostics in a file called diagnostics. 

It will save data containing individual diagnostics on every single predicted syllable in a csv file called raw_data.csv.

Finally it will produce graphs on the false positive and false negative overlap, seperated by syllable, in a files called fp_overlap.jpg
and fn_overlap.jpg, respectively. 

The two files in this repository are given as an example to test the program, but it will work with any two csv files of bird scoring data seperated by name, start, and stop. Just first enter the predicted csv, and then the test set csv, and it will evaluate the efficiency on the model in terms of predicting complete words.



**Dependencies**

R. 
The library ggplot2. To install on unix, check https://www.r-bloggers.com/installing-r-packages/ for an example
