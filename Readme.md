


# R Package `songbookdown`

This package uses [`bookdown`](https://bookdown.org/yihui/bookdown/) to turn plain text files into a songbook. Since it's using bookdown, a plethora of output formats are are available: beautiful html sites, pdf documents, docx files and epub just to name a few of them. Check the [`bookdown` documentation ](https://bookdown.org/yihui/bookdown/)(https://bookdown.org/yihui/bookdown/output-formats.html) for more information on this. 

The key concept behind this package is the following: You copy and paste plain text lyrics (with or without guitar chord information) from a site like https://www.ultimate-guitar.com/ or similar<sup>1</sup> and save it as "songname.txt" within a R-Project. You then add some metadata to this song using a yaml-style header. The `songbook` package then uses these songs to generate the input files that can be used by `bookdown` to create a songbook. 


<sup>1</sup> Remember to mention the source!