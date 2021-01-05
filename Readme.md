


# R Package `songbookdown`

# What is this about?

This package uses [`bookdown`](https://bookdown.org/yihui/bookdown/) to turn plain text files into a songbook. Since it's using `bookdown`, a plethora of output formats are are available: beautiful html sites, pdf documents, docx files and epub just to name a few of them. Check the [`bookdown` documentation](https://bookdown.org/yihui/bookdown/output-formats.html) for more information on this. 

The key concept behind this package is the following: You copy and paste plain text lyrics (with or without guitar chord information) from a site like https://www.ultimate-guitar.com/ or similar<sup>1</sup> and save it as "songname.txt" within an R-Project. You then add some metadata to this song using a yaml-style header. The `songbook` package then uses these textfiles to generate intermediate files that in turn will be used by `bookdown` to create a songbook. 

Just as `knitr` leverages pandoc (see image below), `songbookdown` leverages `bookdown` and the rest of the tools to build an awesome songbook. 

<sup>1</sup> Remember to mention the source!

![](https://bookdown.org/yihui/rmarkdown-cookbook/images/workflow.png)

## Why use `songbookdown`?

The package offers a way to specify typical song related metadata in a simple and structured way without thinking about how this metadata is used and displayed in the output. Such song related data can be title of the song, artist, tempo etc. 

## Installing `songbookdown`

The package is not on CRAN, so to install `songbookdown` use `devtools` or `remotes` and install it from github like so: 

```
devtools::install_github("ratnanil/songbookdown")
```

## Warning!

This package is still under development. Things will most likely break, contact me if you are interested in using it, then I will try and be more careful :-)