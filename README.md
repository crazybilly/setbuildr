# Overview
An R package for building mobile-friendly slides from a set of songs.

## Building songs

Write each song as its own Rmarkdown file, ie. a rmd with h2-style headings to indicate slide breaks like this:

```markdown
## New Slide
lyrics lyrics lyrics
lyrics lyrics lyrics
lyrics lyrics lyrics

## New Slide
lyrics lyrics lyrics
lyrics lyrics lyrics
lyrics lyrics lyrics
```

## Building a set

Once you have your songs built, write, then knit an rmarkdown file as its own rmarkdown file. See sample-set.rmd as an example. You'll need to change the references to the CSS file to use the one in the inst directory (or use your own mobile-friendly).




