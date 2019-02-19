# song-time-tags

This package provides `#lang song-time-tags`.

## Example

```racket
#lang song-time-tags

dimensions 1400 720

song-time-tags
  (1,1)
    0:30 A
    0:45 A
    1:56 B
    2:07 A
  (1,2)
    0:14 B
    0:28 B
    0:39 C
    1:01 B

song-names
  (1,1) "Alpha"
  (1,2) "Bravo"

song-durations
  (1,1) 3:01
  (1,2) 2:04
```

## Installation

In DrRacket, go to the `File` menu, click `Package Manager`, and click on the `Do What I Mean` tab. In the `Package Source` field put `git://github.com/AlexKnauth/song-time-tags`, and click `Install`.

Or on the command line, run the command `raco pkg install git://github.com/AlexKnauth/song-time-tags`.
