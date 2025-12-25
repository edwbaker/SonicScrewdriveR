# Correlate channels in a WaveMC object

Uses the corenv function from seewave to calculate the envelope
correlation for timed events between the channels of a WaveMC object

## Usage

``` r
corWaveMC(wave, times, window, temp = 25, cluster = NULL)
```

## Arguments

- wave:

  A WaveMC object

- times:

  One or more times of events to correlate

- window:

  Width of the window to correlate in seconds (centred on times)

- temp:

  Air temperature in Celsius

- cluster:

  A cluster for parallel execution

## Value

List of corenv lists for events, and a list of the time differences
between channels
