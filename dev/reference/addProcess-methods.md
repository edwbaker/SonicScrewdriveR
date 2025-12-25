# Add a process to a Tagged Wave or WaveMC object

This function takes a `TaggedWave` or `TaggedWaveMC` object and adds a
process to the `processing` slot. This is used to keep a record of the
processes that have been applied to the object.

## Usage

``` r
addProcess(object, process, output = NULL, duration = NULL)

# S4 method for class 'TaggedWave'
addProcess(object, process, output = NULL, duration = NULL)

# S4 method for class 'TaggedWaveMC'
addProcess(object, process, output = NULL, duration = NULL)
```

## Arguments

- object:

  An object.

- process:

  A description of the process.

- output:

  The output of the process.

- duration:

  The duration of the process in seconds.

## Value

The object with the process added.
