# Ledeco

This tool does everything you want concerning the analysis of your datasets be it iOS privacy label or traffic.


## Dependencies

```
OpenJDK 18.0.1
sbt 1.6.2
prettyPictureMaker 0.4.6
```

You can download the ppm [here](https://github.com/simkoc/prettyPictureMaker) checkout the version tag and use `sbt publishLocal` to make ppm localy available for building ledeco.

## Building

```
sbt stage
```

## Running

```
$> ./run.sh
usage: ledeco {-c,-h} large-scale-privacy-label scoter createNameMap list import tracker analysis

Leak detection and comparison

          -c/--config <value> the location of the config file (def:./resources/main.conf)

          -h/--help prints this help message

          large-scale-privacy-label large scale privacy label analysis

          scoter the screenshot evaluator

          createNameMap create the name map for ipa -> apk

          list 

          import import the databases provided

          tracker actions concerning identification or modification of tracker

          analysis analyzing a given collection (set) for information
```

ledeco has a few more features than you probably need. We will only describe the interesting features: `large-scale-privacy-label,scoter,import,tracker, analysis`
