# Ledeco

This tool does everything you want concerning the analysis of your datasets be it iOS privacy label or traffic.


## Dependencies

```
OpenJDK 18.0.1
sbt 1.6.2
prettyPictureMaker 0.4.6
```

You can download the PrettyPrictureMaker (ppm) [here](https://github.com/simkoc/prettyPictureMaker) checkout the version tag and use `sbt publishLocal` to make ppm localy available for building ledeco.

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

### large-scale-privacy-label

```
$> ./run.sh large-scale-privacy-label [folder] [where] [what] {-d,-h,-e,-f}
```

This action analyzes the set of privacy labels contained in subfolders contained below `folder`. `where` names the folder to output the results into whereas `what` describes the action that is to be done. For `what` you can choose between `analysis,bar-plotting,keyness-plotting` each conducting the named action. The `-d` parameter can be used to provide a csv list of categories (subfolder names) that are not to be evaluated. The `-e` flag equalizes the categories so each category has only as many labels as the lowest category. `-f` is a deprecated flag.
