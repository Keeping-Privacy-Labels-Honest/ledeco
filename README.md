# Ledeco

This tool does everything you want concerning the analysis of your datasets be it iOS privacy label or traffic.


## Dependencies

```
OpenJDK 18.0.1
sbt 1.6.2
prettyPictureMaker 0.4.6
IndiaPaleAle 1.0.1
```

You can download the PrettyPrictureMaker (ppm) [here](https://github.com/simkoc/prettyPictureMaker) checkout the version tag and use `sbt publishLocal` to make ppm locally available for building ledeco.

IndiaPaleAle is part of our artifact publication and available [here](https://github.com/Keeping-Privacy-Labels-Honest/IndiaPaleAle/releases/tag/v1.0.1),  checkout the version tag and use `sbt publishLocal` to make IndiaPaleAle locally available for building ledeco.

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

ledeco has a few more features than you probably need. We will only describe the interesting features: `large-scale-privacy-label,scoter,import,tracker,analysis`

### large-scale-privacy-label

```
$> ./run.sh large-scale-privacy-label [folder] [where] [what] {-d,-h,-e,-f}
```

This action analyzes the set of privacy labels contained in subfolders contained below `folder`. `where` names the folder to output the results into whereas `what` describes the action that is to be done. For `what` you can choose between `analysis,bar-plotting,keyness-plotting` each conducting the named action. The `-d` parameter can be used to provide a csv list of categories (subfolder names) that are not to be evaluated. The `-e` flag equalizes the categories so each category has only as many labels as the lowest category. `-f` is a deprecated flag.

### Scoter

```
./run.sh [folder] [out]
```

The `folder` contains a set of iPhone screenshots to be evaluated. `Out names the file into which to dump the results.


### Import

```
./run.sh [user] [password] [dbname] {-l,-p,-h} iphone
```

This action imports the database `dbname` of `user` with `password` at `-l` (default: `localhost`) listening on `-p` (default: 5432) into the database specified by the main.conf.
The provided database is the database used by the traffic collection as the database used by `ledeco` is slightly different.


### tracker

```
./run.sh tracker convertEasy [easyTxt] [out]
```

Converts the provided easy privacy or easy list txt file specified by `easyTxt` into a set of json files that can be used for further analysis outputtet into the `out` folder.

### analysis

The base action to conduct your analysis 

```
./run.sh analysis {-a} perform [basics honey-data IDFA hosts GDPR-required tracker]
```

This action uses the analysis config provided by `-a` (default: `resources/analysisConfig.json`). It is then up to you what analysis action you want to do:
- basics: prints out basic numbers concerning the configured data set
- honey-data: search for configured honey-data in your configured data set
- IDFA: search for the IDFA in your configured data set
- hosts: check what hosts are contacted
- GDPR-required: uses the screenshot evaluation to determine what apps should have a GDPR dialog
- tracker: uses the configured folder of tracker jsons to check what trackers are contacted in the configured data set
