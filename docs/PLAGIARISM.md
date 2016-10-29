### `bin/plagiarism`

## Invocation:

`./bin/plagiarism [-c <config_file>] <language> <source_dir> <pattern>...`, where:

* `config_file` is the path to a JSON configuration file. If not specified, this will default to `config/plagiarism.json`.
* `language` is the name of a directory in `lang/` (currently `c`, `python`, or `java`).
* `source_dir` is a directory containing student directories.
* `pattern` is any number of glob-style patterns describing the files within each student submission.

Compares every file matching any specified pattern found in the active student submission in `source_dir` against all other active student submissions, writing sorted (by percent match) CSV data to standard output. Each row in the CSV data takes the following format:

`<percent_match>,.analysis_data/concatenated/<first_student>,.analysis_data/concatenated/<second_student>,<pairs>`, where `<pairs>` is a `:` separated list of space-separated pairs of integers describing matching line numbers.

## Configuration File

An example:

    {
	    "WorkingDir" : ".analysis_data",
	    "HashBound" : 65536,
	    "GlobalFileName" : "__global_fingerprint_count__",
	    "SharedThreshold" : 0.6,

	    "c" : {
		    "UpperBound" : 30,
		    "LowerBound" : 15
	    },

	    "python" : {
		    "UpperBound" : 20,
		    "LowerBound" : 10
	    },

	    "java" : {
		    "UpperBound" : 30,
		    "LowerBound" : 15
	    }
    }

Field description:
* `WorkingDir` contains the path to a directory used to store temporary data.
* `HashBound` contains the upper limit upon the domain of the hashing function used during during matching. A lower number will increases matches (including false positives), a higher number will decrease matches (including false negative).
* `GlobalFileName` contains the name of a file (stored in `WorkingDir`) used to store data relating to overall token sequence occurences (for the purpose of detecting instructor-provided code).
* `SharedThreshold` contains the fraction of files that need to contain a given fingerprint in order for that fingerprint to be excluded from matching.

A configuration entry for each language is also expected for each language, containing the following fields `UpperBound` and `LowerBound`: `UpperBound` specifies the length of a series of tokens over which all sequences will be matched, and `LowerBound` specifies the length of a series of tokens under which no sequences will be matched.
