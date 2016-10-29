### `bin/plagiarism`

Invocation: `./bin/plagiarism [-c <config_file>] <language> <source_dir> <pattern>...`, where: 

* `config_file` is the path to a JSON configuration file. If not specified, this will default to `config/plagiarism.json`.
* `language` is the name of a directory in `lang/` (currently `c`, `python`, `python2`, or `java`).
* `source_dir` is a directory containing student directories.
* `pattern` is any number of glob-style patterns describing the files within each student submission.

Compares every file matching any specified pattern found in the active student submission in `source_dir` against all other active student submissions, writing sorted (by percent match) CSV data to standard output. Each row in the CSV data takes the following format:

`<percent_match>,.analysis_data/concatenated/<first_student>,.analysis_data/concatenated/<second_student>,<pairs>`, where `<pairs>` is a `:` separated list of space-separated pairs of integers describing matching line numbers.
