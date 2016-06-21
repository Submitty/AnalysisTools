# analysis tools

## identity tool
To make source files anonymous, run `./bin/anonymize_dirs` upon a directory. This tool accepts three flags (each with one argument), allowing the user to specify what should be replaced. Output files are placed in `moss_data/anonymized`.

`-n <f>` case-insensitively replaces all strings that occur in the whitespace-delimited file `f`.

`-t <f>` case-insensitively replaces all strings that occur in the first column of the CSV file `f` with the values in the second column. For example, if `f` contains `foo,bar`, all instances of `foo` will be replaced with `bar`.

`-r <substitution>` takes a regular expression substitution in `s/pattern/replace/` form, where pattern is a Perl-style regular expression and `replace` is a string. Currently, `replace` is inserted verbatim, but in the future it might be extended to perform some standard substitutions if necessary.

A typical use case might look like the following: `./bin/anonymize_dirs source_dir -t first_names.csv -t last_names.csv -n rcs_ids.txt -r 's/66[0-9]{7}/RIN/'`.

## plagiarism detection
To run the plagiarism detection system, call the provided `moss` script with a directory containing source files as an argument: `./moss source_dir`.
By default, the script displays an output summary using `less`, but an optional `-o` argument can be used to write output to a file: `./moss source_dir -o results.out`. To output an HTML table rather than plain text, pass the `-h` flag.

##Java Parser
The eclipse jdt library is required. 

##Class Structure Analysis
python-graph-tool required.
use: `python CSA.py student\_structure\_file subgraph\_structure\_file`


