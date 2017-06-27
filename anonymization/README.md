### `bin/anonymize`
Usage example: `cat file.c | ./bin/anonymize -n name_list.txt -t to_replace.csv -r 's/foo/bar/' -a 10`

Anonymizes standard input, writing to standard output. In the example above, case-insensitively redacts all words found in the whitespace-delimited list in `name_list.txt`, replaces any word found in the first column of the two-column CSV file `to_replace.csv` with the corresponding word in the second column, and replaces any string matching regular expression `foo` with the string `bar`. If the `-a <n>` flag is provided, replace only in the first `n` lines of the input.

Multiple instances of each flag may be passed when appropriate. For example, `./bin/anonymize -n name_list.txt -n name_list_2.txt` will replace words found in either list.

### `bin/anonymization`
Usage example: `./bin/anonymization source_dir -n first_names.txt -n last_names.txt -t rcs_ids.txt -r 's/66[0-9]{7}/REDACTEDRIN/' -a 5 -l 3 source_dir/data/*`. 

Anonymizes all files in the given directory tree. The `-n`, `-t`, `-a`, and `-r` flags have the same meaning as those passed to `bin/anonymize`. One additional flag is also accepted: `-l`, which allows the user to specify a level in the directory tree (as an integer). Any directories at this level will also have their names anonymized according to the CSV files passed using the `-t` flag. Note that only those CSV files will be used for the replacement: simple name lists passed with `-n` will not be used for directory replacement.

Any additional arguments passed after the above flags are treated as files to ignore. To ignore entire directories, ensure the paths are well-formed, i.e. `foo/bar/baz/` (noting the `/` at the end).

Capture the standard output of this run to see the statistics regarding the number of replacements in directory names, file names, and file contents.
