# moss
open source implementation of moss

## running
To run the plagiarism detection system, call the provided `moss` script with a directory containing source files as an argument: `./moss source_dir`.
By default, the script displays an output summary using `less`, but an optional `-o` argument can be used to write output to a file: `./moss source_dir -o results.out`. To output an HTML table rather than plain text, pass the `-h` flag.
