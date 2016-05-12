# moss
open source implementation of moss

## Usage

Running `make` builds a number of binaries, described below, to the `bin` folder.

### Included Tools

#### `bin/winnow`

Command line arguments: upper threshold, lower threshold.

Reads a sequence of whitespace-delimited decimal integer tokens from standard input. Writes a sequence of whitespace-delimited hash values (the fingerprint corresponding to those tokens) to standard output.

#### `bin/walk`

Command line arguments: directory path

Traverses the given directory, mirroring its structure within `moss_data/<timestamp>`. Each non-directory file is identified as being a particular filetype, tokenized, and winnowed. The resulting fingerprint is placed within the corresponding file in the new tree.

#### `bin/genpairs`

Command line arguments: directory path

Traverses the given directory, writing all possible combinations of non-directory paths to standard output (delimited by newlines).

#### `bin/compare_fingerprints`

Command line arguments: none

Reads pairs of paths from standard input, writing a summary of the fingerprint match to standard output.
