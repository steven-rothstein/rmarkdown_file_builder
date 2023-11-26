# RMarkdown File Builder

This repository holds a simple R script that can convert R files into RMarkdown and render the result into HTML.

The input file or directory should contain R file(s) with RMarkdown syntax held in the comments.

## Usage

This script is a command line tool.

USAGE: `Rscript /path/to/rmarkdown_file_builder.R [dir_or_file] [/path/to/target] [document_title]`

`[dir_or_file]`: Accepted values are "-d" to signal a directory is the target, or "-f" to signal a file is the target.

`[/path/to/target]`: The path to the directory or file to parse. If there are spaces in the directory, ensure there are quotes.

`[document_title]`: The title of the resulting RMarkdown document. Ensure it is in quotes.

EXAMPLE USAGE: `Rscript /path/to/rmarkdown_file_builder.R -d Documents/test_dir "Example Title"`

The following command was run and generated the files in the `/example` directory of this repo.

`Rscript rmarkdown_file_builder.R -f rmarkdown_file_builder.R "RMarkdown File Builder"`

## Repository File Structure

- **/.gitignore**

  A very simple file to help with file management.

- **/README.md**

  This file.

- **/rmarkdown_file_builder.R**

  The script to run.

- **/example.zip**

  Holds example outputs from running the script against itself to produce RMD and HTML files.
