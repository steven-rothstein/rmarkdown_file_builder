# This script converts R files to RMarkdown documents.

# # Package Dependency Check and Load

# This section loads necessary packages if installed, and installs them if not.

# ```{r}
package_dependencies = c("tidyr", "dplyr", "magrittr", "rmarkdown", "knitr", "markdown")

lapply_result = lapply(package_dependencies, function(curr_package) {
  if (curr_package %in% rownames(installed.packages())) {
    cat(paste("Loading package \"", curr_package, "\"....", sep = ""))
    
    library(
      curr_package,
      character.only = TRUE,
      #character.only is needed since curr_package is a variable
      quietly = TRUE,
      #to ensure not too verbose
      warn.conflicts = FALSE #to ensure not too verbose
    )
    
    cat(paste("complete.\n", sep = ""))
  } else {
    cat(paste("Installing package \"", curr_package, "\"\n\n", sep = ""))
    
    install.packages(curr_package, repos = "http://cran.us.r-project.org")
  }
})

cat("Finished loading packages.\n\n")

rm(package_dependencies, lapply_result)
# ```

# # Function and Global Variable Setup

# Initialize the list holding the parsed lines to write out

# ```{r}
CAT_CONTENT_LIST = list()
# ```

# Define functions for the script.

# This function returns the number of lines parsed thus far from the source file.
# ```{r}
get_content_list_length = function() {
  return(length(CAT_CONTENT_LIST))
}
# ```

# This function determines the next list index to utilize.
# ```{r}
generate_next_content_list_index = function() {
  return(get_content_list_length() + 1)
}
# ```

# `to_append` is the string for which at least 1 newline `('\n')` is to be appended.
# `num_newlines`, with default value of 1, is the number of newlines to append.
# The function's return value is `to_append` with however many newlines appended to the end, as defined by `num_newlines`.
# ```{r}
append_newline_to_string = function(to_append, num_newlines = 1) {
  paste(to_append,
        rep("\n", num_newlines) %>% paste(collapse = ""),
        sep = "") %>% return()
}
# ```

# `to_write` is the string for which at least 1 newline `('\n')` is to be appended.
# `num_newlines`, with default value of 1, is the number of newlines to append.
# The function's adds `to_write` with however many newlines appended to the end, as defined by `num_newlines`, to the list of parsed lines.
# ```{r}
add_rmarkdown_content_to_content_list = function(to_write,
                                                 num_newlines = 1,
                                                 after_position = get_content_list_length()) {
  if (after_position >= 0) {
    CAT_CONTENT_LIST <<-
      append(
        CAT_CONTENT_LIST,
        append_newline_to_string(to_write, num_newlines),
        after_position
      )
  }
}
# ```

# A simple function to return -1 and reinitialize line tracker variable(s) to a default value.
# ```{r}
reset_line_num_tracker = function() {
  return(-1)
}
# ```

# A simple function to return NA and reinitialize name tracker variable(s) to a default value.
# ```{r}
reset_function_name_tracker = function() {
  return(NA)
}
# ```

# # Validate Command Line Arguments

# This section checks that the command line arguments are appropriate.

# ```{r}
dir_arg_str = "-d"
file_arg_str = "-f"

curr_command_args = commandArgs(trailingOnly = TRUE)
if ((length(curr_command_args) != 3) ||
    (!(curr_command_args[1] %in% c(dir_arg_str, file_arg_str)))) {
  command_arg_df = data.frame(
    c("[dir_or_file]", "[/path/to/target]", "[document_title]"),
    c(dir_arg_str, "Documents/test_dir", "\"Example Title\""),
    c(
      paste(
        "Accepted values are \"",
        dir_arg_str,
        "\" to signal a directory is the target, or \"",
        file_arg_str,
        "\" to signal a file is the target.\n\n",
        sep = ""
      ),
      "The path to the directory or file to parse. If there are spaces in the directory, ensure there are quotes.\n\n",
      "The title of the resulting RMarkdown document. Ensure it is in quotes.\n\n"
    ),
    stringsAsFactors = FALSE
  )
  
  command_arg_fields_str = "command_arg_fields"
  command_arg_example_vals_str = "command_arg_example_vals"
  command_arg_desc_str = "command_arg_desc"
  
  names(command_arg_df) = c(command_arg_fields_str,
                            command_arg_example_vals_str,
                            command_arg_desc_str)
  
  command_arg_full_text_str = "command_arg_full_text"
  
  command_arg_df[, command_arg_full_text_str] = command_arg_df %>% {
    paste(.[, command_arg_fields_str], .[, command_arg_desc_str], sep = ": ")
  }
  
  cat("ERROR...incorrect command line arguments.\n\n")
  
  usage_prefix = "USAGE: Rscript /path/to/rmarkdown_file_builder.R "
  
  paste(usage_prefix,
        paste(command_arg_df[, command_arg_fields_str], collapse = " "),
        "\n\n",
        sep = "") %>% cat
  
  lapply_result = lapply(command_arg_df[, command_arg_full_text_str], cat)
  rm(lapply_result)
  
  paste("EXAMPLE ",
        usage_prefix,
        paste(command_arg_df[, command_arg_example_vals_str], collapse = " "),
        "\n",
        sep = "") %>% cat
  
  quit()
}
# ```

# # Write RMarkdown Headers

# In this section, we take our command line arguments and assign them to variables.
# Next, we write some headers that are needed to produce the resulting HTML document.

# ```{r}
dir_or_file = curr_command_args[1]
target_path = curr_command_args[2]
document_title = curr_command_args[3]

to_parse_full_filenames = target_path

is_dir = (dir_or_file == dir_arg_str)

if (is_dir) {
  to_parse_full_filenames = list.files(path = target_path,
                                       pattern = ".+\\.R$",
                                       full.names = TRUE)
}

to_parse_basenames = basename(to_parse_full_filenames)

yaml_delim = "---"
tab_substitute = "  "
tabbed_newline = paste("\n", tab_substitute, sep = "")

add_rmarkdown_content_to_content_list(yaml_delim)
paste("title: \"", document_title, "\"", sep = "") %>% add_rmarkdown_content_to_content_list
paste(
  "output:",
  tabbed_newline,
  "html_document:",
  tabbed_newline,
  tab_substitute,
  "toc: true",
  tabbed_newline,
  tab_substitute,
  "toc_depth: 4",
  sep = ""
) %>% add_rmarkdown_content_to_content_list
add_rmarkdown_content_to_content_list(yaml_delim, 2)

add_rmarkdown_content_to_content_list(
  "```{r setup, include=FALSE}\nknitr::opts_chunk$set(echo = TRUE, eval = FALSE, comment = \"\")\n```",
  2
)
# ```

# # Parse File(s)

# This section is where the magic happens. Here, we parse through the source file(s) and convert to RMarkdown.

# ```{r}
cat("Parsing...")

double_asterisk_str = "**"

for (curr_file_index in 1:length(to_parse_full_filenames)) {
  curr_full_filename = to_parse_full_filenames[curr_file_index]
  curr_basename = to_parse_basenames[curr_file_index]
  
  curr_file_connection = file(curr_full_filename, open = "r")
  curr_file_contents = readLines(curr_file_connection, warn = FALSE)
  
  if (is_dir) {
    paste(double_asterisk_str,
          curr_basename,
          double_asterisk_str,
          sep = "") %>% {
            paste("## ", ., sep = "")
          } %>% add_rmarkdown_content_to_content_list(2)
  }
  
  most_recent_code_block_start_index = reset_line_num_tracker()
  curr_function_name = reset_function_name_tracker()
  
  for (curr_file_line_index in 1:length(curr_file_contents)) {
    curr_file_line = curr_file_contents[curr_file_line_index]
    
    insert_yaml_delim = FALSE
    
    comment_char_start_regex = "^#"
    function_block_start_regex = "^(.+) = function\\(.+$"
    
    num_newlines = 1
    if (grepl(comment_char_start_regex, curr_file_line)) {
      #Remove comment character and any leading/trailing whitespace
      curr_file_line %<>% {
        gsub(comment_char_start_regex, "", .)
      } %>% trimws
      
      num_newlines = 2
      
      if (grepl("^```\\{r", curr_file_line)) {
        most_recent_code_block_start_index = generate_next_content_list_index()
      } else if (grepl("^```$", curr_file_line)) {
        insert_yaml_delim = TRUE
      }
    } else if (grepl(function_block_start_regex, curr_file_line)) {
      curr_function_name = gsub(function_block_start_regex, "\\1", curr_file_line) %>% {
        gsub("`", "", .)
      }
      
      last_blank_line_index = reset_line_num_tracker()
      if (most_recent_code_block_start_index > 1) {
        for (curr_blank_search_line_num in (most_recent_code_block_start_index - 1):1) {
          if (CAT_CONTENT_LIST[[curr_blank_search_line_num]] == "\n") {
            last_blank_line_index = curr_blank_search_line_num
            break
          }
        }
      }
      
      ifelse(is_dir, "### ", "## ") %>% paste(curr_function_name, sep = "") %>% add_rmarkdown_content_to_content_list(num_newlines = 2, after_position = last_blank_line_index)
      
      most_recent_code_block_start_index = reset_line_num_tracker()
      curr_function_name = reset_function_name_tracker()
    }
    
    add_rmarkdown_content_to_content_list(curr_file_line, num_newlines)
    if (insert_yaml_delim) {
      add_rmarkdown_content_to_content_list(yaml_delim, 2)
    }
  }
  
  close(curr_file_connection)
  
  #Append a couple of extra newlines before the next file.
  add_rmarkdown_content_to_content_list("", 2)
}

complete_with_newline_str = "complete\n"

cat(complete_with_newline_str)
# ```

# # Write Out Final File

# In this penultimate section, we loop through the final RMarkdown strings and write them to an output file.

# ```{r}
cat("Creating RMD file...")

dot_rmd_str = ".rmd"

output_file_name = paste("output_",
                         format(Sys.time(), "%Y%m%d%H%M%S"),
                         dot_rmd_str,
                         sep = "")
if (!is_dir) {
  output_file_name = basename(target_path) %>% {
    gsub("\\.R$", dot_rmd_str, .)
  }
}

output_dir_name = dirname(target_path) %>% normalizePath #expands "."
output_file_name %<>% {
  paste(output_dir_name, "/", ., sep = "")
}

for (cat_content_list_index in 1:get_content_list_length()) {
  append = TRUE
  if (cat_content_list_index == 1) {
    append = FALSE
  }
  
  cat(CAT_CONTENT_LIST[[cat_content_list_index]], file = output_file_name, append = append)
}

cat(complete_with_newline_str)
# ```

# # Render the HTML Document

# Last but not least, convert the RMarkdown to HTML.

#```{r}
cat("Rendering...")
render(output_file_name, "html_document")
#```