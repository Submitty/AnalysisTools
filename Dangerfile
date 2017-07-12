affected_files = git.added_files + git.modified_files
haskell_files = affected_files.select { |file| file.end_with?(".hs") }
hlint.lint haskell_files inline_mode: true ignore: "Parse error"
