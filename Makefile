README.md: README-unprocessed.md
	pandoc --filter pandoc-include-code $? -o $@
