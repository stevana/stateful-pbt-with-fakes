README.md: README-unprocessed.md
	pandoc --filter pandoc-include-code $? -t gfm -o $@
