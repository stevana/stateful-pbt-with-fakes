README.md: README-unprocessed.md
	pandoc 	--filter=pandoc-include-code \
		--from=gfm+attributes \
		--to=gfm \
		--output $@ \
		$? 
