update_index:
	git show master:./presentation/index.html > index.html

upload:
	git push origin gh-pages
