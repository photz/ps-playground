.PHONY: watch run

watch:
	find src -name '*.purs' \
	| grep -v '#' \
	| entr make run

run:
	pulp run
