md2man.1:
	./create.sh

.PHONY: show
show: md2man.1
	man "./$<"

.PHONY: clean
clean:
	-rm md2man.md
	-rm md2man.1
