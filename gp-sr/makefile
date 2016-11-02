all: train test

train:
	scalac Training.scala sym_reg/*.scala

test:
	scalac Test.scala sym_reg/*.scala

.PHONY: clean

clean:
	rm *.class */*.class
