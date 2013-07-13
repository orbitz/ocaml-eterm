.PHONY: all clean test

all:
	$(MAKE) -C lib

test: all
	$(MAKE) -C lib test
	$(MAKE) -C test test

clean:
	$(MAKE) -C lib clean

