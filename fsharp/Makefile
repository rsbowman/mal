SRC = mal.fs error.fs reader.fs env.fs eval.fs core.fs main.fs

all: main

main: $(SRC)
	fsharpc -o $@.exe $^

clean:
	rm -f main *.exe
