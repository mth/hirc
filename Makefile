# Use the following to install ghc with mtl and network libs on debian:
# aptitude install ghc6 libghc6-mtl-dev libghc6-network-dev libghc6-http-dev libghc6-regex-compat-dev

hirc: Hirc.hs HircBot.hs
	ghc -o $@ -O2 --make $+

all: hirc acrogrep history

acrogrep: acrogrep.c
	cc -o $@ -W -O2 $+

history: acrogrep.c
	cc -o $@ -W -O2 $+

clean:
	rm -f hirc *.hi *.o acrogrep history
