# Use the following to install ghc with mtl and network libs on debian:
# aptitude install ghc6 libghc6-mtl-dev libghc6-network-dev libghc6-http-dev libghc6-regex-compat-dev

hirc: Hirc.hs Utf8Conv.hs Calculator.hs HircBot.hs
	ghc -o $@ -W -O2 -XBangPatterns --make $+
	strip $@

# This can be used to convert old databases into utf-8
utf8-recode: Utf8Conv.hs Utf8Recode.hs
	ghc -o $@ -W -O2 --make $+
	strip $@

calc: Calculator.hs CalcTest.hs
	ghc -o $@ -W -O2 --make $+
	strip $@

all: hirc acrogrep history

acrogrep: acrogrep.c
	cc -o $@ -W -O2 $+

history: history.c
	cc -o $@ -W -O2 $+

clean:
	rm -f hirc *.hi *.o acrogrep history
