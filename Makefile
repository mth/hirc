# Use the following to install ghc with mtl and network libs on debian:
# aptitude install ghc6 libghc6-mtl-dev libghc6-network-dev libghc6-http-dev libghc6-regex-posix-dev

HIRC = Hirc.hs Utf8Conv.hs Calculator.hs HircBot.hs

hirc: $(HIRC) initenv.o
	ghc -o $@ -W -O2 -XBangPatterns -XForeignFunctionInterface --make $+
	strip $@

hirc-prof: $(HIRC)
	ghc -o $@ -W -O2 -XBangPatterns --make $+\
		-prof -auto-all -caf-all -fforce-recomp

# This can be used to convert old databases into utf-8
utf8-recode: Utf8Conv.hs Utf8Recode.hs
	ghc -o $@ -W -O2 --make $+
	strip $@

calc: Calculator.hs CalcTest.hs
	ghc -o $@ -W -O2 --make $+
	strip $@

convert-conf: ConvertConf.hs
	ghc -o $@ -W -O2 --make $+
	strip $@

all: hirc acrogrep history

acrogrep: acrogrep.c
	cc -o $@ -W -O2 $+

history: history.c
	cc -o $@ -W -O2 $+

clean:
	rm -f hirc *.hi *.o acrogrep history
