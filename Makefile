all: AlgorithmW benchmark # AlgorithmW.pdf

GHC=ghc-7.6.3

AlgorithmW: AlgorithmW.lhs
	${GHC} $@

benchmark: benchmark.hs AlgorithmW.lhs
	${GHC} $@

AlgorithmW.pdf: AlgorithmW.tex
	latex AlgorithmW.tex
	pdflatex AlgorithmW.tex

AlgorithmW.tex: AlgorithmW.lhs
	lhs2TeX AlgorithmW.lhs > AlgorithmW.tex

clean:
	rm -f *tex *aux *log *out *ptb *~ *dvi *hi *o AlgorithmW
