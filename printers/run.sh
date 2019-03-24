cd ..
cabal v1-build || exit 1
cd out
../dist/build/LangToGroup-printer/LangToGroup-printer || exit 1
xelatex out.tex
