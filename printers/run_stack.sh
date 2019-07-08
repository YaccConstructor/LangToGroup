cd ..
stack build || exit 1
mkdir out
cd out
../.stack-work/dist/x86_64-linux/Cabal-2.2.0.1/build/LangToGroup-printer/LangToGroup-printer || exit 1
xelatex out.tex