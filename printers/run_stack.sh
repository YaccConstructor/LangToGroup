cd ..
stack build || exit 1
mkdir out
cd out
stack exec LangToGroup-printer || exit 1