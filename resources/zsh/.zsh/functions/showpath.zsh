# PATH を1行1エントリで表示する。
# $path は $PATH と連動した配列なので、コロン区切りを目で追う必要がない。
showpath() {
    print -l $path
}
