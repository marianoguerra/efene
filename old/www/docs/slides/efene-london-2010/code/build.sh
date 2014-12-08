#!/usr/bin/env sh

for i in *.ifn;do pygmentize -f html $i > $i.html; done
for i in *.fn;do pygmentize -f html $i > $i.html; done
for i in *.erl;do pygmentize -f html $i > $i.html; done
