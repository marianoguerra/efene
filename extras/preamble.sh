#!/usr/bin/env sh
echo "<html>\n<head>\n <title>efene tutorial</title>\n"
echo ' <link rel="stylesheet" href="master.css" type="text/css" media="screen" charset="utf-8" />\n'
echo "</head>\n"
echo "<body>"
echo "\n <style>"
pygmentize -S tango -f html 2>/dev/null
echo "\n </style>"
