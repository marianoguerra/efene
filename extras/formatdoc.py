import os
import re
import sys

def main():
    if len(sys.argv) != 2:
        print "usage: %s filename" % sys.argv[0]
        sys.exit(-1)

    text = file(sys.argv[1]).read()

    return re.sub("\n@(.*?)@\n", replace_code, text)

def replace_code(match):
    code = match.group()[2:-2]

    result = '<div class="langname">efene:</div>'
    result = result + os.popen2('./fn2shell.sh "' + code + '"')[1].read()
    result = result + '<div class="langname">erlang:</div>'
    result = result + os.popen2('./erl2shell.sh "' + code + '"')[1].read()

    return result


if __name__ == '__main__':
    print main()
