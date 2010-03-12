package main

import "os"
import "exec"
import "fmt"
import "flag"
import "path"
import "syscall"
import "strings"

var outputDir  = flag.String("o", ".", "output directory")
var showHelp   = flag.Bool("h", false, "show this help")
var verbose    = flag.Bool("v", false, "be verbose")
var outputType = flag.String("t", "beam", "output type (beam, ast, tree, lex, erl, fn, erl2ast)")
var eval       = flag.String("c", "", "eval expression")
var eeval      = flag.String("C", "", "eval expression and print it in efene and erlang")
var shell      = flag.Bool("s", false, "shell")

const fnSuffix = ".fn"

func showErrorAndQuit(message string, status int) {
	os.Stderr.WriteString(message + "\n")
	os.Exit(status)
}

func checkArgs() {
	flag.Parse()

	if *showHelp {
		flag.Usage()
		os.Exit(0)
	}

	if *eval != "" || *eeval != "" || *shell{
		return
	}

	if flag.NArg() == 0 {
		showErrorAndQuit("No input files", -1)
	}

	// check that args end with the efene suffix and that the files exist
	args := flag.Args()
	for i := 0; i < flag.NArg(); i++ {
		file := args[i]
		if *outputType == "erl2ast" {
			if !strings.HasSuffix(file, ".erl") {
				showErrorAndQuit("Invalid input filename. '" + file + "'", -1)
			}

		} else if !strings.HasSuffix(file, fnSuffix) {
			showErrorAndQuit("Invalid input filename '" + file + "'", -1)
		}

		fmt.Printf("")

		if syscall.Access(file, syscall.O_RDONLY) != 0 {
			showErrorAndQuit("Can't read file '" + file + "'", -1)
		}
	}
}

func printArray(things []string, printNewLine bool) {
	for i := 0; i < len(things); i++ {
		fmt.Printf(things[i] + " ")
	}

	if printNewLine {
		fmt.Printf("\n")
	}
}

func getProgramPath() (string, os.Error) {
	program := os.Args[0]
	dir, _ := path.Split(program)

	if dir == "" {
		binPath, pathError := exec.LookPath(program)

		if pathError != nil {
			if syscall.Access(program, syscall.O_RDONLY) != 0 {
				return "", os.NewError("Path to " + program + " couldn't be found")
			}

			cwd, cwdError := os.Getwd()

			if cwdError != nil {
				return "", cwdError
			}

			return cwd, nil
		}

		binPath, _ = path.Split(binPath)
		return binPath, nil
	}

	dir, _ = abspath(dir)
	return dir, nil
}

func abspath(thepath string) (string, os.Error) {
	if !strings.HasPrefix(thepath, "/") {
		cwd, cwdError := os.Getwd()

		if cwdError != nil {
			return "", cwdError
		}

		return path.Clean(path.Join(cwd, thepath)), nil
	}

	return thepath, nil
}

func getPath(name string) string {
	path, pathError := exec.LookPath(name)

	if pathError != nil {
		showErrorAndQuit("Can't find " + name + " program", -1)
	}

	return path
}

func run(args []string) {
	programPath, programError := getProgramPath()
	if programError != nil {
		showErrorAndQuit(programError.String(), -1)
	}

	erlPath := getPath("erl")

	if *verbose {
		fmt.Println("cd " + programPath)
		fmt.Print(erlPath+ " ")
		printArray(args, true)
	}

	pid, error := os.ForkExec(erlPath, args, os.Environ(), programPath,
		[]*os.File{os.Stdin, os.Stdout, os.Stderr})

	if error != nil {
		showErrorAndQuit(error.String(), -1)
	}

	os.Wait(pid, 0)
}

func main() {
	checkArgs()
	erlPath := getPath("erl")

	if *shell {
		shellArgs := []string{erlPath, "-run", "efene", "main",
			"shell", "-run", "init", "stop", "-noshell"}
		run(shellArgs)
		return
	}

	if *eval != "" || *eeval != "" {
		option := "eval"
		expr   := *eval

		if *eeval != "" {
			option = "eeval"
			expr   = *eeval
		}

		evalArgs := []string{erlPath, "-run", "efene", "main",
			option, expr, "-run", "init", "stop", "-noshell"}
		run(evalArgs)
		return
	}

	output, absError := abspath(*outputDir)
	if absError != nil {
		showErrorAndQuit(absError.String(), -1)
	}

	head := []string{erlPath, "-run", "efene", "main", *outputType, output}
	tail := []string{"-run", "init", "stop", "-noshell"}
	start := len(head) + flag.NArg()
	args := make([]string, len(head) + len(tail) + flag.NArg())

	for i := 0; i < len(head); i++ {
		args[i] = head[i]
	}

	for i := len(head); i < len(head) + flag.NArg(); i++ {
		args[i], _ = abspath(flag.Arg(i - len(head)))
	}

	for i := start; i < start + len(tail); i++ {
		args[i] = tail[i - start]
	}

	run(args)
}

