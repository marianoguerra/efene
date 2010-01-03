package main

import "os"
import "exec"
import "fmt"
import "flag"
import "path"
import "syscall"
import "strings"

var outputDir = flag.String("o", ".", "output directory")
var showHelp = flag.Bool("h", false, "show this help")
var verbose = flag.Bool("v", false, "be verbose")

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

	if flag.NArg() == 0 {
		showErrorAndQuit("No input files", -1)
	}

	// check that args end with the efene suffix and that the files exist
	args := flag.Args()
	for i := 0; i < flag.NArg(); i++ {
		file := args[i]
		if !strings.HasSuffix(file, fnSuffix) {
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

func main() {
	checkArgs()
	output, absError := abspath(*outputDir)
	if absError != nil {
		showErrorAndQuit(absError.String(), -1)
	}

	programPath, programError := getProgramPath()
	if programError != nil {
		showErrorAndQuit(programError.String(), -1)
	}

	erlPath, pathError := exec.LookPath("erl")

	if pathError != nil {
		showErrorAndQuit("Can't find erl program", -1)
	}


	head := []string{erlPath, "-run", "efene", "main", output}
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

	if *verbose {
		fmt.Println("cd " + programPath)
		fmt.Print(erlPath + " ")
		printArray(args, true)
	}

	pid, error := os.ForkExec(erlPath, args, os.Environ(), programPath, []*os.File{os.Stdin, os.Stdout, os.Stderr})

	if error != nil {
		showErrorAndQuit(error.String(), -1)
	}

	os.Wait(pid, 0)
}

