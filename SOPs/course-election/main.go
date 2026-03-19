package main

import "os"

func main() {
	if len(os.Args) < 2 {
		usage()
		os.Exit(1)
	}

	switch os.Args[1] {
	case "status":
		if err := runStatus(os.Args[2:]); err != nil {
			fatal(err)
		}
	case "warmup":
		if err := runWarmup(os.Args[2:]); err != nil {
			fatal(err)
		}
	case "select":
		if err := runSelect(os.Args[2:]); err != nil {
			fatal(err)
		}
	case "drop":
		if err := runDrop(os.Args[2:]); err != nil {
			fatal(err)
		}
	default:
		usage()
		os.Exit(1)
	}
}
