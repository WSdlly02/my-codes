package main

import (
	"os"

	"course-election/internal/app"
)

func main() {
	os.Exit(app.Run(os.Args[1:]))
}
