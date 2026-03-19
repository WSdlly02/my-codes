package app

func Run(args []string) int {
	if len(args) < 1 {
		usage()
		return 1
	}

	switch args[0] {
	case "status":
		if err := runStatus(args[1:]); err != nil {
			fatal(err)
		}
	case "query":
		if err := runQuery(args[1:]); err != nil {
			fatal(err)
		}
	case "warmup":
		if err := runWarmup(args[1:]); err != nil {
			fatal(err)
		}
	case "select":
		if err := runSelect(args[1:]); err != nil {
			fatal(err)
		}
	case "drop":
		if err := runDrop(args[1:]); err != nil {
			fatal(err)
		}
	default:
		usage()
		return 1
	}
	return 0
}
