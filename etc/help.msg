Usage:	xsb [flags] [-l] [-i]
	xsb [flags] -n
	xsb [flags] module
	xsb [flags] -B boot_module [-D cmd_loop_driver] [-t] [-e goal]
	xsb [flags] -B module_to_disassemble -d
	xsb -[h | v]

      	memory management flags:
	    -c tcpsize | -m glsize | -o complsize | -u pdlsize | -r
	    -g gc_type (where gc_type is one of "none", "sliding", or "copying")
	miscellaneous flags:
	     -s | -T
	module:
	    Module to execute after XSB starts up.
	    Module should have no suffixes, no directory part, and
	    the file module.O must be on the library search path.
      	boot_module:
	    This is a developer's option.
	    The -B flags tells XSB which bootstraping module to use instead
	    of the standard loader. The loader must be specified using its
	    full pathname, and boot_module.O must exist.
    	module_to_disassemble:
	    This is a developer's option.
	    The -d flag tells XSB to act as a disassembler.
	    The -B flag specifies the module to disassemble.
	cmd_loop_driver:
	    The top-level command loop driver to be used instead of the
	    standard one. Usually needed when XSB is run as a server.

      	    -i : bring up the XSB interpreter
       -e goal : evaluate goal when XSB starts up
            -l : the interpreter prints unbound variables using letters
            -n : used when calling XSB from C
            -B : specify the boot module to use in lieu of the standard loader
            -D : Sets top-level command loop driver to replace the default.
            -t : trace execution at the SLG-WAM instruction level
                 (for this to work, build XSB with the --debug option)
            -d : disassemble the loader and exit
          -c N : allocate N KB for the trail/choice-point stack
          -m N : allocate N KB for the local/global stack
          -o N : allocate N KB for the SLG completion stackof
          -u N : allocate N KB for the SLG unification (table copy) stack
            -r : turn off automatic stack expansion
            -s : record maximum usage of stacks (for statistics/0).
            -T : print a trace of each called predicate
            -v : print the version and configuration information about XSB.
            -h : print this help message
