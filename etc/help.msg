Usage:	xsb [flags] [-l] [-i]
	xsb [flags] -n
	xsb [flags] module
	xsb [flags] -B boot_module [-D cmd_loop_driver] [-t] [-e goal]
	xsb [flags] -B module_to_disassemble -d
	xsb -[h | v]
	xsb --help | --version | --nobanner | --quietload | --noprompt

      	memory management flags:
	    -c tcpsize | -m glsize | -o complsize | -u pdlsize | -r | -g gc_type
	miscellaneous flags:
	     -s | -S | -T

	module:
	    Module to execute after XSB starts up.
	    Module should have no suffixes, no directory part, and
	    the file module.xwam must be on the library search path.
      	boot_module:
	    This is a developer's option.
	    The -B flags tells XSB which bootstraping module to use instead
	    of the standard loader.  The loader must be specified using its
	    full pathname, and boot_module.xwam must exist.
    	module_to_disassemble:
	    This is a developer's option.
	    The -d flag tells XSB to act as a disassembler.
	    The -B flag specifies the module to disassemble.
	cmd_loop_driver:
	    The top-level command loop driver to be used instead of the
	    standard one.  Usually needed when XSB is run as a server.

      	    -i : bring up the XSB interpreter
       -e goal : evaluate goal when XSB starts up
            -l : the interpreter prints unbound variables using letters
            -n : used when calling XSB from C
            -B : specify the boot module to use in lieu of the standard loader
            -D : Sets top-level command loop driver to replace the default
            -t : trace execution at the SLG-WAM instruction level
                 (for this to work, build XSB with the --debug option)
            -d : disassemble the module and exit
          -c N : allocate N KB for the trail/choice-point stack
          -m N : allocate N KB for the local/global stack
          -o N : allocate N KB for the SLG completion stack
          -u N : allocate N KB for the SLG unification stack
            -r : turn off automatic stack expansion
    -g gc_type : choose the garbage collection ("none", "sliding",
						"indirection" or "copying")
            -p : enable Prolog profiling thru use of profile_call/1
            -s : maintain detailed statistical information
	    -S : set default tabling method to subsumption-based
            -T : print a trace of each called predicate
 -v, --version : print the version and configuration information about XSB
    -h, --help : print this help message
    --nobanner : don't show the XSB banner on startup
   --quietload : don't show the `module loaded' messages
    --noprompt : don't show prompt (for non-interactive use)
