#! /bin/sh

# Run XSB on selected packages, so they wwould be compiled at the
# installation time.
# Gets the full path name of the XSB executable as the
# first argument.

flora_command="[flora]. bootstrap_flora, [flrshell], flcompile_all_demos."

while test 1=1
do
    case "$1" in
     -skip-flora-demos)
	    shift
	    flora_command="[flora]. bootstrap_flora, [flrshell]."
	    ;;
    *)
	    break
	    ;;
    esac
done

xsb_executable=$1

echo ""
echo "Configuring XSB packages. Wait..."
echo "[perlmatch]. [gap]. [wildmatch]. [regmatch]. [slx]. \
      $flora_command \
      halt." \
		    | $xsb_executable 
