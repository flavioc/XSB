#! /bin/sh

# This runs XSB on some packages, so they will be compiled at the
# installation time. Gets the full path name of the xsb executable as the
# first argument.

xsb_executable=$1

echo ""
echo "Configuring XSB packages. Wait..."
echo "[perlmatch]. [slx]. [flip]. parse_all_demos, end. halt." \
		    | $xsb_executable 
