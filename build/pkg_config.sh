#! /bin/sh

# Run XSB on selected packages, so they wwould be compiled at the
# installation time.
# Gets the full path name of the XSB executable as the
# first argument.

xsb_executable=$1

echo ""
echo "Configuring XSB packages. Wait..."
echo "[perlmatch]. [gap]. [wildmatch]. [regmatch]. [slx]. \
      [flora]. bootstrap_flora, [flrshell], flcompile_all_demos. \
      halt." \
		    | $xsb_executable 
