#! /bin/sh


cat <<EOF
*******************************************************************************

The installation process is now complete. The log is in: Installation_summary

We shall greatly appreciate, if you would agree to send us this log.
Installation logs help the XSB group to keep track of the usage of the
system on different architectures and to focus its efforts on the 
architectures that are used more frequently.

The log will be sent automatically to  \`xsb-contact@cs.sunysb.edu'
Would you like to send us the installation log? (y/n): y
EOF

read sendlog

if test "$sendlog" != "n" -a "$sendlog" != "no" ; then
    (cat sendlog.msg Installation_summary \
	| mail xsb-contact@cs.sunysb.edu) \
    && echo "" ; echo "Thank you!"; echo ""
fi

if test ! -f "$HOME/.xsb/registration" ; then

    cat <<EOF
*******************************************************************************

Now, the final step of the installation procedure is to register you as  
an XSB user.  This will be done by automatically sending an e-mail  
message to  \`xsb-contact@cs.sunysb.edu'
   
We strongly encourage you to register.  If you do so, you will be	  
included in a mailing list for future releases and major bug-fixes.  

Would you like to register as an XSB user? (y/n): y
EOF

    read register

    if test "$register" != "n" -a "$register" != "no" ; then
	(cat registration.msg | mail xsb-contact@cs.sunysb.edu \
	    && echo ""; \
		echo "Thank you for registering as an XSB user!"); echo ""

	#make sure .xsb/ exists
	test -d "$HOME/.xsb" || mkdir "$HOME/.xsb"
	date > "$HOME/.xsb/registration"
    fi
fi
