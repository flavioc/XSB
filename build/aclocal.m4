dnl This file contains some standard Autoconf macros, which were adapted by
dnl kifer for building XSB.

dnl Redefine  AC_INIT_PARSE_ARGS() to customize for our needs!
define([AC_INIT_PARSE_ARGS], [
# Initialize some variables set by options.
# The variables have the same names as the options, with
# dashes changed to underlines.
build=NONE
host=NONE
no_create=
nonopt=NONE
configuration=NONE
no_recursion=
prefix=NONE
silent=
srcdir=
target=NONE
verbose=
config_tag=NONE
dnl Installation directory options.
dnl These are left unexpanded so users can "make install site_prefix=/foo"
dnl and all the variables that are supposed to be based on exec_prefix
dnl by default will actually change.
dnl Use braces instead of parens because sh, perl, etc. also accept them.
site_prefix=NONE
mandir=NONE

# Initialize some other variables.
subdirs=
MFLAGS= MAKEFLAGS=
# Maximum number of lines to put in a shell here document.
ac_max_here_lines=12

ac_prev=
ac_caching=
for ac_option
do

  # If the previous option needs an argument, assign it.
  if test -n "$ac_prev"; then
    eval "$ac_prev=\$ac_option"
    ac_prev=
    continue
  fi


  case "$ac_option" in
changequote(, )dnl
  -*=*) ac_optarg=`echo "$ac_option" | sed 's/[-_a-zA-Z0-9]*=//'` ;;
changequote([, ])dnl
  *) ac_optarg= ;;
  esac


  # Accept the important Cygnus configure options, so we can diagnose typos.

  case "$ac_option" in

  -cache | --cache | --cach | --cac | --ca)
    ac_caching=yes ;;

  -force64 | --force64 | --64)
    ac_force64=yes ;;

  -linuxaout | --linuxaout | --aout)
    ac_linuxaout=yes ;;

  -disable-* | --disable-*)
    ac_feature=`echo $ac_option|sed -e 's/-*disable-//'`
    # Reject names that are not valid shell variable names.
changequote(, )dnl
    if test -n "`echo $ac_feature| sed 's/[-a-zA-Z0-9_]//g'`"; then
changequote([, ])dnl
      AC_MSG_ERROR($ac_feature: invalid feature name)
    fi
    ac_feature=`echo $ac_feature| sed 's/-/_/g'`
    eval "enable_${ac_feature}=no" ;;

  -enable-* | --enable-*)
    ac_feature=`echo $ac_option|sed -e 's/-*enable-//' -e 's/=.*//'`
    # Reject names that are not valid shell variable names.
changequote(, )dnl
    if test -n "`echo $ac_feature| sed 's/[-_a-zA-Z0-9]//g'`"; then
changequote([, ])dnl
      AC_MSG_ERROR($ac_feature: invalid feature name)
    fi
    ac_feature=`echo $ac_feature| sed 's/-/_/g'`
    case "$ac_option" in
      *=*) ;;
      *) ac_optarg=yes ;;
    esac
    eval "enable_${ac_feature}='$ac_optarg'" ;;


  -help | --help | --hel | --he)
    # Omit some internal or obsolete options to make the list less imposing.
    # This message is too long to be a string in the A/UX 3.1 sh.
    cat << EOF
changequote(, )dnl
Usage: configure [options] [host]
Options: [defaults in brackets after descriptions]

Control:
  --help                  Print this message
  --cache                 use cached test results from a previous run
                          of \`configure', if available
  --force64               On 64 bit machines that default to the 32 bit mode,
                          force the 64 bit mode.

  --linuxaout             If you are using an older Linux system that
                          utilizez the a.out format, you must tell us, or
                          else XSB won't be configured correctly.

  --optimization-level=level
                          You can override the default optimization settings by
                          specifying something like O2 (or xO2, for some
                          compilers).

  --no-create             Do not create output files
  --quiet, --silent       Do not print \`checking...' messages
  --version               Print the version of autoconf that created configure

Directory and file names:
  --prefix=PREFIX         Install architecture-independent files in PREFIX
                          [$ac_default_prefix]
  --site-prefix=DIR       Site-specific XSB libraries in DIR [PREFIX/site]

  --site-static-libraries=DIR
                          These might be needed, if compiling with support 
                          for statically linked packages (such as Oracle)
                          or if your standard C libraries are in 
                          odd places. These libraries are added as -L options 
                          during XSB compilation.
                          You can specify a list of libraries by enclosing 
                          them in quotes. This list is automatically added 
                          to the loader flags to ensure that the packages will
                          be linked in.
  --site-dynamic-libraries=DIR
                          This specifies the list of libraries used by 
                          packages linked dynamically with XSB.
                          These libraries are automatically added to the XSB 
                          library search path, so they could be loaded at
                          run time.
  --site-includes=DIR     These might be needed if it is necessary to tell 
                          the compiler about additional header files to
                          include with the -I option. This need arises if
                          your standard header files are in odd places or
                          if XSB is compiled with ODBC support.
                          You can specify a list of libraries by enclosing 
                          them in quotes.

  --config-tag=TAG        Makes the configuration directory name look like
                          [CONFIG_PREFIX]/configuration-TAG. If TAG is missing,
                          the directory is [CONFIG_PREFIX]/[configuration].
                          TAG is used for debugging, so one can have
                          different builds under the same architecture.
EOF
    cat << EOF

Host type:
  --host=HOST             Configure for HOST [guessed]

Features and packages:
  --disable-FEATURE       Do not include FEATURE (same as --enable-FEATURE=no)
  --enable-FEATURE[=ARG]  Include FEATURE [ARG=yes]
  --with-PACKAGE[=ARG]    Use PACKAGE [ARG=yes]
  --without-PACKAGE       Do not use PACKAGE (same as --with-PACKAGE=no)

changequote([, ])dnl
EOF
    if test -n "$ac_help"; then
      echo "--enable and --with options recognized:$ac_help"
    fi
    exit 0 ;;

  -host | --host | --hos | --ho)
    ac_prev=host ;;
  -host=* | --host=* | --hos=* | --ho=*)
    host="$ac_optarg" ;;

  -mandir | --mandir | --mandi | --mand | --man | --ma | --m)
    ac_prev=mandir ;;
  -mandir=* | --mandir=* | --mandi=* | --mand=* | --man=* | --ma=* | --m=*)
    mandir="$ac_optarg" ;;

  -site-prefix | --site-prefix | --site-prefi | --site-pref \
          | --site-pre | --site-pr | --site-p)
    ac_prev=site_prefix ;;
  -site-prefix=* | --site-prefix=* | --site-prefi=* | --site-pref=* \
          | --site-pre=* | --site-pr=* | --site-p=*)
    site_prefix="$ac_optarg" ;;

  -config-tag | --config-tag | --config-ta | --config-t)
    ac_prev=config_tag ;;
  -config-tag=* | --config-tag=* | --config-ta=* | --config-t=*)
changequote(, )dnl
    if test -n "`echo $ac_optarg| sed 's/[-a-zA-Z0-9]//g'`"; then
changequote([, ])dnl
      AC_MSG_WARN($ac_optarg: config-tag must be alphanumeric with optional dashes)
    fi
    config_tag="$ac_optarg" ;;

  -no-create | --no-create | --no-creat | --no-crea | --no-cre \
  | --no-cr | --no-c)
    no_create=yes ;;

  -no-recursion | --no-recursion | --no-recursio | --no-recursi \
  | --no-recurs | --no-recur | --no-recu | --no-rec | --no-re | --no-r)
    no_recursion=yes ;;

  -optimization-level | --optimization-level | --opt)
    ac_prev=optimization_level ;;
  -optimization-level=* | --optimization-level=* | --opt*=*)
    optimization_level="$ac_optarg" ;;

  -prefix | --prefix | --prefi | --pref | --pre | --pr | --p)
    ac_prev=prefix ;;
  -prefix=* | --prefix=* | --prefi=* | --pref=* | --pre=* | --pr=* | --p=*)
    prefix="$ac_optarg" ;;

  -q | -quiet | --quiet | --quie | --qui | --qu | --q \
  | -silent | --silent | --silen | --sile | --sil)
    silent=yes ;;

  -site-includes | --site-includes | --site-include | --site-includ \
   | --site-inclu | --site-incl  | --site-inc | --site-in )
    ac_prev=site_includes ;;
  -site-includes=* | --site-includes=* | --site-include=* | --site-includ=* \
   | --site-inclu=* | --site-incl=*  | --site-inc=* | --site-in=* )
    site_includes="$ac_optarg" ;;

  -site-dynamic-libraries | --site-dynamic-libraries | --site-dynamic \
     | --site-dyn )
    ac_prev=site_dynamic_libraries ;;
  -site-dynamic*=* | --site-dyn*=* )
    site_dynamic_libraries="$ac_optarg" ;;

  -site-static-libraries | --site-static-libraries | --site-static \
     | --site-stat )
    ac_prev=site_static_libraries ;;
  -site-static*=* | --site-stat*=* )
    site_static_libraries="$ac_optarg" ;;

  -v | -verbose | --verbose | --verbos | --verbo | --verb)
    verbose=yes ;;

  -version | --version | --versio | --versi | --vers)
    echo "configure generated by autoconf version AC_ACVERSION"
    exit 0 ;;

  -with-* | --with-*)
    ac_package=`echo $ac_option|sed -e 's/-*with-//' -e 's/=.*//'`
    # Reject names that are not valid shell variable names.
changequote(, )dnl
    if test -n "`echo $ac_package| sed 's/[-_a-zA-Z0-9]//g'`"; then
changequote([, ])dnl
      AC_MSG_ERROR($ac_package: invalid package name)
    fi
    ac_package=`echo $ac_package| sed 's/-/_/g'`
    case "$ac_option" in
      *=*) ;;
      *) ac_optarg=yes ;;
    esac
    eval "with_${ac_package}='$ac_optarg'" ;;

  -without-* | --without-*)
    ac_package=`echo $ac_option|sed -e 's/-*without-//'`
    # Reject names that are not valid shell variable names.
changequote(, )dnl
    if test -n "`echo $ac_package| sed 's/[-a-zA-Z0-9_]//g'`"; then
changequote([, ])dnl
      AC_MSG_ERROR($ac_package: invalid package name)
    fi
    ac_package=`echo $ac_package| sed 's/-/_/g'`
    eval "with_${ac_package}=no" ;;

  -*) AC_MSG_ERROR([$ac_option: invalid option; use --help to show usage])
    ;;

  *)
changequote(, )dnl
    if test -n "`echo $ac_option| sed 's/[-a-z0-9.]//g'`"; then
changequote([, ])dnl
      AC_MSG_WARN($ac_option: invalid host type)
    fi
    if test "x$nonopt" != xNONE; then
      AC_MSG_ERROR(can only configure for one host and one target at a time)
    fi
    nonopt="$ac_option"
    configuration=$nonopt
    ;;

  esac
done

if test -n "$ac_prev"; then
  AC_MSG_ERROR(missing argument to --`echo $ac_prev | sed 's/_/-/g'`)
fi
])

dnl AC_CACHE_LOAD()
define([AC_CACHE_LOAD], [ ])

