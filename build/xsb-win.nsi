;NSIS install script for windows (& cygwin) XSB.
;   requires MakeNSISW v2.0 or greater <nsis.sourceforge.net>
;
;This script will install XSB by default in the $PROGRAMFILES\XSB directory. It
;also gives an option to install different XSB packages and xsb executable compiled
;with gcc and ming-gw (requires cygwin). It also adds shortcuts to the start menu
;and desktop. Uninstall is also supported.
;
;$Id$
;--------------------------------
SetCompressor zlib

;Include Modern UI
!include "MUI.nsh"

!define VERSION "2.7.1"
!define RELEASE_NAME "Kinryo"

!define MUI_ABORTWARNING

; The name of the installer
Name "XSB ${VERSION}"
Caption "XSB ${VERSION} (${RELEASE_NAME}) Setup"
Icon "xsb.ico"
; The file to write
OutFile "xsb-${VERSION}.win.installer.exe"

; The default installation directory
InstallDir $PROGRAMFILES\XSB

; Registry key to check for directory (so if you install again, it will
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\XSB" "Install_Dir"

ShowInstDetails show
AutoCloseWindow false
WindowIcon off
XPStyle on
InstProgressFlags smooth


Var "PdfLink"
Var "CygLink"
;--------------------------------

; Pages
  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE "..\LICENSE"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  !insertmacro MUI_UNPAGE_FINISH

;--------------------------------
;Languages

  !insertmacro MUI_LANGUAGE "English"
;--------------------------------

; The stuff to install
Section "!XSB (required)" SecCore
	StrCpy $PdfLink "false"
	StrCpy $CygLink "false"

	SectionIn RO

	SetDetailsPrint textonly
	DetailPrint "Installing XSB..."
	SetDetailsPrint listonly

	; Remove old shortcuts, if any
	Delete "$SMPROGRAMS\XSB Prolog\Documentation\*.*"
	RMDir "$SMPROGRAMS\XSB Prolog\Documentation"
	Delete "$SMPROGRAMS\XSB Prolog\Support\*.*"
	RMDir "$SMPROGRAMS\XSB Prolog\Support"
	Delete "$SMPROGRAMS\XSB Prolog\*.*"
	RMDir "$SMPROGRAMS\XSB Prolog"

	; Put file there
	CreateDirectory $INSTDIR\bin
	CreateDirectory $INSTDIR\config
	CreateDirectory $INSTDIR\packages

	; Set output path to the installation directory.
	SetOutPath $INSTDIR
	File /r /x .cvsignore /x CVS /x *.exe  "..\build"
	File /r /x .cvsignore /x CVS "..\cmplib"
	File /r /x .cvsignore /x CVS "..\emu"
	File /r /x .cvsignore /x CVS "..\etc"
	File /r /x .cvsignore /x CVS "..\gpp"
	File /r /x .cvsignore /x CVS "..\lib"
	File /r /x .cvsignore /x CVS "..\misc"
	File /r /x .cvsignore /x CVS "..\prolog_includes"
	File /r /x .cvsignore /x CVS "..\syslib"
	File "xsb.ico"
	File "..\FAQ"
	File "..\INSTALL"
	File "..\INSTALL_PROBLEMS"
	File "..\INSTALL_WINDOWS"
	File "..\LICENSE"
	File "..\Makefile"
	File "..\README"

	;copy windows executable
	SetOutPath $INSTDIR\config
	File /r "..\config\x86-pc-windows"

	;config/i686-pc-cygwin/lib gets copied because of some symbolic link
	;remove this if distributing cygwin binaires.
	RMDir /r $INSTDIR\config\i686-pc-cygwin

	;copy readme for packages, minimal packages install
	SetOutPath $INSTDIR\packages
	File "..\packages\Makefile"
	File "..\packages\README"

	;flip and flora are copied because of some symbolic link, remove them here.
	RMDir /r $INSTDIR\packages\flip
	RMDIR /r $INSTDIR\packages\flora

	;add batch file to run windows xsb
	SetOutPath $INSTDIR\bin
	File "..\bin\wxsb.bat"

	; Write the installation path into the registry
	WriteRegStr HKLM SOFTWARE\XSB "Install_Dir" "$INSTDIR"

	; Write the uninstall keys for Windows
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\XSB" "DisplayName" "XSB Prolog"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\XSB" "UninstallString" '"$INSTDIR\uninstall.exe"'
	WriteUninstaller "uninstall.exe"

SectionEnd

; Section /o "XSB for Cygwin" SecCygwin
	; SetDetailsPrint textonly
	; DetailPrint "Installing XSB for Cygwin..."
	; SetDetailsPrint listonly
	;
	; SetOutPath $INSTDIR\config
	; File /r "..\config\i686-pc-cygwin"
	; SetOutPath $INSTDIR\bin
	; File "..\bin\xsb"
	;
	; StrCpy $CygLink "true"
; SectionEnd

Section "Examples" SecExamples
	CreateDirectory $INSTDIR\examples
	SetOutPath $INSTDIR\examples

	SetDetailsPrint textonly
	DetailPrint "Installing Examples..."
	SetDetailsPrint listonly

	File /r /x .cvsignore /x CVS "..\examples"
SectionEnd

Section "Documentation" SecDocs
	CreateDirectory $INSTDIR\docs
	SetOutPath $INSTDIR\docs

	SetDetailsPrint textonly
	DetailPrint "Installing Documentation..."
	SetDetailsPrint listonly

	File "..\docs\manual1.pdf"
	File "..\docs\manual2.pdf"

	StrCpy $PdfLink "true"
	;File /r /x .cvsignore /x CVS "docs\techman"
	;File /r /x .cvsignore /x CVS "docs\userman"

	;;for some reason these get copied when copying documenation, must be a sym link somewhere!!!!!
	;RMDir /r $INSTDIR\packages\flip
	;RMDir /r $INSTDIR\packages\flora
	;RMDir /r $INSTDIR\packages\iprolog
SectionEnd

SectionGroup /e "Packages"
Section "CDF" SecCdf
	SetDetailsPrint textonly
	DetailPrint "Installing CDF Package..."
	SetDetailsPrint listonly

	SetOutPath $INSTDIR\packages
	File /r /x .cvsignore /x CVS "..\packages\altCDF"
	File /r /x .cvsignore /x CVS "..\packages\CDF"
	File "..\packages\altcdf.P"
	File "..\packages\cdf.P"
SectionEnd

Section /o "CHR" SecChr
	SetDetailsPrint textonly
	DetailPrint "Installing CHR Package..."
	SetDetailsPrint listonly

	SetOutPath $INSTDIR\packages
	File "..\packages\chr.P"
	File /r /x .cvsignore /x CVS "..\packages\chr"
	SetOutPath $INSTDIR\bin
	File "..\bin\chr_pp"
SectionEnd

Section /o "DbDrivers" SecDbDrivers
	SetDetailsPrint textonly
	DetailPrint "Installing DbDrivers Package..."
	SetDetailsPrint listonly

	SetOutPath $INSTDIR\packages
	File "..\packages\dbdrivers.P"
	File /r /x .cvsignore /x CVS "..\packages\dbdrivers"
SectionEnd

Section "FLORA" SecFlora
	SetDetailsPrint textonly
	DetailPrint "Installing Flora Package..."
	SetDetailsPrint listonly

	SetOutPath $INSTDIR\packages
	File /r /x .cvsignore /x CVS "..\packages\flora2"
SectionEnd

Section /o "Gap" SecGap
	SetDetailsPrint textonly
	DetailPrint "Installing Gap Package..."
	SetDetailsPrint listonly

	SetOutPath $INSTDIR\packages
	File "..\packages\gap.P"
	File /r /x .cvsignore /x CVS "..\packages\gap"
SectionEnd

Section /o "libwww" SecLibwww
	SetDetailsPrint textonly
	DetailPrint "Installing Libwww Package..."
	SetDetailsPrint listonly

	SetOutPath $INSTDIR\packages
	File "..\packages\libwww.P"
	File /r /x .cvsignore /x CVS "..\packages\libwww"
SectionEnd

Section /o "perlmatch" SecPerlMatch
	SetDetailsPrint textonly
	DetailPrint "Installing PerlMatch Package..."
	SetDetailsPrint listonly

	SetOutPath $INSTDIR\packages
	File "..\packages\perlmatch.P"
	File /r /x .cvsignore /x CVS "..\packages\perlmatch"
SectionEnd

Section /o "regmatch" SecRegMatch
	SetDetailsPrint textonly
	DetailPrint "Installing RegMatch Package..."
	SetDetailsPrint listonly

	SetOutPath $INSTDIR\packages
	File "..\packages\regmatch.P"
	File /r /x .cvsignore /x CVS "..\packages\regmatch"
SectionEnd

Section /o "slx" SecSlx
	SetDetailsPrint textonly
	DetailPrint "Installing SLX Package..."
	SetDetailsPrint listonly

	SetOutPath $INSTDIR\packages
	File "..\packages\slx.P"
	File /r /x .cvsignore /x CVS "..\packages\slx"
SectionEnd

Section /o "w4" SecW4
	SetDetailsPrint textonly
	DetailPrint "Installing W4 Package..."
	SetDetailsPrint listonly

	SetOutPath $INSTDIR\packages
	File /r /x .cvsignore /x CVS "..\packages\w4"
SectionEnd

Section /o "wildmatch" SecWildMatch
	SetDetailsPrint textonly
	DetailPrint "Installing WildMatch Package..."
	SetDetailsPrint listonly

	SetOutPath $INSTDIR\packages
	File "..\packages\wildmatch.P"
	File /r /x .cvsignore /x CVS "..\packages\wildmatch"
SectionEnd

Section /o "xasp" SecXasp
	SetDetailsPrint textonly
	DetailPrint "Installing XASP Package..."
	SetDetailsPrint listonly

	SetOutPath $INSTDIR\packages
	File "..\packages\xasp.P"
	File /r /x .cvsignore /x CVS "..\packages\xasp"
SectionEnd

Section "xsbdoc" SecXsbDoc
	SetDetailsPrint textonly
	DetailPrint "Installing XSBDoc Package..."
	SetDetailsPrint listonly

	SetOutPath $INSTDIR\packages
	File "..\packages\xsbdoc.P"
	File /r /x .cvsignore /x CVS "..\packages\xsbdoc"
SectionEnd
SectionGroupEnd

; Optional section (can be disabled by the user)
Section "Start Menu Shortcuts" SecShortcuts
	SetDetailsPrint textonly
	DetailPrint "Adding shortcuts..."
	SetDetailsPrint listonly

	;create folders
	CreateDirectory "$SMPROGRAMS\XSB Prolog"
	CreateDirectory "$SMPROGRAMS\XSB Prolog\Documentation"
	CreateDirectory "$SMPROGRAMS\XSB Prolog\Support"

	;main folder
	CreateShortCut "$SMPROGRAMS\XSB Prolog\Uninstall XSB.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
	CreateShortCut "$SMPROGRAMS\XSB Prolog\XSB.lnk" "$INSTDIR\config\x86-pc-windows\bin\xsb.exe" " " "$INSTDIR\xsb.ico"
	WriteINIStr "$SMPROGRAMS\XSB Prolog\XSB Site.url" "InternetShortcut" "URL" "http://xsb.sourceforge.net/"

	StrCmp $CygLink "true" add_cyg_link support
add_cyg_link:
	CreateShortCut "$SMPROGRAMS\XSB Prolog\XSB (Cygwin).lnk" "$INSTDIR\config\i686-pc-cygwin\bin\xsb.exe" " " "$INSTDIR\xsb.ico"
support:
	;support folder
	WriteINIStr "$SMPROGRAMS\XSB Prolog\Support\Mailing Lists.url" "InternetShortcut" "URL" "http://sourceforge.net/mail/?group_id=1176"
	WriteINIStr "$SMPROGRAMS\XSB Prolog\Support\Bug Tracking.url" "InternetShortcut" "URL" "http://sourceforge.net/tracker/?group_id=1176&atid=101176"

	;documentation folder
	StrCmp $PdfLink "true" add_pdf_link add_web_link
add_pdf_link:
	CreateShortCut "$SMPROGRAMS\XSB Prolog\Documentation\User Manual Volume 1.lnk" "$INSTDIR\docs\manual1.pdf"
	CreateShortCut "$SMPROGRAMS\XSB Prolog\Documentation\User Manual Volume 2.lnk" "$INSTDIR\docs\manual2.pdf"
	Goto done
add_web_link:
	WriteINIStr "$SMPROGRAMS\XSB Prolog\Documentation\User Manual Volume 1.url" "InternetShortcut" "URL" "http://xsb.sourceforge.net/manual1/index.html"
	WriteINIStr "$SMPROGRAMS\XSB Prolog\Documentation\User Manual Volume 2.url" "InternetShortcut" "URL" "http://xsb.sourceforge.net/manual2/index.html"
done:
	WriteINIStr "$SMPROGRAMS\XSB Prolog\Documentation\Release Notes.url" "InternetShortcut" "URL" "http://xsb.sourceforge.net/rel_notes.html"

	;desktop shortcut
	CreateShortCut "$DESKTOP\XSB.lnk" "$INSTDIR\config\x86-pc-windows\bin\xsb.exe" " " "$INSTDIR\xsb.ico"
SectionEnd

;--------------------------------

; Uninstaller

Section "Uninstall"

	; Remove registry keys
	DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\XSB"
	DeleteRegKey HKLM SOFTWARE\XSB

	; Remove shortcuts, if any
	Delete "$SMPROGRAMS\XSB Prolog\Documentation\*.*"
	Delete "$SMPROGRAMS\XSB Prolog\Support\*.*"
	Delete "$SMPROGRAMS\XSB Prolog\*.*"
	Delete "$DESKTOP\XSB.lnk"

	; Remove directories used
	RMDir "$SMPROGRAMS\XSB Prolog\Documentation"
	RMDir "$SMPROGRAMS\XSB Prolog\Support"
	RMDir "$SMPROGRAMS\XSB Prolog"

	;should not delete this directory because user might override install directory to say C:\
	;RMDir /r "$INSTDIR"

SectionEnd

;--------------------------------
;Descriptions

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCore}          "The core files required to use XSB"
  ;!insertmacro MUI_DESCRIPTION_TEXT ${SecCygwin}        "XSB compiled using gcc and ming-gw (requires cygwin)."
  !insertmacro MUI_DESCRIPTION_TEXT ${SecExamples}      "Example XSB Programs"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecDocs}          "XSB user manual (requires Adobe Acrobat)."
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCdf}           "Coherent Data Format for compiling ontology information into prolog."
  !insertmacro MUI_DESCRIPTION_TEXT ${SecChr}           "Constraint Handling Rules"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecDbDrivers}     "XSB-Db interface, with support for mysql. Run XSB/packages/dbdrivers/run_configuration_scripts to configure."
  !insertmacro MUI_DESCRIPTION_TEXT ${SecFlora}         "FLORA-2 is a sophisticated object-oriented knowledge base language and application development environment."
  !insertmacro MUI_DESCRIPTION_TEXT ${SecGap}           "GAP package"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecLibwww}        "XSB web interface"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecPerlMatch}     "XSB perl interface (requires perl)."
  !insertmacro MUI_DESCRIPTION_TEXT ${SecRegMatch}      "XSB regular expression interface (requires regexp package). "
  !insertmacro MUI_DESCRIPTION_TEXT ${SecSlx}           "SLX preprocessor of extended logic programs, under WFSX semantics, into equivalent normal programs for XSB"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecW4}            "XML parser for XSB."
  !insertmacro MUI_DESCRIPTION_TEXT ${SecWildMatch}     "XSB wildcard matcher"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecXasp}          "Answer Set Programming with XSB."
  !insertmacro MUI_DESCRIPTION_TEXT ${SecXsbDoc}        "The xsbdoc Documentation Generator."
  !insertmacro MUI_DESCRIPTION_TEXT ${SecShortcuts}     "Adds icons to your start menu and your desktop for easy access"
!insertmacro MUI_FUNCTION_DESCRIPTION_END
