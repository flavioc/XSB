#!/usr/bin/perl

## File:      mailarchive.pl
## Author(s): Jun Wang
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) The Research Foundation of SUNY, 2000
## 
## XSB is free software; you can redistribute it and/or modify it under the
## terms of the GNU Library General Public License as published by the Free
## Software Foundation; either version 2 of the License, or (at your option)
## any later version.
## 
## XSB is distributed in the hope that it will be useful, but WITHOUT ANY
## WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
## more details.
## 
## You should have received a copy of the GNU Library General Public License
## along with XSB; if not, write to the Free Software Foundation,
## Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
##
## $Id$
## 
##

# Invocation: mailarchive.pl [--keep] [MailBox] [SavedMailFile]
# Default MailBox: /var/spool/mail/$ENV{'LOGNAME'}
# Default SavedMailFile: $ENV{'HOME'}/mbox

$True=1;
$False=0;

$Keep=$False;
if(@ARGV[0] =~ /^\-\-keep$/i){		#case insensitive
    $Keep=$True;
    shift(@ARGV);
}
$MailBox=@ARGV[0];
$SavedEmail=@ARGV[1];
# Set defaults, if arguments aren't given
if ($MailBox eq "") {
    $MailBox="/var/spool/mail/$ENV{'LOGNAME'}";
}
if ($SavedEmail eq "") {
    $SavedEmail="$ENV{'HOME'}/mbox";
}

#General Config
$ConfigFile="datafields.cfg";
$InstallationLog="archive.P";
$InstalSubject="\\\[Xsb-installation\\\]";
$InstalTitle="INSTALLATION SUMMARY";


#Reading config file. 
#All the fields will be extracted from email installation log 
#need to be stored in this file, one in each line.
unless (open(CONFFILE, $ConfigFile)) {
	die ("cannot open configure file $ConfigFile\n");
}
     @ConfigInput = <CONFFILE>;
#     print (@ConfigInput);



#Open email file and the new file to save after trimmed the installation log.
unless (open(EMAILFILE, $MailBox)) {
	die ("Cannot open system mailbox: \'$MailBox\'\n");
}
unless (open(NEWEMAILFILE, ">>$SavedEmail")) {
	die ("Cannot open local mailbox: \'$SavedEmail\'\n");
}
unless (open(XSBLOG, ">>$InstallationLog")) {
	die ("Cannot open installation archive: \'$InstallationLog\'\n");
}



#Readin email, one by one.
$LineOrigin=<EMAILFILE>;
#Ignore lines otherthan "From ", which indicate a email, notice the space
while($LineOrigin ne "" && $LineOrigin !~ /^From /){
	$LineOrigin=<EMAILFILE>;
}
$emailcounter=0;
while($LineOrigin ne ""){
   $FirstBlock=$True;
   $linecounter=0;
   $EmailCurrent[$linecounter]=$LineOrigin;
   $LineOrigin=<EMAILFILE>;
   $emailcounter++;
   while($LineOrigin ne "" && $LineOrigin !~ /^From /){
      $linecounter++;
      $EmailCurrent[$linecounter]=$LineOrigin;

      #find the Subject and the From line
      #the Subject and the From line has to be in the first block of email msg
      if($EmailCurrent[$linecounter] eq "\n"){
         $FirstBlock=$False;
      }
      if($FirstBlock==$True){
         if($EmailCurrent[$linecounter] =~ /^Subject: /){
            $SubjectLine = $EmailCurrent[$linecounter];
            $SubjectLine =~ s/^Subject: //;
         }
         if($EmailCurrent[$linecounter] =~ /^From: /){
            $FromLine = $EmailCurrent[$linecounter];
            $FromLine =~ s/^From: //;
         }
      }
      $LineOrigin=<EMAILFILE>;
   }

   # If a XSB installation log is found, this email will not be saved,
   # and the file $InstallationLog will be updated.
   $Found=$False;
   if($SubjectLine =~ /$InstalSubject/){ 
      for($loop=0; $loop<=$linecounter; $loop++){
           if($EmailCurrent[$loop]=~/$InstalTitle/){
              $Found=$True;
           }
      }
   }
   if($Found == $False) { 
      for($loop=0; $loop<=$linecounter; $loop++){
         print NEWEMAILFILE ($EmailCurrent[$loop]);
      }
   }
   else{

      #extract the name and email info.
      if($FromLine =~ /\(|\</){
	  # this addr has both name & email separated by <> or ()
	  $Name = $FromLine;
	  $Name =~ s/[\(\<].*//;
	  $Name =~ s/^[ \t\n]*//; 
	  $Name =~ s/[ \t\n]*$//;
	  $EmailAddress=$FromLine;
	  $EmailAddress=~ s/^[^\(\<]*[\(\<]//;
	  $EmailAddress=~ s/[\)\>].*//;
	  $EmailAddress=~ s/[ \t\n]*$//;
	  $EmailAddress=~ s/^[ \t\n]*//;
	  if($Name=~/\@/){
	      $Temp=$Name;
	      $Name=$EmailAddress;
	      $EmailAddress=$Temp;
	  }
      }
      else{
         $EmailAddress=$FromLine;
         $EmailAddress=~ s/[ \t\n]*$//;
         $EmailAddress=~ s/^[ \t\n]*//;
         $Name = "";
	 if($EmailAddress !~ /\@/){
            $Temp=$Name;
	    $Name=$EmailAddress;
	    $EmailAddress=$Temp;
         }
      }
     print XSBLOG ("installation\(\[\n");
     print XSBLOG ("\t\'name\'\(\'$Name\'\)\,\n");
     print XSBLOG ("\t\'email\'\(\'$EmailAddress\'\)");

     #extract the info from configure file.
     foreach $ConfigInput (@ConfigInput){
	 $ConfigInput =~ s/[\n \t]*$//;
	 $ConfigInput =~ s/^[\n \t]*//;
	 if ($ConfigInput =~ /^\#/ || $ConfigInput eq "") {
	     next;
	 }
	 @ConfigInputWords = split(/[ \t]*!![ \t]*/, $ConfigInput);
	 $ConfigInputTemplate = $ConfigInputWords[0] . ":";
	 $ConfigInputKey = $ConfigInputWords[1];
	 for($loop=0; $loop<=$linecounter; $loop++){
	     if($EmailCurrent[$loop]=~ /$ConfigInputTemplate/){
		 print XSBLOG "\,\n";  # terminate the previous line
		 print XSBLOG ("\t\'$ConfigInputKey\'");
		 $ConfigInputValue = $EmailCurrent[$loop];
		 $ConfigInputValue =~ s/$ConfigInputTemplate//;
		 $ConfigInputValue =~ s/[\n \t]*$//;
		 $ConfigInputValue =~ s/^[\n \t]*//;
		 print XSBLOG ("\(\'$ConfigInputValue\'\)");
	     }
	 }
     }
      print XSBLOG ("\n\t\]\).\n\n");
   }
#   print ("$emailcounter \n");
}
close(CONFFILE);
close(EMAILFILE);
close(NEWEMAILFILE);
close(XSBLOG);

if($Keep != $True){
   `rm -f $MailBox`;
}
