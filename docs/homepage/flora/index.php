<html><head><title>Flora's Home Page </title></head>

<body background=/images/BGnds/paper_yellow.gif>
<a name="top">
<h2>
<pre>
<!--
<img align=middle src="images/flora.gif" width=180 height=130>
--->
</pre>
</h2>
<p>
<HR>

<p>
This page is in alpha state. Please excuse our appearance.

<p>
FLORA-2 is a sophisticated deductive object-oriented database system and
application development platform. It is implemented as a compiler that
translates a unified language of F-logic, HiLog, and Transaction Logic into
<a href="http://xsb.sourceforge.net/">
the XSB deductive engine</a>.

The programming language supported by FLORA-2 is a dialect of F-logic with
numerous extensions. Some extensions, such as path expressions, are
borrowed from <a
href="http://www.informatik.uni-freiburg.de/~dbis/">FLORID</a>.  However,
FLORA-2 has unique and powerful features not found in other deductive
object-oriented database systems.  First, as already mentioned, in addition
to F-logic, it supports HiLog and logical updates, as in Transaction Logic.
Second, FLORA-2 was designed with extensibility and flexibility in mind.
The operator grammar is carefully crafted into the language, which allows the
user to extend the syntax with her own idioms.  Third, FLORA-2 provides an
original dynamic module system, which supports modular software design and
turns it into a flexible application development platform.

Applications of FLORA-2 include Intelligent Agents, Semantic Web,
and Ontology management.

<p>
March 11, 2002: We are happy to announce the release of a "late alpha"
version of FLORA-2. This version has significantly improved stability and
several important new features. See 
<a href="../rel_notes.html#flora">release notes</a> for the details.
<p>
Here is the manual:
	<ul>
	  <li> <a href="docs/manual.ps">Postscript version</a>
	  <li> <a href="docs/manual.pdf">PDF version</a>
	</ul>

<p>
Related links:
<ul>
<li> <a href="http://www.informatik.uni-freiburg.de/~dbis/florid/">
    FLORID</a>: A C++-based implementation of F-logic from Freiburg University.
<li> <A href="http://www.dsic.upv.es/~pcarsi/tfl/">Transaction F-logic
  prototype</a> from the University of Valencia, Spain.
<li> <a href="http://www.cs.toronto.edu/~bonner/transaction-logic.html">The Transaction Logic Page</a>
<li> <A href="http://www.cs.sunysb.edu/~kifer/dood/">The DOOD page</a>:
    additional links to information on Deductive and Object-Oriented
    Databases.
</ul>



<HR>
<P>
<?php include "../counter.php";    ?>
<?php $number = CounterImage(); ?>
<IMG SRC="<?php echo($number); ?>" ALIGN="left">
visits since August 10, 2001.
<P>

<hr>
<address>
Email:<a href="mailto:xsb-contact@cs.sunysb.edu">xsb-contact@cs.sunysb.edu</a>
</address>

<hr>
<p>
<!--Don't touch! The date gets updated when you commit changes to this page-->
$Date$

</body>
</html>


