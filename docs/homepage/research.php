<!doctype html public "-//w3c//dtd html 4.0 transitional//en">
<html>
<head>
   <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
   <meta name="GENERATOR" content="Mozilla/4.61 [en] (X11; I; Linux 2.2.12-32 i686) [Netscape]">
   <title>The XSB Research Group</title>
</head>
<body>
<img SRC="new.gif" ALT="" IMG  align=TOP><font color="#FF0000">XSB
2.3</font> has now been released!
<p>
<hr>
<h2>
The XSB Research Group</h2>
The focus of the XSB research group is the development and application
of the XSB system, an open source logic programming system that extends
Prolog with new semantic and operational features, mostly based on the
use of Tabled Logic Programming or <i>tabling.</i>
<p>At first, tabling may seem like a simple idea. At a very high level,
during computation of a goal to a logic program, each subgoal&nbsp; <i>S</i>
is registered in a table the first time it is called, and unique answers
to <i>S </i>are added to the table as they are derived. When subsequent
calls are made to<i> S</i>, the evaluation ensures that answers to <i>S
</i>are
read from the table rather than being rederived using program clauses.
Even from this simple description, a first advantage of tabling can be
seen, that it provides termination to various classes of programs. Consider
the case of ground positive datalog programs ---i .e.Horn Clause programs
that allow only constant terms over a finite alphabet. Such a program contains
only finitely many ground atoms. Each of these atoms can be added at most
once to the table as a subgoal, and each such subgoal can have at most
one answer, leading to a finite computation. Tabled logic programming for
Horn clause programs was first formalized in the early 1980's. Additionally,
several formalisms and systems have been based both on tabled resolution
and on magic sets, which can also be seen as a form of tabled logic programming.
<p>However, tabling can be used to far greater effect than ensuring termination
for Horn clause programs. Seen abstractly, the table described above represents
selected global elements of a given state of a computation, in this case,
subgoals called and their answers so far derived. One powerful feature
of tabling is its ability to maintain other global elements of a computation
in the ``table,'' such as information about whether one subgoal depends
on another, and whether the dependency is through negation. By maintaining
this global information, tabling can be used to evaluate normal logic programs
under the <i>Well-Founded Semantics (WFS).</i> While computation of the
well-founded semantics using tabling is quite complex, the essential idea
is that global information about dependencies is used to determine the
truth value of literals that do not have a derivation. If such literals
are involved in a cyclic dependency through negation, they are undefined
under WFS; if not, the literals belong to an unfounded setand are false
in WFS. In fact, it can be shown that tabling allows non-floundering datalog
programs with negation to terminate with polynomial data complexity under
the well-founded semantics. A second feature to extend the power of tabling
is called <i>tabled aggregation</i>. If <i>partial order tabled aggregation</i>
is used, relations can be defined on answers in a table such that only
those which are optimal with respect to the partial order need be retained.
If <i>functional tabled aggregation</i>} is used, functions can be defined
on answers to a subgoal in a table so that an associative function~---~say
a maximum or least upper bound~---~of the answers is retained rather than
the set of answers themselves.
<p>Using these features, XSB can serve as a development platform for applications
in two ways. First, applications can be programmed using normal program
clauses and the libraries of XSB. Second, XSB can serve as the implementational
infrastructure for useful logics which then serve as the programming language
for applications. These two choices can be thought of as extremes, and
most applications so far have used a mixture of the two approaches.
<p>We consider first the logics that have been implemented in XSB. Based
on XSB's implementation of WFS, the Well-Founded Semantics with Explicit
Negation (WFSX) has been implemented and extended to allow abduction and
logical program updates; the object-based Frame Logic (F-Logic) and several
temporal concurrency logics including the Modal <b><i><u>mu</u></i></b>-Calculus
and Concurrent Temporal Logic have also been implemented using WFS, as
described below. Using functional tabled aggregation, Generalized Annotated
Programs and a type of probabilistic logic programs have been implemented;
while using partial order tabled aggregation a species of Preference Logic
has been implemented. Many of these logics will become more powerful with
the recent addition of logical constraint handling techniques to XSB.
<p>Before turning to applications of these logics we consider in detail
how XSB can form the underpinnings of a system based on a non-traditional
logical formalism using the case of FLORA, which which amalgamates F-logic,
Transaction Logic, and HiLog, and is implemented as an optimizing compiler
whose target code is a normal program executable by XSB. Consider an F-logic
term:
<ul>
<li>
<b>john[spouse->mary, child->>{bob,bill}]</b></li>
</ul>
which indicates that the object&nbsp; <b>john </b>has a spouse-attribute
of <b>mary</b> and chidren-attributes<b> bob</b> and <b>bill</b>. This
term is translated into a conjunction of triples:
<ul>
<li>
<b>fd(john,spouse,mary)</b></li>

<li>
<b>mvd(john,child,bob)</b></li>

<li>
<b>mvd(john,child,bill)</b></li>
</ul>
While this plan seems straightforward, it is hard to realize in a standard
Prolog framework. The main reason is that the translation into XSB uses
only a small number of predicates, which leads to two main problems: (1)
The loss of indexing; and (2) Termination problems.
<p>The first problem cannot be easily solved by increasing the number of
predicates, because of the meta-programming features of F-logic. However,
FLORA takes advantage of the various optimization techniques that exist
in XSB, such as <i>specialization</i> and <i>unification factoring</i>.
The second problem is more serious: the small number of predicates used
in the translation makes even non-recursive FLORA programs into highly
recursive XSB programs. In this situation it is hard to build a compiler
that would produce programs with reasonable termination behavior without
tabling.
<p>The profusion of logics implementable in XSB has led to a proliferation
of research and commercial applications, many of them in areas in which
logic programming has not previously been successful. Using XSB, WFSX has
been used for machine learning. With the addition of abduction, WFSX has
been used for psychiatric diagnosis, as well as for fault diagnosis in
electronic circuits. FLORA has been used for the creation of Web agents,
for applications in neuroscience, and for processing ontologies and meta-data.
Probabilistic logic programs have been used to allow mined association
rules to be used within an intensional database. Preference Logic Programs
have been used to for data cleaning through the formalism of Preference
Logic Grammars. These applications typically mix the use of the various
logics with traditional Prolog programming techniques. The reasons for
the success of each application varies. However, the use of XSB for the
verification of concurrent systems, or <i>model-checking</i> illustrates
many common features of these applications.
<p>Model checkers are usually formulated in two stages. First there is
a <i>process logic</i>, representing the interaction of communicating systems.
Common process logics are Milner's CCS (Calculus for Communicating Systems)
and the \pi-calculus. Abstractly, a process logic can be seen as a labelled
transition system consisting of a set <i>S</i> of global system states
and a labelled relation among elements of&nbsp; <i>S </i>representing communications
among the systems or changes in the internal state of these systems. Transition
systems generated by process logics contain loops and so require a mechanism
like tabling even to be traversed. Temporal concurrency logics, such as
the Modal- \mu-calculus and CTL* have been designed in order to be able
to query interesting properties of transition systems such as whether termination
occurs along all paths starting from a given state, whether such paths
are free from deadlock, are fair, and so on. It can be shown that many
concurrent temporal logics --- such as the non-alternating modal \mu-calculus
--- can be embedded into <i>dynamically</i> <i>stratified</i> logic programs,
and can be evaluated directly in XSB producing a two-valued well-founded
model. It is unknown whether general concurrent temporal logics, (such
as the alternating modal \mu-calculus) can be modularly embedded into the
well-founded semantics, but such logics have been evaluated by using XSB
as a preprocessor to a stable model generator. Thus in model checking,
XSB may be used alone or with other tools depending on the needs of the
user. Given its power and applicability, it has not been trivial to implement
tabling so that it is both efficient and integrable with Prolog. Much of
the work of the XSB group has involved design and implementation of its
underlying engine, the SLG-WAM, and its extensions. From the point of view
of implementation, tabling has necessitated the development of algorithms
in a variety of areas. During a tabled evaluation, one computation path
may consume answers produced by another computation path. This means that
resources must be retained for environments that consume answers to a given
subgoal until they have consumed all answer that the evaluation will produce
for that subgoal. Furthermore, the evaluation must be able to switch efficiently
between computation paths that produce answers and those that consume them.
These differences between tabled and non-tabled evaluation have led to
the study of strategies to schedule the return of answers to consuming
environments; the study of how to efficiently determine when mutually recursive
sets of subgoals have been completely evaluated; and of how to perform
efficient memory management, environment switching and garbage collection.
Much work has also been done on how to efficiently access tables from a
WAM-style engine, how to maintain dependency information in the table (and
stacks) for the well-founded semantics, and how answers in a table can
be efficiently shared among subsuming calls.
<p>Descriptions of tabling algorithms their implementations; of implemented
logics, their applications, and implementations can be found via
<ul>
<li>
<a href="xsb-people.html">List of people</a></li>
</ul>

<p><br><a href="http://sourceforge.net"><img SRC="sflogo.php" ALT="SourceForge Logo" BORDER=0 height=31 width=88></a>
</body>
</html>
