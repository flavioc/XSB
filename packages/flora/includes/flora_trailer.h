%%-*-prolog-*-

#mode standard Prolog

/*****************************************************************************
  closure rules for X::Y
*****************************************************************************/
:- table VAR_FPREFIX(subclass)/2.
:- table VAR_FPREFIX(strict_subclass)/2.

VAR_FPREFIX(subclass)(X,X) :- VAR_FPREFIX(exists)(X).
VAR_FPREFIX(subclass)(X,Y) :- VAR_FPREFIX(strict_subclass)(X,Y).

%% Using flora_subclass can be a major source of inefficiency
%% In most cases, we want to use strict_subclass
%% We table strict_sub to avoid repetition
:- table VAR_FPREFIX(strict_sub)/2.
VAR_FPREFIX(strict_sub)(X,Y) :- 
	ground((X,Y)), !,
	X \= Y, VAR_FPREFIX(sub)(X,Y).
VAR_FPREFIX(strict_sub)(X,Y) :- VAR_FPREFIX(sub)(X,Y), X \= Y.
VAR_FPREFIX(strict_subclass)(X,Y) :- VAR_FPREFIX(strict_sub)(X,Y).

VAR_FPREFIX(strict_subclass)(X,Y) :-
	VAR_FPREFIX(strict_subclass)(X,Z), 
	VAR_FPREFIX(strict_sub)(Z,Y),
	(   X = Y 
	->  STD_FPREFIX(warning_nl)('cyclic subclass hierarchy: %S::%S::%S',
				    args(X,Z,Y))
	;   true
	).


/*****************************************************************************
  closure rules for X:Y, X::Z implies X:Z
*****************************************************************************/
:- table VAR_FPREFIX(isa)/2.

VAR_FPREFIX(isa)(O,C) :-
	VAR_FPREFIX(strict_subclass)(C1,C),
	VAR_FPREFIX(isa)(O,C1).


/*****************************************************************************
  rules for monotonic inheritance of signatures
*****************************************************************************/
:- table VAR_FPREFIX(fs)/3.
:- table VAR_FPREFIX(mvs)/3.
:- table VAR_FPREFIX(ifs)/3.
:- table VAR_FPREFIX(imvs)/3.

%% Instance
VAR_FPREFIX(fs)(O,MethodArgs,R) :-
	VAR_FPREFIX(isa)(O,Class),
	VAR_FPREFIX(fs)(Class,MethodArgs,R).
VAR_FPREFIX(ifs)(O,MethodArgs,R) :-
	VAR_FPREFIX(isa)(O,Class),
	VAR_FPREFIX(ifs)(Class,MethodArgs,R).

VAR_FPREFIX(mvs)(O,MethodArgs,R) :-
	VAR_FPREFIX(isa)(O,Class),
	VAR_FPREFIX(mvs)(Class,MethodArgs,R).
VAR_FPREFIX(imvs)(O,MethodArgs,R) :-
	VAR_FPREFIX(isa)(O,Class),
	VAR_FPREFIX(imvs)(Class,MethodArgs,R).

%% Subclass
VAR_FPREFIX(fs)(Sub,MethodArgs,R) :-
	VAR_FPREFIX(strict_subclass)(Sub,Class),
	VAR_FPREFIX(fs)(Class,MethodArgs,R).
VAR_FPREFIX(ifs)(Sub,MethodArgs,R) :-
	VAR_FPREFIX(strict_subclass)(Sub,Class),
	VAR_FPREFIX(ifs)(Class,MethodArgs,R).

VAR_FPREFIX(mvs)(Sub,MethodArgs,R) :-
	VAR_FPREFIX(strict_subclass)(Sub,Class),
	VAR_FPREFIX(mvs)(Class,MethodArgs,R).
VAR_FPREFIX(imvs)(Sub,MethodArgs,R) :-
	VAR_FPREFIX(strict_subclass)(Sub,Class),
	VAR_FPREFIX(imvs)(Class,MethodArgs,R).


/*****************************************************************************
  rules for nonmonotonic inheritance of behavior
*****************************************************************************/

:- table VAR_FPREFIX(fd)/3.
:- table VAR_FPREFIX(mvd)/3.
:- table VAR_FPREFIX(ifd)/3.
:- table VAR_FPREFIX(imvd)/3.
:- table VAR_FPREFIX(defined_fd)/2.
:- table VAR_FPREFIX(defined_mvd)/2.
:- table VAR_FPREFIX(defined_ifd)/2.
:- table VAR_FPREFIX(defined_imvd)/2.
:- table VAR_FPREFIX(overwritten_fd)/3.
:- table VAR_FPREFIX(overwritten_mvd)/3.
:- table VAR_FPREFIX(overwritten_ifd)/3.
:- table VAR_FPREFIX(overwritten_imvd)/3.
:- table VAR_FPREFIX(conflict_fd)/3.
:- table VAR_FPREFIX(conflict_ifd)/3.
:- table VAR_FPREFIX(conflict_mvd)/3.
:- table VAR_FPREFIX(conflict_imvd)/3.


VAR_FPREFIX(fd)(Object,Method,Value) :-
	%% If Object,Method are ground, check if Object[Method->...]
	%% is defined right away --an optimization
	(ground((Object,Method)) ->
	    tnot(VAR_FPREFIX(defined_fd)(Object,Method)),
	    VAR_FPREFIX(isa)(Object,Class),
	    VAR_FPREFIX(ifd)(Class,Method,Value)
	  ;
	    VAR_FPREFIX(isa)(Object,Class),
	    VAR_FPREFIX(ifd)(Class,Method,Value),
	    STD_FPREFIX(tnot_all)(VAR_FPREFIX(defined_fd)(Object,Method))
	),
	STD_FPREFIX(tnot_all)(VAR_FPREFIX(overwritten_fd)(Object,Class,Method)),
	STD_FPREFIX(tnot_all)(VAR_FPREFIX(conflict_fd)(Object,Class,Method)).

VAR_FPREFIX(defined_fd)(Object,Method) :- VAR_FPREFIX(fd)(Object,Method,_).

VAR_FPREFIX(overwritten_fd)(Object,Class,Method) :-
	VAR_FPREFIX(isa)(Object,SubClass),
	SubClass \= Class,
	VAR_FPREFIX(strict_subclass)(SubClass,Class),
	VAR_FPREFIX(ifd)(SubClass,Method,_).

%% When this is called, Object always belongs to Class
VAR_FPREFIX(conflict_fd)(Object,Class,Method) :-
	VAR_FPREFIX(defined_ifd)(Class1,Method),
	ground((Class1,Class)),
	Class \= Class1,
	VAR_FPREFIX(isa)(Object,Class1),
	tnot(VAR_FPREFIX(strict_subclass)(Class1,Class)),
	tnot(VAR_FPREFIX(strict_subclass)(Class,Class1)).


VAR_FPREFIX(mvd)(Object,Method,Value) :-
	(ground((Object,Method)) ->
	    tnot(VAR_FPREFIX(defined_mvd)(Object,Method)),
	    VAR_FPREFIX(isa)(Object,Class),
	    VAR_FPREFIX(imvd)(Class,Method,Value)
	  ;
	    VAR_FPREFIX(isa)(Object,Class),
	    VAR_FPREFIX(imvd)(Class,Method,Value),
	    STD_FPREFIX(tnot_all)(VAR_FPREFIX(defined_mvd)(Object,Method))
	),
	STD_FPREFIX(tnot_all)(VAR_FPREFIX(overwritten_mvd)(Object,Class,Method)),
	STD_FPREFIX(tnot_all)(VAR_FPREFIX(conflict_mvd)(Object,Class,Method)).

VAR_FPREFIX(defined_mvd)(Object,Method) :-
	VAR_FPREFIX(mvd)(Object,Method,_).

VAR_FPREFIX(overwritten_mvd)(Object,Class,Method) :-
	VAR_FPREFIX(isa)(Object,SubClass),
	VAR_FPREFIX(strict_subclass)(SubClass,Class),
	VAR_FPREFIX(imvd)(SubClass,Method,_).

VAR_FPREFIX(conflict_mvd)(Object,Class,Method) :-
	VAR_FPREFIX(defined_imvd)(Class1,Method),
	ground((Class,Class1)),
	Class \= Class1,
	VAR_FPREFIX(isa)(Object,Class1),
	tnot(VAR_FPREFIX(strict_subclass)(Class1,Class)),
	tnot(VAR_FPREFIX(strict_subclass)(Class,Class1)).


VAR_FPREFIX(ifd)(Class,Method,Value) :-
	(ground((Class,Method)) ->
	    tnot(VAR_FPREFIX(defined_ifd)(Class,Method)),
	    VAR_FPREFIX(strict_subclass)(Class,Super),
	    VAR_FPREFIX(ifd)(Super,Method,Value)
	  ;
	    VAR_FPREFIX(strict_subclass)(Class,Super),
	    VAR_FPREFIX(ifd)(Super,Method,Value),
	    STD_FPREFIX(tnot_all)(VAR_FPREFIX(defined_ifd)(Class,Method))
	),
	STD_FPREFIX(tnot_all)(VAR_FPREFIX(overwritten_ifd)(Class,Super,Method)),
	STD_FPREFIX(tnot_all)(VAR_FPREFIX(conflict_ifd)(Class,Super,Method)).

VAR_FPREFIX(defined_ifd)(Class,Method) :- VAR_FPREFIX(ifd)(Class,Method,_).

VAR_FPREFIX(overwritten_ifd)(Class,Super,Method) :-
	VAR_FPREFIX(strict_subclass)(Class,S),
	VAR_FPREFIX(strict_subclass)(S,Super),
	VAR_FPREFIX(ifd)(S,Method,_).

%% Note: when this is called, Class is always a subclass of Super
VAR_FPREFIX(conflict_ifd)(Class,Super,Method) :-
	VAR_FPREFIX(defined_ifd)(Super1,Method),
	ground((Super1,Super)),
	Super1 \= Super,
	VAR_FPREFIX(strict_subclass)(Class,Super1),
	tnot(VAR_FPREFIX(strict_subclass)(Super1,Super)),
	tnot(VAR_FPREFIX(strict_subclass)(Super,Super1)).


VAR_FPREFIX(imvd)(Class,Method,Value) :-
	(ground((Class,Method)) ->
	    tnot(VAR_FPREFIX(defined_imvd)(Class,Method)),
	    VAR_FPREFIX(strict_subclass)(Class,Super),
	    VAR_FPREFIX(imvd)(Super,Method,Value)
	  ;
	    VAR_FPREFIX(strict_subclass)(Class,Super),
	    VAR_FPREFIX(imvd)(Super,Method,Value),
	    STD_FPREFIX(tnot_all)(VAR_FPREFIX(defined_imvd)(Class,Method))
	),
	STD_FPREFIX(tnot_all)(VAR_FPREFIX(overwritten_imvd)(Class,Super,Method)),
	STD_FPREFIX(tnot_all)(VAR_FPREFIX(conflict_imvd)(Class,Super,Method)).

VAR_FPREFIX(defined_imvd)(Class,Method) :-
	VAR_FPREFIX(imvd)(Class,Method,_).

VAR_FPREFIX(overwritten_imvd)(Class,Super,Method) :-
	VAR_FPREFIX(strict_subclass)(Class,S),
	VAR_FPREFIX(strict_subclass)(S,Super),
	VAR_FPREFIX(imvd)(S,Method,_).

%% Note: when this is called, Class is always a subclass of Super
VAR_FPREFIX(conflict_imvd)(Class,Super,Method) :-
	VAR_FPREFIX(defined_imvd)(Super1,Method),
	ground((Super1,Super)),
	Super1 \= Super,
	VAR_FPREFIX(strict_subclass)(Class,Super1),
	tnot(VAR_FPREFIX(strict_subclass)(Super1,Super)),
	tnot(VAR_FPREFIX(strict_subclass)(Super,Super1)).


/*****************************************************************************
  rules for object existence and empty result sets
*****************************************************************************/
:- table VAR_FPREFIX(exists)/1.
:- table VAR_FPREFIX(mvd)/2.
:- table VAR_FPREFIX(imvd)/2.

VAR_FPREFIX(exists)(X) :- VAR_FPREFIX(fd)(X,_,_).
VAR_FPREFIX(exists)(X) :- VAR_FPREFIX(fd)(_,_,X).
VAR_FPREFIX(exists)(X) :- VAR_FPREFIX(mvd)(X,_,_).
VAR_FPREFIX(exists)(X) :- VAR_FPREFIX(mvd)(_,_,X).
VAR_FPREFIX(exists)(X) :- VAR_FPREFIX(ifd)(X,_,_).
VAR_FPREFIX(exists)(X) :- VAR_FPREFIX(ifd)(_,_,X).
VAR_FPREFIX(exists)(X) :- VAR_FPREFIX(imvd)(X,_,_).
VAR_FPREFIX(exists)(X) :- VAR_FPREFIX(imvd)(_,_,X).
VAR_FPREFIX(exists)(X) :- VAR_FPREFIX(isa)(X,_).
VAR_FPREFIX(exists)(X) :- VAR_FPREFIX(isa)(_,X).
VAR_FPREFIX(exists)(X) :- VAR_FPREFIX(sub)(X,_).
VAR_FPREFIX(exists)(X) :- VAR_FPREFIX(sub)(_,X).

VAR_FPREFIX(exists)(X) :-
	VAR_FPREFIX(fd)(_,M,_),
	M =.. [_|L],
	member(X,L).

VAR_FPREFIX(exists)(X) :-
	VAR_FPREFIX(mvd)(_,M,_),
	M =.. [_|L],
	member(X,L).

VAR_FPREFIX(exists)(X) :-
	VAR_FPREFIX(ifd)(_,M,_),
	M =.. [_|L],
	member(X,L).

VAR_FPREFIX(exists)(X) :-
	VAR_FPREFIX(imvd)(_,M,_),
	M =.. [_|L],
	member(X,L).


VAR_FPREFIX(mvd)(O,M) :- VAR_FPREFIX(mvd)(O,M,_).

VAR_FPREFIX(imvd)(O,M) :- VAR_FPREFIX(imvd)(O,M,_).


/*****************************************************************************
  rules for flattened path expressions on RHS
*****************************************************************************/
VAR_FPREFIX(fd_rhs)(O,M,R)   :- VAR_FPREFIX(fd)(O,M,R).
VAR_FPREFIX(mvd_rhs)(O,M,R)  :- VAR_FPREFIX(mvd)(O,M,R).
VAR_FPREFIX(ifd_rhs)(O,M,R)  :- VAR_FPREFIX(ifd)(O,M,R).
VAR_FPREFIX(imvd_rhs)(O,M,R) :- VAR_FPREFIX(imvd)(O,M,R).
VAR_FPREFIX(mvd_rhs)(O,M)    :- VAR_FPREFIX(mvd)(O,M).
VAR_FPREFIX(imvd_rhs)(O,M)   :- VAR_FPREFIX(imvd)(O,M).
VAR_FPREFIX(isa_rhs)(O1,O2)  :- VAR_FPREFIX(isa)(O1,O2).
VAR_FPREFIX(sub_rhs)(O1,O2)  :- VAR_FPREFIX(subclass)(O1,O2).
VAR_FPREFIX(fs_rhs)(O,M,R)   :- VAR_FPREFIX(fs)(O,M,R).
VAR_FPREFIX(mvs_rhs)(O,M,R)  :- VAR_FPREFIX(mvs)(O,M,R).

