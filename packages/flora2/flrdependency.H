
:- import append/3, member/2 from basics.

:- import variant/2 from subsumes.

:- import conset/2, conget/2 from gensym.

:- import
	flora_firstorderdef/2,
	flora_flogicstaticdef/2
   from flrfirstorder.

:- import flora_prlgdef/2 from flrprolog.

:- import flora_nth_token/2 from flrcomposer.
:- import flora_token_text/6 from flrlexer.

:- dynamic rule(_,_,_).
:- dynamic target(_).
:- dynamic dependency(_,_,_,_).
:- dynamic dependency(_,_,_).
:- dynamic checked(_).
:- dynamic program_tabled(_,_).
:- dynamic warning_seen(_,_,_,_).

:- export
	check_for_dependencies/2.
