
:- import append/3, member/2, length/2 from basics.

:- import subsumes/2,variant/2 from subsumes.

:- import
	flora_set_counter/2,
	flora_get_counter/2
   from flrporting.

:- import
	flora_nth_token/2,
	flora_token_rulenum/2
   from flrcomposer.

:- import flora_token_text/6 from flrlexer.

:- import
	is_prrule/3,
	is_prdirective/2,
	is_prtable/3,
	flora_build_struct/6
   from flrcoder.

:- dynamic rule(_,_,_).
:- dynamic target(_).
:- dynamic dependency(_,_,_,_).
:- dynamic dependency(_,_,_).
:- dynamic checked(_).
:- dynamic program_tabled(_,_).
:- dynamic warning_seen(_,_,_,_,_).

:- export
	is_control/3,
	check_for_dependencies/2.
