
:- import append/3, member/2 from basics.

:- import variant/2 from subsumes.

:- import
	flora_set_counter/2,
	flora_get_counter/2
   from flrporting.

:- import
	flora_firstorderdef/2,
	flora_flogicstaticdef/2
   from flrfirstorder.

:- import flora_prlgdef/2 from flrprolog.

:- import
	flora_nth_token/2,
	flora_token_rulenum/2
   from flrcomposer.

:- import flora_token_text/6 from flrlexer.

:- import
	is_prrule/3,
	is_prdirective/2,
	is_prtable/3,
	is_prhilogtable/2,
	is_prthismodule/1,
	is_prthisfdbstorage/1,
	is_pratom/3,
	is_prnumber/3,
	is_prvariable/3,
	is_prstring/3,
	is_prlist/4,
	is_prfdbstorage/3,
	is_skolem/5,
	is_prterm/4,
	is_goallogic/4,
	is_prcall/2,
	is_pratomlit/3,
	is_prtermlit/4,
	is_prworkspace/3,
	is_florasyslib/5,
	is_prologterm/4,
	is_prologliblit/4,
	is_prnewpred/4,
	is_prnewoid/3,
	is_flogic/4,
	is_fllibdb/1
   from flrcoder.

:- dynamic rule(_,_,_).
:- dynamic target(_).
:- dynamic dependency(_,_,_,_).
:- dynamic dependency(_,_,_).
:- dynamic checked(_).
:- dynamic program_tabled(_,_).
:- dynamic warning_seen(_,_,_,_,_).

:- export
	check_for_dependencies/2.
