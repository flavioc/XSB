
:- import member/2,length/2 from basics.

:- import subsumes/2 from subsumes.

:- import flora_abolish_table_predicate/1 from flrtables.

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
        is_prignoredep/2,
	flora_build_struct/6
   from flrcoder.

:- import flora_decode_predicate/7 from flrwrapper.

:- import get_canonical_form/2 from flrcanon.

:- dynamic rule(_,_,_).
:- dynamic target(_,_,_).
:- dynamic tabled_head(_).
:- dynamic dependency(_,_,_,_).
:- dynamic ignored(_,_,_).
:- dynamic warning_seen(_,_,_,_,_).

:- export
	is_control/3,
        depend/3,
	check_for_dependencies/2.
