:- export init_smodels/2.
:- export atom_handle/2.
:- export in_all_stable_models/2.
:- export a_stable_model/0.
:- export print_current_stable_model/0.
:- export in_current_stable_model/1.

:- import conset/2,coninc/1,conget/2 from prof_sbp.
:- import new_trie/1, delete_trie/1, trie_intern/5 from intern.
:- import trie_interned/4 from intern.
:- import file_write/2, file_nl/1 from xsb_writ.
:- import excess_vars/4 from setof.
:- import warning/1, abort/1, integer/1, repeat/0 from standard.
:- import get_calls/3, get_returns/3, table_state/2 from tables.
:- import (dynamic)/1, assert/1, retractall/1, retract/1 from assert.
:- import is_most_general_term/1, (index)/3 from standard.

/*
:- import asserta/1 from assert.
:- import xsb_configuration/2 from xsb_configuration.
:- import str_cat/3 from string.

:- xsb_configuration(install_dir,Install_dir),
	str_cat(Install_dir,'/packages/sModels/',Smodels_dir),
	asserta(library_directory(Smodels_dir)).
*/

:- import smodelsInit/0, smodelsNumAtoms/1 from smodels.
:- import smodelsBeginBasicRule/0, smodelsAddHead/1 from smodels.
:- import smodelsAddPosBody/1, smodelsAddNegBody/1 from smodels.
:- import smodelsEndRule/0, smodelsCommitRules/0 from smodels.
:- import smodelsExistsModel/1,  smodelsClose/0 from smodels.
:- import smodelsCheckAtom/2 from smodels.
:- import smodelsSetPosCompute/1, smodelsSetNegCompute/1 from smodels.


%%:- import writeln/1 from standard.

