/* File:      flrprolog.H
**
** Author(s): Guizhen Yang
**
** Contact:   xsb-contact@cs.sunysb.edu
**
** Copyright (C) The Research Foundation of SUNY, 1999-2001
**
** XSB is free software; you can redistribute it and/or modify it under the
** terms of the GNU Library General Public License as published by the Free
** Software Foundation; either version 2 of the License, or (at your option)
** any later version.
**
** XSB is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
** FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
** more details.
**
** You should have received a copy of the GNU Library General Public License
** along with XSB; if not, write to the Free Software Foundation,
** Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
**
**
*/


%% NOTE: flora_dynprlg exists only because of the way import
%% is handled in foo@prolog(mod). In this case, FLORA inserts
%% a dynamic declaration that foo is a prolog predicate.
:- dynamic flora_dynprlg(_,_).

%%:- dynamic flora_shprlg(_,_).

:- index 
	flora_dynprlg/2-1,
	%%flora_shprlg/2-1,
	flora_prlg/2-1.

:- import assert/1, retractall/1 from assert.

%%:- import conget/2 from gensym.

:- export
          flora_reset_prolog/0,
          %%flora_shreset_prolog/0,
	  %%flora_shdefine_prolog/2,
	  flora_define_prolog/2,
	  flora_prlgdef/2.

