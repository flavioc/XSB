/* File:      smodelsops.H
** Author(s): Luis Castro
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
** Copyright (C) ECRC, Germany, 1990
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
** $Id$
** 
*/

:- import smodelsInit/0, smodelsNumAtoms/1, smodelsAtomName/1 from smodels.
:- import smodelsBeginBasicRule/0, smodelsAddHead/1 from smodels.
:- import smodelsAddPosBody/1, smodelsAddNegBody/1 from smodels.
:- import smodelsEndRule/0, smodelsCommitRules/0 from smodels.
:- import smodelsExistsModel/1 from smodels.
:- import smodelsClose/0 from smodels.
:- import smodelsCheckAtom/2 from smodels.
:- import ith/3 from basics.

:- export smInit/2, smAddRule/4, smCommitProgram/0, smCompute/0.
:- export smReturnModel/2, smEnd/0.
:- export smInit/1, smAddRule/3, smGetModel/2.

