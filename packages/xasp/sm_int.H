/* File:      smodelsops.H
** Author(s): Luis Castro, Swift
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

:- export smcInit/0,smcReInit/0, smrInit/1,
  smcAddRule/2, smcRetractRule/2,
  smrAddBasicRule/3,smrAddChoiceRule/3,
  smrAddWeightRule/4,smrAddConstraintRule/4,
  smcSetCompute/1, smrSetNegCompute/1, smrSetPosCompute/1,
  smcCommitProgram/0, smrCommitProgram/0,
  smComputeModel/0,
  smcExamineModel/1, smcExamineModel/2, smrExamineModel/2, smEnd/0,
          print_cache/0.

:- import '$sm_pos_compute'/1, '$sm_neg_compute'/1,
          '$sm_atomnum'/2,
           '$sm_basicrule'/3, '$sm_weightrule'/4,
           '$sm_constraintrule'/4, '$sm_choicerule'/3 from usermod.

:- import smodelsInit/0, smodelsNumAtoms/1 from xasppkg.
:- import smodelsCheckAtom/2 from xasppkg.
:- import smodelsBeginBasicRule/0,smodelsBeginWeightRule/0 from xasppkg.
:- import smodelsBeginChoiceRule/0, smodelsBeginConstraintRule/0 
                       from xasppkg.
:- import smodelsAddHead/1 from xasppkg.
:- import smodelsAddPosBody/1, smodelsAddWPosBody/2, 
          smodelsAddNegBody/1, smodelsAddWNegBody/2  from xasppkg.

:- import smodelsSetBody/1, smodelsSetWeight/1 from xasppkg.

:- import smodelsEndRule/0 from xasppkg.
:- import smodelsSetPosCompute/1, smodelsSetNegCompute/1 from xasppkg.
:- import smodelsCommitRules/0 from xasppkg.
:- import smodelsExistsModel/1 from xasppkg.
:- import smodelsClose/0 from xasppkg.

:- import conget/2, conset/2 from machine.
:- import member/2 from basics.
