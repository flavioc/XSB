/* File:      smodels.H
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

:- compiler_options([xpp_on]).

#include "configs/config.h"

#ifdef LINUX
:- compile_command 'egcc -c xsb_wrap_smodels.c; g++ -s -o smodels.so -shared smodels.c xsb_wrap_smodels.o  smodels.o stack.o dcl.o atomrule.o read.o queue.o timer.o list.o improve.o program.o api.o stable.o tree.o denant.o'.
#else
:- compile_command 'gcc -c xsb_wrap_smodels.c; g++ -s -o smodels.so -shared smodels.c xsb_wrap_smodels.o  smodels.o stack.o dcl.o atomrule.o read.o queue.o timer.o list.o improve.o program.o api.o stable.o tree.o denant.o'.
#endif

:- foreign_pred smodelsInit from init(void):void.
:- foreign_pred smodelsNumAtoms(+a) from numberAtoms(a:int):void.
:- foreign_pred smodelsAtomName(+a) from atomName(a:string):void.
:- foreign_pred smodelsBeginBasicRule from beginBasicRule(void):void.
:- foreign_pred smodelsAddHead(+a) from addHead(a:int):void.
:- foreign_pred smodelsAddPosBody(+a) from addPosBody(a:int):void.
:- foreign_pred smodelsAddNegBody(+a) from addNegBody(a:int):void.
:- foreign_pred smodelsEndRule from endRule(void):void.
:- foreign_pred smodelsCommitRules from commitRules(void):void.
:- foreign_pred smodelsPrintProgram from printProgram(void):void.
:- foreign_pred smodelsExistsModel(-retval) from existsModel(void):int.
:- foreign_pred smodelsPrintAnswer from printAnswer(void):void.
:- foreign_pred smodelsClose from close(void):void.
:- foreign_pred smodelsCheckAtom(+a,-retval) from checkAtom(a:int):int.
:- foreign_pred smodelsSetPosCompute(+a) from setPosCompute(a:int):void.
:- foreign_pred smodelsSetNegCompute(+a) from setNegCompute(a:int):void.
:- foreign_pred smodelsResetPosCompute(+a) from resetPosCompute(a:int):void.
:- foreign_pred smodelsResetNegCompute(+a) from resetNegCompute(a:int):void.
:- foreign_pred smodelsRemember from remember(void):void.
:- foreign_pred smodelsForget from forget(void):void.
:- foreign_pred smodelsSetBody(+a) from setBody(a:int):void.
:- foreign_pred smodelsSetHead(+a) from setHead(a:int):void.
:- foreign_pred smodelsWellFounded from wellfounded(void):void.
:- foreign_pred smodelsTestPos(+a,-retval) from testPos(a:int):int.
:- foreign_pred smodelsTestNeg(+a,-retval) from testNeg(a:int):int.
