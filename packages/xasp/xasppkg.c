/* File:      xasppkg.c
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

#include <stdlib.h>
#include <stdio.h>
#include "smodels.h"
#ifndef SMODELS_H
#error "You need the .h and .o files from SModels in your directory"
#endif
#include "api.h"
#include "atomrule.h"

Smodels *smodels;
Api *api;
Atom **atoms;
int curatom,totatoms; /* current atom, used during creation */

extern "C" void init(void)
{
  smodels = new Smodels;

  api = new Api(&(smodels->program));
}

extern "C" void numberAtoms(int nAtoms)
{
  int i;

  atoms = (Atom **) malloc(sizeof(Atom*)*nAtoms);
  for (i=0; i<nAtoms; i++) 
    atoms[i] = api->new_atom();

  curatom = 0;
  totatoms=nAtoms;
}

extern "C" void atomName(char *name)
{ 
  api->set_name(atoms[curatom],name);
  curatom++;
}

extern "C" void beginBasicRule(void)
{
  api->begin_rule(BASICRULE);
}

extern "C" void beginChoiceRule(void)
{
  api->begin_rule(CHOICERULE);
}

extern "C" void beginConstraintRule(void)
{
  api->begin_rule(CONSTRAINTRULE);
}

extern "C" void beginWeightRule(void)
{
  api->begin_rule(WEIGHTRULE);
}

extern "C" void addHead(int atomNum)
{
  api->add_head(atoms[atomNum-1]);
}

extern "C" void addWPosBody(int atomNum,Weight weight)
{
  api->add_body(atoms[atomNum-1],true,weight);
}

extern "C" void addPosBody(int atomNum)
{
  api->add_body(atoms[atomNum-1],true);
}

extern "C" void addWNegBody(int atomNum,Weight weight)
{
  api->add_body(atoms[atomNum-1],false,weight);
}

extern "C" void addNegBody(int atomNum)
{
  api->add_body(atoms[atomNum-1],false);
}

extern "C" void endRule(void)
{
  api->end_rule();
}

extern "C" void commitRules(void)
{
  api->done();
  smodels->init();
}

extern "C" void printProgram(void)
{
  smodels->program.print();
}

extern "C" int existsModel(void)
{
  return smodels->model();
}

extern "C" void printAnswer(void)
{
  smodels->printAnswer();
}

extern "C" void close(void)
{
  delete(api);
  delete(smodels);
  free(atoms);
}

extern "C" int checkAtom(int atom)
{
  return atoms[atom-1]->Bpos;
}

extern "C" void setPosCompute(int atom)
{
  api->set_compute(atoms[atom-1],true);
}

extern "C" void setNegCompute(int atom)
{
  api->set_compute(atoms[atom-1],false);
}

extern "C" void resetPosCompute(int atom)
{
  api->reset_compute(atoms[atom-1],true);
}

extern "C" void resetNegCompute(int atom)
{
  api->reset_compute(atoms[atom-1],false);
}

extern "C" void remember(void)
{
  api->remember();
}

extern "C" void forget(void)
{
  api->forget();
}

extern "C" void setBody(long val)
{
  api->set_atleast_body(val);
}

extern "C" void setWeight(long val)
{
  api->set_atleast_weight(val);
}

extern "C" void setHead(long val)
{
  api->set_atleast_head(val);
}

extern "C" void wellfounded(void)
{
  smodels->wellfounded();
}

extern "C" int testPos(int atom)
{
  return smodels->testPos(atoms[atom-1]);
}

extern "C" int testNeg(int atom)
{
  return smodels->testNeg(atoms[atom-1]);
}
