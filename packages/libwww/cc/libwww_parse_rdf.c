/* File:      libwww_parse_rdf.c
** Author(s): kifer
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 2000
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


#include "libwww_util.h"
#include "libwww_parse.h"
#include "libwww_parse_rdf.h"


/* BOOL, PRIVATE, PUBLIC, etc., are defined in a Libwww header */


void set_rdf_conversions()
{
  /* Must delete old converter and create new. Apparently something in libwww
     releases the atoms used in thes converters, which causes it to crash 
     in HTStreamStack() on the second call to rdfparse. */
  HTPresentation_deleteAll(RDF_converter);
  RDF_converter = HTList_new();

  HTConversion_add(RDF_converter,"*/*", "www/debug",
		   HTBlackHoleConverter, 1.0, 0.0, 0.0);
  HTConversion_add(RDF_converter,"message/rfc822", "*/*",
		   HTMIMEConvert, 1.0, 0.0, 0.0);
  HTConversion_add(RDF_converter,"message/x-rfc822-foot", "*/*",
		   HTMIMEFooter, 1.0, 0.0, 0.0);
  HTConversion_add(RDF_converter,"message/x-rfc822-head", "*/*",
		   HTMIMEHeader, 1.0, 0.0, 0.0);
  HTConversion_add(RDF_converter,"message/x-rfc822-cont", "*/*",
		   HTMIMEContinue, 1.0, 0.0, 0.0);
  HTConversion_add(RDF_converter,"message/x-rfc822-upgrade","*/*",
		   HTMIMEUpgrade, 1.0, 0.0, 0.0);
  HTConversion_add(RDF_converter,"message/x-rfc822-partial", "*/*",
		   HTMIMEPartial, 1.0, 0.0, 0.0);
  HTConversion_add(RDF_converter,"multipart/*", "*/*",
		   HTBoundary, 1.0, 0.0, 0.0);
  HTConversion_add(RDF_converter,"text/x-http", "*/*",
		   HTTPStatus_new, 1.0, 0.0, 0.0);
  /* www/rdf is invented for servers that don't recognize RDF */
  HTConversion_add(RDF_converter,"text/plain", "www/rdf",
		   HTRDFToTriples, 1.0, 0.0, 0.0);
  HTConversion_add(RDF_converter,"www/present", "www/rdf",
		   HTRDFToTriples, 1.0, 0.0, 0.0);
  HTConversion_add(RDF_converter, "text/rdf", "*/*", 
		   HTRDFToTriples, 1.0, 0.0, 0.0);
  HTConversion_add(RDF_converter, "application/rdf", "*/*",
		   HTRDFToTriples, 1.0, 0.0, 0.0);
}

