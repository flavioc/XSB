/*****************************************************************************
 *                         sgml2pl.H
 * typical xsb package header file for the sgml package exporting the symbols
 *
 * ***************************************************************************/

:- export pl_new_sgml_parser/2, 
	pl_sgml_parse/2, 
	pl_set_sgml_parser/2, 
	pl_open_dtd/2, 
	pl_new_dtd/2, 
	pl_free_sgml_parser/1, 
	pl_free_dtd/1, 
	pl_doctype/2, 
	pl_allocate_error_term/2, 
	pl_xml_quote_attribute/2, 
	pl_xml_quote_cdata/2, 
	pl_xml_name/1,
	pl_finalize_warn/1.
