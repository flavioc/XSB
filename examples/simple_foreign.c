#include <stdio.h>
#include <string.h>
#include <alloca.h>

#include "../emu/cinterf.h"

void minus_one(void)
{
   int	i;

	i = ptoc_int(1);
	ctop_int(2, i-1);
}

void change_char(void)
{
   char	*str_in; 
   int	pos;
   char *c, *str_out;

	str_in = (char *) ptoc_string(1);
	str_out = (char *) alloca(strlen(str_in)+1);
	strcpy(str_out, str_in);
	pos = ptoc_int(2);
	c = (char *) ptoc_string(3);
	str_out[pos-1] = c[0];

	/* Now that we have constructed a new symbol, we must ensure that it
	   appears in the symbol table.  This can be done using function
	   string_find() that searches the symbol table for the symbol, and
	   if the symbol does not appear there, it inserts it.  If we are 
	   sure that the symbol already appeared in the symbol table there
	   is no need to use string_find(). 
	 */

	ctop_string(4, (char *) string_find(str_out,4));
}

