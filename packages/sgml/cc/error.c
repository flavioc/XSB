/*****************************************************************************
 *                            error.c
 * This function contains the error handling routine of the sgml parser. This
 * routine creates the error term to throw in case of an exception.
 *
 ***************************************************************************/

#include <errno.h>
#include <stdlib.h>
#include "error.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include "cinterf.h"
#include "dtd.h"
#include "error_term.h"

prolog_term global_error_term = NULL;
prolog_term global_warning_term = NULL;

/**
 * Function to handle the errors. It creates an appropriate error term 
 * for the prolog side to throw.
 * Input : type of error
 * Output : TRUE on success, FALSE on failure
 **/
int
sgml2pl_error(plerrorid id, ...)
{ 
  prolog_term except = p2p_new();
  prolog_term formal = p2p_new();
  prolog_term swi = p2p_new();
  prolog_term tmp1 = p2p_new();
  prolog_term tmp;

  va_list args;
  char msgbuf[1024];
  char *msg = NULL;

  va_start(args, id);
  /*Create the appropriate error term based on the type of error*/
  switch(id)
    { 
    case ERR_ERRNO:					/*Standard unix errors*/
      { 
	int err = va_arg(args, int);
	msg = strerror(err);

	  switch(err)
	    { 
	      /*Not enough memory error*/
	    case ENOMEM:
	  
	      c2p_functor("sgml", 1, tmp1); 	
	      tmp = p2p_arg( tmp1, 1);
	      c2p_functor( "resource_error", 1, tmp);
	  
	      c2p_string( "no_memory", p2p_arg( tmp, 1));
	      p2p_unify( tmp1, formal); 
	      break;
	      /*No access error*/
	    case EACCES:
	      { 
		const char *file = va_arg(args,   const char *);
		const char *action = va_arg(args, const char *);

		c2p_functor("sgml", 1, tmp1);
		tmp = p2p_arg( tmp1, 1);

		c2p_functor( "permission_error", 3, tmp);
		c2p_string( (char*)action, p2p_arg(tmp, 1));
		c2p_string( "file", p2p_arg(tmp, 2));
		c2p_string ( (char*)file, p2p_arg(tmp, 3));

		p2p_unify( tmp1, formal);
		break;
	      }
	      /*Entity not found error*/
	    case ENOENT:
	      { 
		const char *file = va_arg(args, const char *);
	 

		c2p_functor("sgml", 1, tmp1);
		tmp = p2p_arg( tmp1, 1);

		c2p_functor( "permission_error", 2, tmp);
	  		  
		c2p_string( "file", p2p_arg(tmp, 1));
		c2p_string ( (char*)file, p2p_arg(tmp, 2));

		p2p_unify( tmp1, formal); 

		break;
	      }
	      /*Defaults to system error*/
	    default:
	      {
	        c2p_functor("sgml", 1, tmp1);
	        tmp = p2p_arg( tmp1, 1);

		c2p_string("system_error", tmp);
		p2p_unify( tmp1, formal);
		break;
	      }
	    }
	  break;
	}
    case ERR_TYPE:
      { 
	const char *expected = va_arg(args, const char*);
	prolog_term actual        = va_arg(args, prolog_term);


	/*Type error*/
	c2p_functor("sgml", 1, tmp1);
        tmp = p2p_arg( tmp1, 1);

	if( is_attv( actual) && strcmp(expected, "variable") != 0 )
	  {
	    c2p_string( "instantiation_error", tmp);
	    p2p_unify( tmp1, formal);
	  }
	else
	  {
	    c2p_functor( "type_error", 2, tmp);
	    c2p_string( (char*)expected, p2p_arg(tmp, 1));
	    p2p_unify ( actual, p2p_arg(tmp, 2));
	    p2p_unify( tmp1, formal);
	  }
	break;
      }	
    case ERR_DOMAIN:				/*Domain error*/
      { 
	const char *expected = va_arg(args, const char*);
	prolog_term actual        = va_arg(args, prolog_term);

	/*Improper domain of functor*/
        c2p_functor("sgml", 1, tmp1);
        tmp = p2p_arg( tmp1, 1);
	
        if( is_attv( actual) && strcmp(expected, "variable") != 0 )
	  {
	    c2p_string( "instantiation_error", tmp);
	    p2p_unify( tmp1, formal);
	  }
        else
	  {
	    c2p_functor( "domain_error", 2, tmp);
	    c2p_string( (char*)expected, p2p_arg(tmp, 1));
	    p2p_unify( actual, p2p_arg(tmp, 2));
	    p2p_unify( tmp1, formal);
	  }
	break;
      }
    case ERR_EXISTENCE:			/*Existence error*/
      { 
	const char *type = va_arg(args, const char *);
	prolog_term obj  = va_arg(args, prolog_term);

	/*Resource not found*/
	c2p_functor("sgml", 1, tmp1);
        tmp = p2p_arg( tmp1, 1);

	c2p_functor( "existence_error", 2, tmp);
                                                                              
        c2p_string( (char*)type, p2p_arg(tmp, 1));
        p2p_unify ( obj, p2p_arg(tmp, 2));
                                                                                
       	p2p_unify( tmp1, formal);
	break;
      }
    case ERR_FAIL:
      { 
	/*Goal failed error*/
	prolog_term goal  = va_arg(args, prolog_term);

	c2p_functor("sgml", 1, tmp1);
	tmp = p2p_arg( tmp1, 1);

        c2p_functor( "goal_failed", 1, tmp);

	p2p_unify( p2p_arg( tmp,1), goal);	
      
       	p2p_unify( tmp1, formal);
	break;
      }
    case ERR_LIMIT:
      { 
	/*Limit exceeded error*/
	const char *limit = va_arg(args, const char *);
	long maxval  = va_arg(args, long);

        c2p_functor("sgml", 1, tmp1);
	tmp = p2p_arg( tmp1, 1);
	
	c2p_functor( "limit_exceeded", 2, tmp);
	c2p_string( (char*)limit, p2p_arg( tmp,1));
	c2p_int( maxval, p2p_arg( tmp, 2));
       	p2p_unify( tmp1, formal);
	break;
      }
    case ERR_MISC:
      { 
	/*Miscellaneous error*/
	const char *id = va_arg(args, const char *);
      
	const char *fmt = va_arg(args, const char *);

	vsprintf(msgbuf, fmt, args);
	msg = msgbuf;

	c2p_functor("sgml", 1, tmp1);
	tmp = p2p_arg( tmp1, 1);

	c2p_functor( "miscellaneous", 1, tmp);
	c2p_string( (char*)id, p2p_arg( tmp, 1));
	p2p_unify( tmp1, formal);
	break; 
      }
    default:
      assert(0);
    }

  va_end(args);

  if ( msg )
    { 
      prolog_term msgterm  = p2p_new();

      if ( msg )
	{ 
	  c2p_string( msg, msgterm);
	}

      tmp = p2p_new();

      c2p_functor( "context", 1, tmp);
      p2p_unify( p2p_arg( tmp, 1), msgterm);	
      p2p_unify( tmp, swi);
    }

  /*Create the error term to throw*/
  tmp = p2p_new();
  c2p_functor( "error", 2, tmp);
  p2p_unify( p2p_arg( tmp, 1), formal);
  p2p_unify( p2p_arg( tmp, 2), swi);
  p2p_unify( tmp, except);
  
  return p2p_unify( global_error_term, except);
}

