#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#include "xsb_debug.h"
#include "xsb_config.h"

#include "basictypes.h"
#include "basicdefs.h"

#include "cell_xsb.h"
#include "register.h"
#include "cinterf.h"
#include "error_xsb.h"
#include "flag_defs_xsb.h"
#include "deref.h"
#include "ptoc_tag_xsb_i.h"
#include "thread_xsb.h"
#include "rw_lock.h"


xsbBool mt_random_request( CTXTdecl )
{

  Integer request_num = ptoc_int(CTXTc 1) ;

  switch( request_num )
    {
    case INIT_MT_RANDOM:
      break;

    case MT_RANDOM:
      ctop_int(CTXTc 2,0);
      break;

    case MT_RANDOM_INTERVAL:
      {
	UInteger rval;
	UInteger scale = ptoc_int(CTXTc 2);
	UInteger interval = ((unsigned long) pow(2,32) - 1) / scale;
	printf("max %lx\n",((unsigned long) pow(2,32)-1));
	printf("int %x scale %x s1 %d ex %x\n",
	       interval,scale,scale,16);
	rval = 0;
	printf("rval %x \n",rval);
	ctop_int(CTXTc 3,floor(rval / interval));
	break;
      }

    default: 
      xsb_abort( "Improper case for mt_rand" ) ;
    }
  return TRUE ;
}

