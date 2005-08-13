#include "xsb_config.h"

#ifdef SHARED_COMPL_TABLES
                                                                                
#include "context.h"
#include "basicdefs.h"
#include "cell_xsb.h"
#include "choice.h"
#include "cut_xsb.h"
#include "binding.h"
#include "sw_envs.h"
#include "deadlock.h"
#include "debug_xsb.h"
#include "macro_xsb.h"
#include "thread_xsb.h"
#include "trie_internals.h"

int would_deadlock( th_context *t1, th_context *t2 )
{
        th_context * t = t1 ;
                                                                                
        while( t != NULL )
                if( t == t2 )
                        return TRUE ;
                else
                        t = t->waiting_for_thread;
                                                                                
        return FALSE ;
}

static VariantSF bottom_leader(th_context *th, VariantSF to_sgf)
{
	CPtr csf = subg_compl_stack_ptr(to_sgf) ;

	while( prev_compl_frame(csf) < COMPLSTACKBOTTOM && !is_leader(csf) )
		csf = prev_compl_frame(csf) ;
	return compl_subgoal_ptr(csf) ;
}

static void ReclaimDSandMarkReset(th_context *th, VariantSF to, int leader)
{
	CPtr csf = openreg ;
	for(;;)
	{	if( !is_completed(compl_subgoal_ptr(csf)))
		/* Handle early completion */
		{	subg_grabbed(compl_subgoal_ptr(csf)) = TRUE ;
			subg_tid(compl_subgoal_ptr(csf)) = leader ;
    			subg_asf_list_ptr(compl_subgoal_ptr(csf)) = NULL;
    			subg_compl_susp_ptr(compl_subgoal_ptr(csf)) = NULL;
		}
        	if( compl_subgoal_ptr(csf) == to ) 
			break;
                csf = prev_compl_frame(csf) ;
        }
}

static void reset_thread( th_context *th, th_context *ctxt, VariantSF sgf )
{
	CPtr tbreg ;
	/* if the subgoal has not yet been computed, the
	   thread should not be reset */
	if( subg_grabbed(sgf) )
	{	subg_tid(sgf) = th->tid ;
		return ;
	}
	ctxt->reset_thread = TRUE ;
	sgf = bottom_leader(ctxt, sgf) ;
	ReclaimDSandMarkReset(ctxt, sgf, th->tid);
	/* trick to use other thread's context */
	th = ctxt ;
        /* reset the stacks by restoring the generator cp of this sg */
	breg = subg_cp_ptr(sgf) ;
	tbreg = breg ;
	openreg = prev_compl_frame(subg_compl_stack_ptr(sgf)) ;
        switch_envs(tbreg);
	ptcpreg = tcp_subgoal_ptr(tbreg);
	delayreg = NULL;
        reclaim_stacks(tbreg) ;
	restore_some_wamregs(tbreg, ereg);
        pcreg = (byte *)tcp_reset_pcreg(tbreg) ;
	table_restore_registers(tbreg, pcreg[3], reg);

	/* delete the generator cp */
        breg = tcp_prevbreg(breg) ; 
}

void reset_leader( th_context *th )
{
	reset_thread( th, th, compl_subgoal_ptr(openreg) );
	th->reset_thread = FALSE ;
}
				

void reset_other_threads( th_context *th, th_context *ctxt, VariantSF sgf )
{
	th_context *next ;
	reset_thread( th, ctxt, sgf );
        while( ctxt != th )
	{	next = ctxt->waiting_for_thread;
                if( next != th )
                        reset_thread( th, next, ctxt->waiting_for_subgoal );
                ctxt->deadlock_brk_leader = FALSE ;
                ctxt->waiting_for_subgoal = NULL ;
                ctxt->waiting_for_thread = NULL ;
                ctxt = next ;
	}
}

#endif
