#include "context.h"
#include "basicdefs.h"
#include "choice.h"
#include "cut_xsb.h"
#include "binding.h"
#include "sw_envs.h"
#include "deadlock.h"
#include "debug_xsb.h"
#include "macro_xsb.h"
#include "thread_xsb.h"
#include "trie_internals.h"

#ifdef MULTI_THREAD
                                                                                
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

static void reinit_subgoal( VariantSF sgf )
{
/*    SM_DeallocateStructList(smALN,subg_answers(sgf),subg_ans_list_tail(sgf));
 */
    subg_ans_list_ptr(sgf) = empty_return();
    subg_ans_list_tail(sgf) = NULL;
    subg_asf_list_ptr(sgf) = NULL;
}

static VariantSF bottom_leader(th_context *th, VariantSF to_sgf)
{
	CPtr csf = openreg ;
	while( compl_subgoal_ptr(csf) != to_sgf )
		csf = prev_compl_frame(csf) ;
	while( prev_compl_frame(csf) < COMPLSTACKBOTTOM && !is_leader(csf) )
		csf = prev_compl_frame(csf) ;
	return compl_subgoal_ptr(csf) ;
}

static void ReclaimDSandMarkReset(th_context *th, VariantSF to, int leader)
{
	CPtr csf = openreg ;
	for(;;)
	{	subg_grabbed(compl_subgoal_ptr(csf)) = TRUE ;
		subg_tid(compl_subgoal_ptr(csf)) = leader ;
		reinit_subgoal(compl_subgoal_ptr(csf)) ;
        	if( compl_subgoal_ptr(csf) == to ) 
			break;
                csf = prev_compl_frame(csf) ;
        }
}

static void reset_thread( th_context *th, th_context *ctxt, VariantSF sgf )
{
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
        switch_envs(breg);
	ptcpreg = tcp_subgoal_ptr(breg);
	delayreg = NULL;
        reclaim_stacks(breg) ;
	restore_some_wamregs(breg, ereg);
        pcreg = (byte *)tcp_reset_pcreg(breg) ;
	table_restore_registers(breg, pcreg[3], reg);

	/* delete the generator cp */
        breg = tcp_prevbreg(breg) ; 
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
