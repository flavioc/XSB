module('assert', unloaded).
entry('assert',0x8179bc0,'abolish'/1,'UDEF','UNLOADED','n/a',0x8179bd0).
entry('assert',0x81799c0,'dynamic'/1,'UDEF','UNLOADED','n/a',0x81799d0).
entry('assert',0x8179990,'assert'/1,'UDEF','UNLOADED','n/a',0x81799a0).

module('basics', unloaded).
entry('basics',0x8179900,'append'/3,'UDEF','UNLOADED','n/a',0x8179910).

module('machine', unloaded).
entry('machine',0x81797e0,'term_psc'/2,'UDEF','UNLOADED','n/a',0x81797f0).
entry('machine',0x8179790,'stat_set_flag'/2,'UDEF','UNLOADED','n/a',0x81797a0).
entry('machine',0x8179740,'thread_request'/5,'UDEF','UNLOADED','n/a',0x8179750).

module('thread',loaded).
entry('thread',0x817ac48,'n_list'/3,'PRED','HIDDEN','UNTABLED',0x817bb40).
entry('thread',0x817ac00,'par_wait'/1,'PRED','HIDDEN','UNTABLED',0x817bc20).
entry('thread',0x817abb8,'par_spawn'/2,'PRED','HIDDEN','UNTABLED',0x817bba0).
entry('thread',0x817ab28,'make_ts_goal'/2,'PRED','HIDDEN','UNTABLED',0x817b788).
entry('thread',0x817aab0,'ts_prefix'/1,'PRED','HIDDEN','UNTABLED',0x817b720).
entry('thread',0x817a930,'user_mutex'/2,'PRED','HIDDEN','UNTABLED',0x817b578).
entry('thread',0x817a8d8,'init_multi_threading'/0,'PRED','HIDDEN','UNTABLED',0x817b528).
entry('thread',0x817a890,'_$main'/0,'UDEF','HIDDEN','n/a',0x817a8a0).
entry('thread',0x817a840,'_$thread_run'/1,'PRED','HIDDEN','UNTABLED',0x817b468).
entry('thread',0x817a810,'xsb_sys_mutex_unlock'/2,'PRED','HIDDEN','UNTABLED',0x817b0f8).
entry('thread',0x817a7e0,'xsb_sys_mutex_lock'/2,'PRED','HIDDEN','UNTABLED',0x817b0b0).
entry('thread',0x817a6b8,'xsb_mutex_init_np0'/3,'PRED','HIDDEN','UNTABLED',0x817ae70).
entry('thread',0x817a668,'console_exmut'/1,'PRED','VISIBLE','UNTABLED',0x817bd18).
entry('thread',0x817a618,'exmut_execute'/2,'PRED','VISIBLE','UNTABLED',0x817bc90).
entry('thread',0x817a5c8,'n_par_execute'/2,'PRED','VISIBLE','UNTABLED',0x817baf0).
entry('thread',0x817a580,'par_execute'/1,'PRED','VISIBLE','UNTABLED',0x817bab0).
entry('thread',0x817a528,'ts_abolish_table_call'/1,'PRED','VISIBLE','UNTABLED',0x817b908).
entry('thread',0x817a4e0,'ts_abolish'/0,'PRED','VISIBLE','UNTABLED',0x817ba68).
entry('thread',0x817a490,'ts_abolish_tables'/0,'PRED','VISIBLE','UNTABLED',0x817ba20).
entry('thread',0x817a440,'ts_init_tables'/0,'PRED','VISIBLE','UNTABLED',0x817b988).
entry('thread',0x817a3f8,'ts_tnot'/1,'PRED','VISIBLE','UNTABLED',0x817b948).
entry('thread',0x817a3b0,'ts_call'/1,'PRED','VISIBLE','UNTABLED',0x817b8c8).
entry('thread',0x817a368,'ts_assert'/1,'PRED','VISIBLE','UNTABLED',0x817b828).
entry('thread',0x817a338,'xsb_mutex_init_np'/2,'PRED','VISIBLE','UNTABLED',0x817b2f8).
entry('thread',0x817a308,'xsb_mutex_destroy'/1,'PRED','VISIBLE','UNTABLED',0x817b3b8).
entry('thread',0x817a2d8,'xsb_mutex_unlock'/1,'PRED','VISIBLE','UNTABLED',0x817b388).
entry('thread',0x817a2a8,'xsb_mutex_trylock'/1,'PRED','VISIBLE','UNTABLED',0x817b358).
entry('thread',0x817a278,'xsb_mutex_lock'/1,'PRED','VISIBLE','UNTABLED',0x817b328).
entry('thread',0x817a248,'xsb_mutex_init'/1,'PRED','VISIBLE','UNTABLED',0x817b2c8).
entry('thread',0x817a218,'xsb_thread_self'/1,'PRED','VISIBLE','UNTABLED',0x817b298).
entry('thread',0x817a1e8,'xsb_thread_detach'/1,'PRED','VISIBLE','UNTABLED',0x817b268).
entry('thread',0x817a1b8,'xsb_thread_join'/1,'PRED','VISIBLE','UNTABLED',0x817b238).
entry('thread',0x817a188,'xsb_thread_join'/2,'PRED','VISIBLE','UNTABLED',0x817b208).
entry('thread',0x817a158,'xsb_thread_exit'/1,'PRED','VISIBLE','UNTABLED',0x817b1a0).
entry('thread',0x817a128,'xsb_thread_exit'/0,'PRED','VISIBLE','UNTABLED',0x817b1d0).
entry('thread',0x817a0f8,'xsb_thread_create'/2,'PRED','VISIBLE','UNTABLED',0x817b170).
entry('thread',0x817a0c8,'xsb_thread_create'/1,'PRED','VISIBLE','UNTABLED',0x817b140).
entry('thread',0x817a070,'xsb_user_mutex_unlock'/1,'PRED','VISIBLE','UNTABLED',0x817b6a8).
entry('thread',0x817a020,'xsb_user_mutex_lock'/1,'PRED','VISIBLE','UNTABLED',0x817b5e8).
entry('thread',0x8179fc8,'xsb_sys_mutex_unlock'/1,'PRED','VISIBLE','UNTABLED',0x817b428).
entry('thread',0x8179f78,'xsb_sys_mutex_lock'/1,'PRED','VISIBLE','UNTABLED',0x817b3e8).
entry('thread',0x8179f28,'xsb_mutex_destroy'/2,'PRED','VISIBLE','UNTABLED',0x817afd0).
entry('thread',0x8179ed8,'xsb_mutex_unlock'/2,'PRED','VISIBLE','UNTABLED',0x817af88).
entry('thread',0x8179e88,'xsb_mutex_trylock'/2,'PRED','VISIBLE','UNTABLED',0x817af40).
entry('thread',0x8179e38,'xsb_mutex_lock'/2,'PRED','VISIBLE','UNTABLED',0x817aef8).
entry('thread',0x8179de8,'xsb_mutex_init_np'/3,'PRED','VISIBLE','UNTABLED',0x817b018).
entry('thread',0x8179d98,'xsb_mutex_init'/2,'PRED','VISIBLE','UNTABLED',0x817aeb0).
entry('thread',0x8179d48,'xsb_thread_self'/2,'PRED','VISIBLE','UNTABLED',0x817ae28).
entry('thread',0x8179cf8,'xsb_thread_detach'/2,'PRED','VISIBLE','UNTABLED',0x817ade0).
entry('thread',0x8179ca8,'xsb_thread_join'/3,'PRED','VISIBLE','UNTABLED',0x817ad98).
entry('thread',0x8179c58,'xsb_thread_exit'/2,'PRED','VISIBLE','UNTABLED',0x817ad38).
entry('thread',0x8179c08,'xsb_thread_create'/3,'PRED','VISIBLE','UNTABLED',0x817acf0).

module('tables', unloaded).
entry('tables',0x8179b70,'abolish_table_pred'/1,'UDEF','UNLOADED','n/a',0x8179b80).
entry('tables',0x8179b20,'abolish_table_call'/1,'UDEF','UNLOADED','n/a',0x8179b30).
entry('tables',0x81792f0,'tnot'/1,'UDEF','UNLOADED','n/a',0x8179300).

module('standard', unloaded).
entry('standard',0x8179ad8,'name'/2,'UDEF','UNLOADED','n/a',0x8179ae8).
entry('standard',0x8179a98,'=..'/2,'UDEF','UNLOADED','n/a',0x8179aa8).
entry('standard',0x8179a50,'functor'/3,'UDEF','UNLOADED','n/a',0x8179a60).
entry('standard',0x8179a08,'table'/1,'UDEF','UNLOADED','n/a',0x8179a18).
entry('standard',0x8179870,'writeln'/1,'UDEF','UNLOADED','n/a',0x8179880).
entry('standard',0x8179828,'call'/1,'UDEF','UNLOADED','n/a',0x8179838).
entry('standard',0x8179258,':'/2,'ORDI','UNLOADED','n/a',0x8179268).
entry('standard',0x8179208,','/2,'ORDI','UNLOADED','n/a',0x8179218).
entry('standard',0x81791b0,'true'/0,'ORDI','UNLOADED','n/a',0x81791c0).

module('usermod',loaded).
equiv(usermod,global).


/*text below		*/

segment([
     inst(0x817aca0, test_heap, 0, 2000), 
     inst(0x817aca8, try, 0, 0x817acb8), 
     inst(0x817acb0, trust, 0, 0x817acd0), 
     inst(0x817acb8, allocate_gc, 2, 3), 
     inst(0x817acbc, getpbreg, 2), 
     inst(0x817acc0, call, 3, 0x817a8d8, 'init_multi_threading'/0), 
     inst(0x817acc8, putpbreg, 2), 
     inst(0x817accc, fail), 
     inst(0x817acd0, proceed)]).
segment([
     inst(0x817acf0, test_heap, 3, 2000), 
     inst(0x817acf8, movreg, r1, r6), 
     inst(0x817acfc, movreg, r2, r7), 
     inst(0x817ad00, movreg, r3, r5), 
     inst(0x817ad04, putnumcon, r1, 1), 
     inst(0x817ad0c, movreg, r6, r2), 
     inst(0x817ad10, movreg, r7, r3), 
     inst(0x817ad14, puttvar, r4, r4), 
     inst(0x817ad18, xsb_execute, 0x8179740, 'thread_request'/5)]).
segment([
     inst(0x817ad38, test_heap, 2, 2000), 
     inst(0x817ad40, allocate_gc, 2, 4), 
     inst(0x817ad44, getpvar, 2, r1), 
     inst(0x817ad48, getpvar, 3, r2), 
     inst(0x817ad4c, call, 4, 0x817a490, 'ts_abolish_tables'/0), 
     inst(0x817ad54, call, 4, 0x817a4e0, 'ts_abolish'/0), 
     inst(0x817ad5c, putnumcon, r1, 2), 
     inst(0x817ad64, putdval, 2, r2), 
     inst(0x817ad68, puttvar, r3, r3), 
     inst(0x817ad6c, puttvar, r4, r4), 
     inst(0x817ad70, putdval, 3, r5), 
     inst(0x817ad74, deallocate), 
     inst(0x817ad78, xsb_execute, 0x8179740, 'thread_request'/5)]).
segment([
     inst(0x817ad98, test_heap, 3, 2000), 
     inst(0x817ada0, movreg, r1, r6), 
     inst(0x817ada4, movreg, r2, r7), 
     inst(0x817ada8, movreg, r3, r5), 
     inst(0x817adac, putnumcon, r1, 3), 
     inst(0x817adb4, movreg, r6, r2), 
     inst(0x817adb8, movreg, r7, r3), 
     inst(0x817adbc, puttvar, r4, r4), 
     inst(0x817adc0, xsb_execute, 0x8179740, 'thread_request'/5)]).
segment([
     inst(0x817ade0, test_heap, 2, 2000), 
     inst(0x817ade8, movreg, r1, r6), 
     inst(0x817adec, movreg, r2, r5), 
     inst(0x817adf0, putnumcon, r1, 4), 
     inst(0x817adf8, movreg, r6, r2), 
     inst(0x817adfc, puttvar, r3, r3), 
     inst(0x817ae00, puttvar, r4, r4), 
     inst(0x817ae04, xsb_execute, 0x8179740, 'thread_request'/5)]).
segment([
     inst(0x817ae28, test_heap, 2, 2000), 
     inst(0x817ae30, movreg, r1, r6), 
     inst(0x817ae34, movreg, r2, r5), 
     inst(0x817ae38, putnumcon, r1, 5), 
     inst(0x817ae40, movreg, r6, r2), 
     inst(0x817ae44, puttvar, r3, r3), 
     inst(0x817ae48, puttvar, r4, r4), 
     inst(0x817ae4c, xsb_execute, 0x8179740, 'thread_request'/5)]).
segment([
     inst(0x817ae70, test_heap, 3, 2000), 
     inst(0x817ae78, movreg, r1, r6), 
     inst(0x817ae7c, movreg, r3, r5), 
     inst(0x817ae80, putnumcon, r1, 6), 
     inst(0x817ae88, movreg, r6, r3), 
     inst(0x817ae8c, puttvar, r4, r4), 
     inst(0x817ae90, xsb_execute, 0x8179740, 'thread_request'/5)]).
segment([
     inst(0x817aeb0, test_heap, 2, 2000), 
     inst(0x817aeb8, movreg, r1, r3), 
     inst(0x817aebc, movreg, r2, r5), 
     inst(0x817aec0, putnumcon, r1, 6), 
     inst(0x817aec8, putnumcon, r2, 1), 
     inst(0x817aed0, puttvar, r4, r4), 
     inst(0x817aed4, xsb_execute, 0x8179740, 'thread_request'/5)]).
segment([
     inst(0x817aef8, test_heap, 2, 2000), 
     inst(0x817af00, movreg, r1, r6), 
     inst(0x817af04, movreg, r2, r5), 
     inst(0x817af08, putnumcon, r1, 7), 
     inst(0x817af10, movreg, r6, r2), 
     inst(0x817af14, puttvar, r3, r3), 
     inst(0x817af18, puttvar, r4, r4), 
     inst(0x817af1c, xsb_execute, 0x8179740, 'thread_request'/5)]).
segment([
     inst(0x817af40, test_heap, 2, 2000), 
     inst(0x817af48, movreg, r1, r6), 
     inst(0x817af4c, movreg, r2, r5), 
     inst(0x817af50, putnumcon, r1, 8), 
     inst(0x817af58, movreg, r6, r2), 
     inst(0x817af5c, puttvar, r3, r3), 
     inst(0x817af60, puttvar, r4, r4), 
     inst(0x817af64, xsb_execute, 0x8179740, 'thread_request'/5)]).
segment([
     inst(0x817af88, test_heap, 2, 2000), 
     inst(0x817af90, movreg, r1, r6), 
     inst(0x817af94, movreg, r2, r5), 
     inst(0x817af98, putnumcon, r1, 9), 
     inst(0x817afa0, movreg, r6, r2), 
     inst(0x817afa4, puttvar, r3, r3), 
     inst(0x817afa8, puttvar, r4, r4), 
     inst(0x817afac, xsb_execute, 0x8179740, 'thread_request'/5)]).
segment([
     inst(0x817afd0, test_heap, 2, 2000), 
     inst(0x817afd8, movreg, r1, r6), 
     inst(0x817afdc, movreg, r2, r5), 
     inst(0x817afe0, putnumcon, r1, 10), 
     inst(0x817afe8, movreg, r6, r2), 
     inst(0x817afec, puttvar, r3, r3), 
     inst(0x817aff0, puttvar, r4, r4), 
     inst(0x817aff4, xsb_execute, 0x8179740, 'thread_request'/5)]).
segment([
     inst(0x817b018, test_heap, 3, 2000), 
     inst(0x817b020, switchonbound, r1, 135771376, 7), 
     inst(0x817b02c, try, 3, 0x817b044), 
     inst(0x817b034, retry, 3, 0x817b060), 
     inst(0x817b03c, trust, 3, 0x817b07c), 
     inst(0x817b044, getcon, r1, 0x817a72c), 
     inst(0x817b04c, movreg, r2, r1), 
     inst(0x817b050, putnumcon, r2, 1), 
     inst(0x817b058, xsb_execute, 0x817a6b8, 'xsb_mutex_init_np0'/3), 
     inst(0x817b060, getcon, r1, 0x817a774), 
     inst(0x817b068, movreg, r2, r1), 
     inst(0x817b06c, putnumcon, r2, 2), 
     inst(0x817b074, xsb_execute, 0x817a6b8, 'xsb_mutex_init_np0'/3), 
     inst(0x817b07c, getcon, r1, 0x817a7bc), 
     inst(0x817b084, movreg, r2, r1), 
     inst(0x817b088, putnumcon, r2, 3), 
     inst(0x817b090, xsb_execute, 0x817a6b8, 'xsb_mutex_init_np0'/3), 
     hash_table([
          hash_entry(0x817b4f0,817b07c), 
          hash_entry(0x817b4f4,8110b6c), 
          hash_entry(0x817b4f8,8110b6c), 
          hash_entry(0x817b4fc,817b044), 
          hash_entry(0x817b500,8110b6c), 
          hash_entry(0x817b504,817b060), 
          hash_entry(0x817b508,8110b6c)])]).
segment([
     inst(0x817b0b0, test_heap, 2, 2000), 
     inst(0x817b0b8, movreg, r1, r6), 
     inst(0x817b0bc, movreg, r2, r5), 
     inst(0x817b0c0, putnumcon, r1, 11), 
     inst(0x817b0c8, movreg, r6, r2), 
     inst(0x817b0cc, puttvar, r3, r3), 
     inst(0x817b0d0, puttvar, r4, r4), 
     inst(0x817b0d4, xsb_execute, 0x8179740, 'thread_request'/5)]).
segment([
     inst(0x817b0f8, test_heap, 2, 2000), 
     inst(0x817b100, movreg, r1, r6), 
     inst(0x817b104, movreg, r2, r5), 
     inst(0x817b108, putnumcon, r1, 12), 
     inst(0x817b110, movreg, r6, r2), 
     inst(0x817b114, puttvar, r3, r3), 
     inst(0x817b118, puttvar, r4, r4), 
     inst(0x817b11c, xsb_execute, 0x8179740, 'thread_request'/5)]).
segment([
     inst(0x817b140, test_heap, 1, 2000), 
     inst(0x817b148, puttvar, r2, r2), 
     inst(0x817b14c, puttvar, r3, r3), 
     inst(0x817b150, xsb_execute, 0x8179c08, 'xsb_thread_create'/3)]).
segment([
     inst(0x817b170, test_heap, 2, 2000), 
     inst(0x817b178, puttvar, r3, r3), 
     inst(0x817b17c, xsb_execute, 0x8179c08, 'xsb_thread_create'/3)]).
segment([
     inst(0x817b1a0, test_heap, 1, 2000), 
     inst(0x817b1a8, puttvar, r2, r2), 
     inst(0x817b1ac, xsb_execute, 0x8179c58, 'xsb_thread_exit'/2)]).
segment([
     inst(0x817b1d0, test_heap, 0, 2000), 
     inst(0x817b1d8, putnumcon, r1, 0), 
     inst(0x817b1e0, puttvar, r2, r2), 
     inst(0x817b1e4, xsb_execute, 0x8179c58, 'xsb_thread_exit'/2)]).
segment([
     inst(0x817b208, test_heap, 2, 2000), 
     inst(0x817b210, puttvar, r3, r3), 
     inst(0x817b214, xsb_execute, 0x8179ca8, 'xsb_thread_join'/3)]).
segment([
     inst(0x817b238, test_heap, 1, 2000), 
     inst(0x817b240, puttvar, r2, r2), 
     inst(0x817b244, puttvar, r3, r3), 
     inst(0x817b248, xsb_execute, 0x8179ca8, 'xsb_thread_join'/3)]).
segment([
     inst(0x817b268, test_heap, 1, 2000), 
     inst(0x817b270, puttvar, r2, r2), 
     inst(0x817b274, xsb_execute, 0x8179cf8, 'xsb_thread_detach'/2)]).
segment([
     inst(0x817b298, test_heap, 1, 2000), 
     inst(0x817b2a0, puttvar, r2, r2), 
     inst(0x817b2a4, xsb_execute, 0x8179d48, 'xsb_thread_self'/2)]).
segment([
     inst(0x817b2c8, test_heap, 1, 2000), 
     inst(0x817b2d0, puttvar, r2, r2), 
     inst(0x817b2d4, xsb_execute, 0x8179d98, 'xsb_mutex_init'/2)]).
segment([
     inst(0x817b2f8, test_heap, 2, 2000), 
     inst(0x817b300, puttvar, r3, r3), 
     inst(0x817b304, xsb_execute, 0x8179de8, 'xsb_mutex_init_np'/3)]).
segment([
     inst(0x817b328, test_heap, 1, 2000), 
     inst(0x817b330, puttvar, r2, r2), 
     inst(0x817b334, xsb_execute, 0x8179e38, 'xsb_mutex_lock'/2)]).
segment([
     inst(0x817b358, test_heap, 1, 2000), 
     inst(0x817b360, puttvar, r2, r2), 
     inst(0x817b364, xsb_execute, 0x8179e88, 'xsb_mutex_trylock'/2)]).
segment([
     inst(0x817b388, test_heap, 1, 2000), 
     inst(0x817b390, puttvar, r2, r2), 
     inst(0x817b394, xsb_execute, 0x8179ed8, 'xsb_mutex_unlock'/2)]).
segment([
     inst(0x817b3b8, test_heap, 1, 2000), 
     inst(0x817b3c0, puttvar, r2, r2), 
     inst(0x817b3c4, xsb_execute, 0x8179f28, 'xsb_mutex_destroy'/2)]).
segment([
     inst(0x817b3e8, test_heap, 1, 2000), 
     inst(0x817b3f0, movreg, r1, r2), 
     inst(0x817b3f4, putnumcon, r1, 11), 
     inst(0x817b3fc, puttvar, r3, r3), 
     inst(0x817b400, puttvar, r4, r4), 
     inst(0x817b404, puttvar, r5, r5), 
     inst(0x817b408, xsb_execute, 0x8179740, 'thread_request'/5)]).
segment([
     inst(0x817b428, test_heap, 1, 2000), 
     inst(0x817b430, movreg, r1, r2), 
     inst(0x817b434, putnumcon, r1, 12), 
     inst(0x817b43c, puttvar, r3, r3), 
     inst(0x817b440, puttvar, r4, r4), 
     inst(0x817b444, puttvar, r5, r5), 
     inst(0x817b448, xsb_execute, 0x8179740, 'thread_request'/5)]).
segment([
     inst(0x817b468, test_heap, 1, 2000), 
     inst(0x817b470, try, 1, 0x817b480), 
     inst(0x817b478, trust, 1, 0x817b4b4), 
     inst(0x817b480, allocate_gc, 2, 3), 
     inst(0x817b484, getpvar, 2, r1), 
     inst(0x817b488, call, 3, 0x817a440, 'ts_init_tables'/0), 
     inst(0x817b490, putpval, 2, r1), 
     inst(0x817b494, call, 3, 0x8179828, 'call'/1), 
     inst(0x817b49c, putnumcon, r1, 0), 
     inst(0x817b4a4, puttvar, r2, r2), 
     inst(0x817b4a8, deallocate), 
     inst(0x817b4ac, xsb_execute, 0x8179c58, 'xsb_thread_exit'/2), 
     inst(0x817b4b4, putnumcon, r1, 0), 
     inst(0x817b4bc, puttvar, r2, r2), 
     inst(0x817b4c0, xsb_execute, 0x8179c58, 'xsb_thread_exit'/2)]).
segment([
     inst(0x817b528, test_heap, 0, 2000), 
     inst(0x817b530, allocate_gc, 2, 3), 
     inst(0x817b534, putstr, r1, 0x817a840), 
     inst(0x817b53c, bldavar), 
     inst(0x817b540, putpvar, 2, r2), 
     inst(0x817b544, builtin, '11', term_psc), 
     inst(0x817b548, putnumcon, r1, 63), 
     inst(0x817b550, putuval, 2, r2), 
     inst(0x817b554, deallocate), 
     inst(0x817b558, builtin, '18', stat_set_flag), 
     inst(0x817b55c, proceed)]).
segment([
     inst(0x817b578, test_heap, 2, 2000), 
     inst(0x817b580, putnumcon, r3, 21), 
     inst(0x817b588, addreg, r3, r1), 
     inst(0x817b58c, putnumcon, r3, 1), 
     inst(0x817b594, subreg, r3, r1), 
     inst(0x817b598, gettval, r2, r1), 
     inst(0x817b59c, movreg, r2, r3), 
     inst(0x817b5a0, putnumcon, r4, 21), 
     inst(0x817b5a8, subreg, r4, r3), 
     inst(0x817b5ac, jumplt, r3, 0x8110b6c), 
     inst(0x817b5b4, putnumcon, r3, 40), 
     inst(0x817b5bc, subreg, r3, r2), 
     inst(0x817b5c0, jumpge, r2, 0x8110b6c), 
     inst(0x817b5c8, proceed)]).
segment([
     inst(0x817b5e8, test_heap, 1, 2000), 
     inst(0x817b5f0, gettbreg, r2), 
     inst(0x817b5f4, test_heap, 2, 2000), 
     inst(0x817b5fc, try, 2, 0x817b614), 
     inst(0x817b604, retry, 2, 0x817b65c), 
     inst(0x817b60c, trust, 2, 0x817b674), 
     inst(0x817b614, allocate_gc, 2, 5), 
     inst(0x817b618, getpvar, 2, r2), 
     inst(0x817b61c, putpvar, 3, r2), 
     inst(0x817b620, call, 5, 0x817a930, 'user_mutex'/2), 
     inst(0x817b628, putpval, 3, r1), 
     inst(0x817b62c, putpvar, 4, r2), 
     inst(0x817b630, call, 5, 0x817a7e0, 'xsb_sys_mutex_lock'/2), 
     inst(0x817b638, putuval, 4, r1), 
     inst(0x817b63c, putnumcon, r2, 0), 
     inst(0x817b644, term_comp, r1, r2, r3), 
     inst(0x817b648, jumpnz, r3, 0x8110b6c), 
     inst(0x817b650, putpbreg, 2), 
     inst(0x817b654, deallocate), 
     inst(0x817b658, proceed), 
     inst(0x817b65c, movreg, r1, r3), 
     inst(0x817b660, putstr, r1, 0x817a9f8), 
     inst(0x817b668, bldtval, r3), 
     inst(0x817b66c, xsb_execute, 0x8179870, 'writeln'/1), 
     inst(0x817b674, movreg, r1, r3), 
     inst(0x817b678, putstr, r1, 0x817aa50), 
     inst(0x817b680, bldtval, r3), 
     inst(0x817b684, xsb_execute, 0x8179870, 'writeln'/1)]).
segment([
     inst(0x817b6a8, test_heap, 1, 2000), 
     inst(0x817b6b0, gettbreg, r2), 
     inst(0x817b6b4, test_heap, 2, 2000), 
     inst(0x817b6bc, allocate_gc, 2, 5), 
     inst(0x817b6c0, getpvar, 2, r2), 
     inst(0x817b6c4, putpvar, 3, r2), 
     inst(0x817b6c8, call, 5, 0x817a930, 'user_mutex'/2), 
     inst(0x817b6d0, putpval, 3, r1), 
     inst(0x817b6d4, putpvar, 4, r2), 
     inst(0x817b6d8, call, 5, 0x817a810, 'xsb_sys_mutex_unlock'/2), 
     inst(0x817b6e0, putuval, 4, r1), 
     inst(0x817b6e4, putnumcon, r2, 0), 
     inst(0x817b6ec, term_comp, r1, r2, r3), 
     inst(0x817b6f0, jumpnz, r3, 0x8110b6c), 
     inst(0x817b6f8, putpbreg, 2), 
     inst(0x817b6fc, deallocate), 
     inst(0x817b700, proceed)]).
segment([
     inst(0x817b720, test_heap, 1, 2000), 
     inst(0x817b728, getlist, r1), 
     inst(0x817b72c, uninumcon, 36), 
     inst(0x817b734, unitvar, r1), 
     inst(0x817b738, getlist, r1), 
     inst(0x817b73c, uninumcon, 116), 
     inst(0x817b744, unitvar, r2), 
     inst(0x817b748, getlist, r2), 
     inst(0x817b74c, uninumcon, 115), 
     inst(0x817b754, unitvar, r1), 
     inst(0x817b758, getlist, r1), 
     inst(0x817b75c, uninumcon, 95), 
     inst(0x817b764, uninil), 
     inst(0x817b768, proceed)]).
segment([
     inst(0x817b788, test_heap, 2, 2000), 
     inst(0x817b790, allocate_gc, 2, 9), 
     inst(0x817b794, getpvar, 2, r1), 
     inst(0x817b798, getpvar, 3, r2), 
     inst(0x817b79c, putpvar, 4, r1), 
     inst(0x817b7a0, call, 9, 0x817a218, 'xsb_thread_self'/1), 
     inst(0x817b7a8, putpval, 4, r1), 
     inst(0x817b7ac, putpvar, 5, r2), 
     inst(0x817b7b0, call, 9, 0x8179ad8, 'name'/2), 
     inst(0x817b7b8, putpvar, 6, r1), 
     inst(0x817b7bc, call, 9, 0x817aab0, 'ts_prefix'/1), 
     inst(0x817b7c4, putpval, 6, r1), 
     inst(0x817b7c8, putpval, 5, r2), 
     inst(0x817b7cc, putpvar, 7, r3), 
     inst(0x817b7d0, call, 9, 0x8179900, 'append'/3), 
     inst(0x817b7d8, putpvar, 8, r1), 
     inst(0x817b7dc, putpval, 7, r2), 
     inst(0x817b7e0, call, 9, 0x8179ad8, 'name'/2), 
     inst(0x817b7e8, putlist, r3), 
     inst(0x817b7ec, bldpval, 2), 
     inst(0x817b7f0, bldnil), 
     inst(0x817b7f4, putdval, 3, r1), 
     inst(0x817b7f8, putlist, r2), 
     inst(0x817b7fc, bldpval, 8), 
     inst(0x817b800, bldtval, r3), 
     inst(0x817b804, deallocate), 
     inst(0x817b808, builtin, '202', univ), 
     inst(0x817b80c, proceed)]).
segment([
     inst(0x817b828, test_heap, 1, 2000), 
     inst(0x817b830, gettbreg, r2), 
     inst(0x817b834, test_heap, 2, 2000), 
     inst(0x817b83c, try, 2, 0x817b84c), 
     inst(0x817b844, trust, 2, 0x817b88c), 
     inst(0x817b84c, allocate_gc, 2, 4), 
     inst(0x817b850, getstr, r1, 0x81790a0), 
     inst(0x817b858, unitvar, r1), 
     inst(0x817b85c, unipvar, 2), 
     inst(0x817b860, puttbreg, r2), 
     inst(0x817b864, putpvar, 3, r2), 
     inst(0x817b868, call, 4, 0x817ab28, 'make_ts_goal'/2), 
     inst(0x817b870, putstr, r1, 0x81790a0), 
     inst(0x817b878, bldpval, 3), 
     inst(0x817b87c, bldpval, 2), 
     inst(0x817b880, deallocate), 
     inst(0x817b884, xsb_execute, 0x8179990, 'assert'/1), 
     inst(0x817b88c, allocate_gc, 2, 3), 
     inst(0x817b890, putpvar, 2, r2), 
     inst(0x817b894, call, 3, 0x817ab28, 'make_ts_goal'/2), 
     inst(0x817b89c, putuval, 2, r1), 
     inst(0x817b8a0, deallocate), 
     inst(0x817b8a4, xsb_execute, 0x8179990, 'assert'/1)]).
segment([
     inst(0x817b8c8, test_heap, 1, 2000), 
     inst(0x817b8d0, allocate_gc, 2, 3), 
     inst(0x817b8d4, putpvar, 2, r2), 
     inst(0x817b8d8, call, 3, 0x817ab28, 'make_ts_goal'/2), 
     inst(0x817b8e0, putuval, 2, r1), 
     inst(0x817b8e4, deallocate), 
     inst(0x817b8e8, xsb_execute, 0x8179828, 'call'/1)]).
segment([
     inst(0x817b908, test_heap, 1, 2000), 
     inst(0x817b910, allocate_gc, 2, 3), 
     inst(0x817b914, putpvar, 2, r2), 
     inst(0x817b918, call, 3, 0x817ab28, 'make_ts_goal'/2), 
     inst(0x817b920, putuval, 2, r1), 
     inst(0x817b924, deallocate), 
     inst(0x817b928, xsb_execute, 0x8179b20, 'abolish_table_call'/1)]).
segment([
     inst(0x817b948, test_heap, 1, 2000), 
     inst(0x817b950, allocate_gc, 2, 3), 
     inst(0x817b954, putpvar, 2, r2), 
     inst(0x817b958, call, 3, 0x817ab28, 'make_ts_goal'/2), 
     inst(0x817b960, putuval, 2, r1), 
     inst(0x817b964, deallocate), 
     inst(0x817b968, xsb_execute, 0x81792f0, 'tnot'/1)]).
segment([
     inst(0x817b988, test_heap, 0, 2000), 
     inst(0x817b990, allocate_gc, 2, 6), 
     inst(0x817b994, putnumcon, r1, 2), 
     inst(0x817b99c, call, 6, 0x8179f78, 'xsb_sys_mutex_lock'/1), 
     inst(0x817b9a4, putuval, 2, r1), 
     inst(0x817b9a8, putpvar, 3, r2), 
     inst(0x817b9ac, call, 6, 0x817ab28, 'make_ts_goal'/2), 
     inst(0x817b9b4, putpval, 3, r1), 
     inst(0x817b9b8, putpvar, 4, r2), 
     inst(0x817b9bc, putpvar, 5, r3), 
     inst(0x817b9c0, builtin, '200', functor), 
     inst(0x817b9c4, putstr, r1, 0x817ab78), 
     inst(0x817b9cc, bldpval, 4), 
     inst(0x817b9d0, bldpval, 5), 
     inst(0x817b9d4, call, 6, 0x8179a08, 'table'/1), 
     inst(0x817b9dc, putstr, r1, 0x817ab78), 
     inst(0x817b9e4, bldpval, 4), 
     inst(0x817b9e8, bldpval, 5), 
     inst(0x817b9ec, call, 6, 0x81799c0, 'dynamic'/1), 
     inst(0x817b9f4, putnumcon, r1, 2), 
     inst(0x817b9fc, deallocate), 
     inst(0x817ba00, xsb_execute, 0x8179fc8, 'xsb_sys_mutex_unlock'/1)]).
segment([
     inst(0x817ba20, test_heap, 0, 2000), 
     inst(0x817ba28, allocate_gc, 2, 4), 
     inst(0x817ba2c, putuval, 2, r1), 
     inst(0x817ba30, putpvar, 3, r2), 
     inst(0x817ba34, call, 4, 0x817ab28, 'make_ts_goal'/2), 
     inst(0x817ba3c, putuval, 3, r1), 
     inst(0x817ba40, deallocate), 
     inst(0x817ba44, xsb_execute, 0x8179b70, 'abolish_table_pred'/1)]).
segment([
     inst(0x817ba68, test_heap, 0, 2000), 
     inst(0x817ba70, allocate_gc, 2, 4), 
     inst(0x817ba74, putuval, 2, r1), 
     inst(0x817ba78, putpvar, 3, r2), 
     inst(0x817ba7c, call, 4, 0x817ab28, 'make_ts_goal'/2), 
     inst(0x817ba84, putuval, 3, r1), 
     inst(0x817ba88, deallocate), 
     inst(0x817ba8c, xsb_execute, 0x8179bc0, 'abolish'/1)]).
segment([
     inst(0x817bab0, test_heap, 1, 2000), 
     inst(0x817bab8, allocate_gc, 2, 3), 
     inst(0x817babc, putpvar, 2, r2), 
     inst(0x817bac0, call, 3, 0x817abb8, 'par_spawn'/2), 
     inst(0x817bac8, putuval, 2, r1), 
     inst(0x817bacc, deallocate), 
     inst(0x817bad0, xsb_execute, 0x817ac00, 'par_wait'/1)]).
segment([
     inst(0x817baf0, test_heap, 2, 2000), 
     inst(0x817baf8, allocate_gc, 2, 3), 
     inst(0x817bafc, movreg, r1, r4), 
     inst(0x817bb00, movreg, r2, r1), 
     inst(0x817bb04, movreg, r4, r2), 
     inst(0x817bb08, putpvar, 2, r3), 
     inst(0x817bb0c, call, 3, 0x817ac48, 'n_list'/3), 
     inst(0x817bb14, putuval, 2, r1), 
     inst(0x817bb18, deallocate), 
     inst(0x817bb1c, xsb_execute, 0x817a580, 'par_execute'/1)]).
segment([
     inst(0x817bb40, test_heap, 3, 2000), 
     inst(0x817bb48, try, 3, 0x817bb58), 
     inst(0x817bb50, trust, 3, 0x817bb68), 
     inst(0x817bb58, getnumcon, r2, 0), 
     inst(0x817bb60, getnil, r3), 
     inst(0x817bb64, proceed), 
     inst(0x817bb68, getlist, r3), 
     inst(0x817bb6c, unitval, r1), 
     inst(0x817bb70, unitvar, r3), 
     inst(0x817bb74, putnumcon, r4, 1), 
     inst(0x817bb7c, subreg, r4, r2), 
     inst(0x817bb80, jump, 0x817bb40)]).
segment([
     inst(0x817bba0, test_heap, 2, 2000), 
     inst(0x817bba8, switchonterm, r1, 0x817bbc4, 0x817bbd0), 
     inst(0x817bbb4, try, 2, 0x817bbc4), 
     inst(0x817bbbc, trust, 2, 0x817bbd0), 
     inst(0x817bbc4, getnil, r1), 
     inst(0x817bbc8, getnil, r2), 
     inst(0x817bbcc, proceed), 
     inst(0x817bbd0, allocate_gc, 2, 4), 
     inst(0x817bbd4, getlist, r1), 
     inst(0x817bbd8, unitvar, r1), 
     inst(0x817bbdc, unipvar, 2), 
     inst(0x817bbe0, getlist, r2), 
     inst(0x817bbe4, unitvar, r2), 
     inst(0x817bbe8, unipvar, 3), 
     inst(0x817bbec, call, 4, 0x817a0f8, 'xsb_thread_create'/2), 
     inst(0x817bbf4, putdval, 2, r1), 
     inst(0x817bbf8, putdval, 3, r2), 
     inst(0x817bbfc, deallocate), 
     inst(0x817bc00, jump, 0x817bba0)]).
segment([
     inst(0x817bc20, test_heap, 1, 2000), 
     inst(0x817bc28, switchonterm, r1, 0x817bc44, 0x817bc4c), 
     inst(0x817bc34, try, 1, 0x817bc44), 
     inst(0x817bc3c, trust, 1, 0x817bc4c), 
     inst(0x817bc44, getnil, r1), 
     inst(0x817bc48, proceed), 
     inst(0x817bc4c, allocate_gc, 2, 3), 
     inst(0x817bc50, getlist, r1), 
     inst(0x817bc54, unitvar, r1), 
     inst(0x817bc58, unipvar, 2), 
     inst(0x817bc5c, call, 3, 0x817a1b8, 'xsb_thread_join'/1), 
     inst(0x817bc64, putdval, 2, r1), 
     inst(0x817bc68, deallocate), 
     inst(0x817bc6c, jump, 0x817bc20)]).
segment([
     inst(0x817bc90, test_heap, 2, 2000), 
     inst(0x817bc98, gettbreg, r3), 
     inst(0x817bc9c, test_heap, 3, 2000), 
     inst(0x817bca4, try, 3, 0x817bcb4), 
     inst(0x817bcac, trust, 3, 0x817bcf0), 
     inst(0x817bcb4, allocate_gc, 2, 5), 
     inst(0x817bcb8, getpvar, 2, r1), 
     inst(0x817bcbc, getpvar, 3, r2), 
     inst(0x817bcc0, getpvar, 4, r3), 
     inst(0x817bcc4, putpval, 3, r1), 
     inst(0x817bcc8, call, 5, 0x817a020, 'xsb_user_mutex_lock'/1), 
     inst(0x817bcd0, putpval, 2, r1), 
     inst(0x817bcd4, call, 5, 0x8179828, 'call'/1), 
     inst(0x817bcdc, putpbreg, 4), 
     inst(0x817bce0, putdval, 3, r1), 
     inst(0x817bce4, deallocate), 
     inst(0x817bce8, xsb_execute, 0x817a070, 'xsb_user_mutex_unlock'/1), 
     inst(0x817bcf0, movreg, r2, r1), 
     inst(0x817bcf4, xsb_execute, 0x817a070, 'xsb_user_mutex_unlock'/1)]).
segment([
     inst(0x817bd18, test_heap, 1, 2000), 
     inst(0x817bd20, gettbreg, r2), 
     inst(0x817bd24, test_heap, 2, 2000), 
     inst(0x817bd2c, try, 2, 0x817bd3c), 
     inst(0x817bd34, trust, 2, 0x817bd7c), 
     inst(0x817bd3c, allocate_gc, 2, 4), 
     inst(0x817bd40, getpvar, 2, r1), 
     inst(0x817bd44, getpvar, 3, r2), 
     inst(0x817bd48, putnumcon, r1, 20), 
     inst(0x817bd50, call, 4, 0x817a020, 'xsb_user_mutex_lock'/1), 
     inst(0x817bd58, putpval, 2, r1), 
     inst(0x817bd5c, call, 4, 0x8179828, 'call'/1), 
     inst(0x817bd64, putpbreg, 3), 
     inst(0x817bd68, putnumcon, r1, 20), 
     inst(0x817bd70, deallocate), 
     inst(0x817bd74, xsb_execute, 0x817a070, 'xsb_user_mutex_unlock'/1), 
     inst(0x817bd7c, putnumcon, r1, 20), 
     inst(0x817bd84, xsb_execute, 0x817a070, 'xsb_user_mutex_unlock'/1)]).
