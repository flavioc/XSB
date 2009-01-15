/* File:      hash_xsb.c
** Author(s): Ernie Johnson
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
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


#include "xsb_config.h"
#include "xsb_debug.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "auxlry.h"
#include "context.h"
#include "cell_xsb.h"
#include "hash_xsb.h"
#include "psc_xsb.h"
#include "flags_xsb.h"
#include "memory_xsb.h"
#include "thread_xsb.h"

/*
 *  The "Atom Table" is divided into two structures, based on the type
 *  of the information to be interned.  Symbolic constants, structures,
 *  and predicates which are assigned Psc records are stored in the
 *  "symbol table", while simple strings are stored in the "string table."
 *  Both subtables are maintained as hash tables.
 */

Hash_Table symbol_table = {2053, 0, NULL};
Hash_Table string_table = {16381, 0, NULL};


/*
 *  Prime numbers which are close to powers of 2.  Used for choosing
 *  the next size for a hash table.
 */

#define NUM_OF_PRIMES 16
static unsigned int primes[NUM_OF_PRIMES] =
    {127, 257, 509, 1021, 2053, 4099, 8191, 16381, 32771, 65537, 131071,
       262147, 524287, 1048573, 2097143, 4194301};


/*
 *  Return the next prime number greater than the number received.
 *  If no such prime number can be found, compute an approximate one.
 */

unsigned long next_prime(unsigned long some_int) {

  byte i;

  i = 0;
  while ( (some_int >= primes[i]) && (i < NUM_OF_PRIMES) )
    i++;

  if (i < NUM_OF_PRIMES)
    return (primes[i]);
  else    /* Ran out of prime numbers */
    return (2 * some_int - 1);
}


/*
 *  Hash object on first 25 characters of its name, plus its arity.
 *  Return a bucket number in the range [0 .. (hash_table_size - 1)].
 */

unsigned long hash(const char *obj_name, byte arity, unsigned long hash_table_size) {

  unsigned long hashval, temp;
  int i, j, k;

  hashval = 0;
  if (*obj_name != '\0')
    for (k=0; k<10; k++) {
      for (i = 4; i >= 0; i--) {
	temp = 0;
	for (j = 0; j < 5; j++) {
	  temp = (temp << i) + *obj_name;
	  obj_name++;
	  if (*obj_name == '\0') {
	    hashval = hashval + temp;
	    goto Done;
	  }
	}
	hashval = hashval + temp;
      }
    }
 Done:
  return ((hashval + arity) MOD hash_table_size);
}

/* unsigned long hash(char *obj_name, byte arity, unsigned long hash_table_size) { */

/*   unsigned long hashval, temp; */
/*   int i, j; */

/*   hashval = 0; */
/*   if (*obj_name != '\0') */
/*     for (i = 4; i >= 0; i--) { */
/*       temp = 0; */
/*       for (j = 0; j < 5; j++) { */
/* 	temp = (temp << i) + *obj_name; */
/* 	obj_name++; */
/* 	if (*obj_name == '\0') { */
/* 	  hashval = hashval + temp; */
/* 	  goto Done; */
/* 	} */
/*       } */
/*       hashval = hashval + temp; */
/*     } */
/*  Done: */
/*   return ((hashval + arity) MOD hash_table_size); */
/* } */


/*
 *  Increase the size of the Symbol Table and rehash each entry.
 */

void expand_symbol_table() {

  Pair *new_table, *bucket_ptr, cur_pair, next_pair;
  Psc cur_psc;
  unsigned long index, new_size, new_index;

  new_size = next_prime(symbol_table.size);
  new_table = (Pair *)mem_calloc(new_size, sizeof(void *),ATOM_SPACE);

  for (bucket_ptr = (Pair *)symbol_table.table, index = 0;
       index < symbol_table.size;  bucket_ptr++, index++)

    for (cur_pair = *bucket_ptr; cur_pair != NULL; cur_pair = next_pair) {
      next_pair = pair_next(cur_pair);
      cur_psc = pair_psc(cur_pair);
      new_index = hash(get_name(cur_psc), get_arity(cur_psc), new_size);
      pair_next(cur_pair) = new_table[new_index];
      new_table[new_index] = cur_pair;
    }

  mem_dealloc((void *)symbol_table.table,symbol_table.size,ATOM_SPACE);
  symbol_table.size = new_size;
  symbol_table.table = (void **)new_table;
  /*printf("expanded atom table to: %d\n",new_size);*/
}


/*
 *  Increase the size of the String Table and rehash each entry.
 *                      
 *  String Table entries have the form:
 *           +--------------------------+
 *           | Ptr_to_Next | String ... |
 *           +--------------------------+
 */

void expand_string_table() {

  void **new_table, **bucket_ptr, **cur_entry, **next_entry;
  char *string;
  unsigned long index, new_size, new_index;

  new_size = next_prime(string_table.size);
  new_table = (void **)mem_calloc(new_size, sizeof(void *),STRING_SPACE);

  for (bucket_ptr = string_table.table, index = 0;
       index < string_table.size;
       bucket_ptr++, index++)
    for (cur_entry = (void **)*bucket_ptr;
	 cur_entry != NULL;
	 cur_entry = next_entry) {
      next_entry = (void **)*cur_entry;
      string = (char *)(cur_entry + 1);
      new_index = hash(string, 0, new_size);
      *cur_entry = new_table[new_index];
      new_table[new_index] = (void *)cur_entry;
    }

  mem_dealloc((void *)string_table.table,string_table.size*sizeof(void *),STRING_SPACE);
  string_table.size = new_size;
  string_table.table = new_table;
  //  printf("expanded string table to: %ld\n",new_size);
}


/*
 *  Collect and report statistics on the symbol and string tables:
 *   - number of used and unused buckets
 *   - first and last buckets containing objects
 *   - buckets which are most full
 */

void symbol_table_stats(CTXTdecl) {

  unsigned long   i, symbols, bucket_contains, used_buckets, unused_buckets,
                  fullest_bucket_size, fullest_bucket_num, last_index;
  int first_index;
  Pair pair_ptr;

  SYS_MUTEX_LOCK( MUTEX_SYMBOL ) ;

  symbols = used_buckets = unused_buckets = last_index = 0;
  fullest_bucket_size = fullest_bucket_num = 0;
  first_index = -1;
  
  for (i = 0; i < symbol_table.size; i++) {
    if (symbol_table.table[i] != NULL) {
      if (first_index == -1)
	first_index = i;
      last_index = i;
      used_buckets++;
      bucket_contains = 0;
      for (pair_ptr = (Pair)symbol_table.table[i];   pair_ptr != NULL;
	   pair_ptr = pair_next(pair_ptr)) {
	symbols++;
	bucket_contains++;
      }
      if (bucket_contains > fullest_bucket_size) {
	fullest_bucket_size = bucket_contains;
	fullest_bucket_num = i;
      }
    }
    else
      unused_buckets++;
  }
  printf("\nSymbol table statistics:");
  printf("\n------------------------\n");
  printf("Table Size:\t%lu\n", symbol_table.size);
  printf("Total Symbols:\t%lu\n", symbols);
  if (symbols != symbol_table.contains)
    printf("Symbol count incorrect in 'symbol_table': %lu\n",
	   symbol_table.contains);
  printf("\tused buckets:\t%lu  (range: [%d, %lu])\n", used_buckets,
	 first_index, last_index);
  printf("\tunused buckets:\t%lu\n", unused_buckets);
  printf("\tmaximum bucket size:\t%lu  (#: %lu)\n", fullest_bucket_size, 
	 fullest_bucket_num);

 SYS_MUTEX_UNLOCK( MUTEX_SYMBOL ) ;

}  


void string_table_stats(CTXTdecl) {

  unsigned long   i, strings, bucket_contains, used_buckets, unused_buckets,
                  fullest_bucket_size, fullest_bucket_num, last_index;
  int first_index;
  void *ptr;

 SYS_MUTEX_LOCK( MUTEX_STRING ) ;

  strings = used_buckets = unused_buckets = last_index = 0;
  fullest_bucket_size = fullest_bucket_num = 0;
  first_index = -1;
  
  for (i = 0; i < string_table.size; i++) {
    if (string_table.table[i] != NULL) {
      if (first_index == -1)
	first_index = i;
      last_index = i;
      used_buckets++;
      bucket_contains = 0;
      for (ptr = string_table.table[i];  ptr != NULL;  ptr = *(void **)ptr) {
	strings++;
	bucket_contains++;
      }
      if (bucket_contains > fullest_bucket_size) {
	fullest_bucket_size = bucket_contains;
	fullest_bucket_num = i;
      }
    }
    else
      unused_buckets++;
  }
  printf("\nString table statistics:");
  printf("\n------------------------\n");
  printf("Table Size:\t%lu\n", string_table.size);
  printf("Total Strings:\t%lu\n", strings);
  if (strings != string_table.contains)
    printf("String count incorrect in 'string_table': %lu\n",
	   string_table.contains);
  printf("\tused buckets:\t%lu  (range: [%d, %lu])\n", used_buckets,
	 first_index, last_index);
  printf("\tunused buckets:\t%lu\n", unused_buckets);
  printf("\tmaximum bucket size:\t%lu  (#: %lu)\n", fullest_bucket_size, 
	 fullest_bucket_num);

 SYS_MUTEX_UNLOCK( MUTEX_STRING ) ;

}

void free_unused_strings() {
  unsigned long i;
  void *ptr, *prevptr;
  int used = 0, unused = 0;

  for (i = 0; i < string_table.size; i++) {
    if (string_table.table[i] != NULL) {
      prevptr = &string_table.table[i];
      ptr = *(void **)prevptr;
      while (ptr != NULL) {
	if ((*(Integer *)ptr) & 1) {
	  used++;
	  (*(Integer *)ptr) &= ~1;
	  prevptr = ptr;
	  ptr = *(void **)ptr;
	} else {
	  unused++;
	  //	  printf("unused: '%s'\n",(char *)ptr+4);
	  *(void **)prevptr = *(void **)ptr;
	  mem_dealloc(ptr,strlen(((char *)ptr)+4)+sizeof(void *)+1,STRING_SPACE);
	  //	  *(((char *)ptr)+4) = '?';
	  ptr = *(void **)prevptr;
	  string_table.contains--;
	  //	  ptr = *(void **)ptr;  // replace with above when have marked all
	}
      }
    }
  }
  //  if (unused > 0) printf("%d",unused);
  //  printf("string_table scanned. used=%d, unused=%d\n",used,unused);
}


  
