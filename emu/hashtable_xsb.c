
#include "xsb_config.h"
#include "xsb_debug.h"

#include <stdio.h>
#include <stdlib.h>

#include "auxlry.h"
#include "cell_xsb.h"
#include "psc_xsb.h"
#include "cinterf.h"
#include "trie_internals.h"
#include "macro_xsb.h"
#include "error_xsb.h"

#include "hashtable_xsb.h"
#include "tr_utils.h"

/*
  A simple hashtable.
  The first bucket is integrated into the hashtable and the overflow bullers
  are calloc()'ed. This gives good performance on insertion and search.
  When a collision happens on insertion, it requires a calloc().
  But collisions should be rare. Otherwise, should use a bigger table.
*/


#define table_hash(val, length)    ((word)(val) % (length))

#define get_top_bucket(htable,I) \
    	((xsbBucket *)(htable->table + (htable->bucket_size * I)))
/* these two make sense only for top buckets, because we deallocate overflow
   buckets when they become free. */
#define mark_bucket_free(bucket)   (bucket->name = (Cell)0)
#define is_free_bucket(bucket)     (bucket->name == (Cell)0)

static void init_hashtable(xsbHashTable *table);


xsbBucket *search_bucket(Cell name,
			 xsbHashTable *table,
			 enum xsbHashSearchOp search_op)
{
  xsbBucket *bucket, *prev;

  if (! table->initted) init_hashtable(table);

  prev = NULL;
  bucket = get_top_bucket(table,table_hash(name,table->length));
  while (bucket && bucket->name) {
    if (bucket->name == name) {
      if (search_op == hashtable_delete) {
	if (!prev) {
	  /* if deleting a top bucket, copy the next bucket into the top one
	     and delete that next bucket. If no next, then just nullify name */
	  prev = bucket;
	  bucket=bucket->next;
	  if (bucket) {
	    /* use memcpy() because client bucket might have extra fields */
	    memcpy(prev, bucket, table->bucket_size);
	    free(bucket);
	  } else
	    mark_bucket_free(prev);
	} else {
	  /* Not top bucket: rearrange pointers & free space */
	  prev->next = bucket->next;
	  free(bucket);
	}
	return NULL;
      } else
	return bucket;
    }
    prev = bucket;
    bucket = bucket->next;
  }
  /* not found */
  if (search_op != hashtable_insert) return NULL;
  /* else create new bucket */
  /* calloc nullifies the allocated space; CLIENTS RELY ON THIS */
  if (!bucket) { /* i.e., it is not a top bucket */
    bucket = (xsbBucket *)calloc(1,table->bucket_size);
    if (!bucket)
      xsb_exit("Out of Memory: Can't allocate hash bucket");
    prev->next = bucket;
    /* NOTE: not necessary to nullify bucket->next because of calloc() */
  }
  bucket->name = name;
  return bucket;
}


static void init_hashtable(xsbHashTable *table)
{
  /* calloc zeroes the allocated space; clients rely on this */
  table->table = (char *)calloc(table->length,table->bucket_size);
  if (!table->table)
    xsb_exit("Out of Memory: Can't create hash table");
  table->initted = TRUE;
}

void destroy_hashtable(xsbHashTable *table)
{
  int i;
  xsbBucket *bucket, *next;

  table->initted = FALSE;
  for (i=0; i < table->length; i++) {
    /* follow pointers and free up buckets */
    bucket=get_top_bucket(table,i)->next;
    while (bucket != NULL) {
      next = bucket->next;
      free(bucket);
      bucket = next;
    }
  }
  free(table->table);
}



void show_table_state(xsbHashTable *table)
{
  xsbBucket *bucket;
  int i;

  xsb_dbgmsg("\nCell Status\tOverflow Count\n");
  for (i=0; i < table->length; i++) {
    bucket = get_top_bucket(table,i);
    if (is_free_bucket(bucket)) {
      /* free cell */
      xsb_dbgmsg("   ---\t\t   ---");
    } else {
      int overflow_count=0;

      fprintf(stddbg, "  taken\t\t");
      bucket = bucket->next;
      while (bucket != NULL) {
	overflow_count++;
	bucket = bucket->next;
      }
      xsb_dbgmsg("   %d", overflow_count);
    }
  }
}

