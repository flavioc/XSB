

/* clients can define their own bucket structures as long as the top of that
   structure coinsides with xsbBucket */
typedef struct bucket xsbBucket;
struct bucket {
  Cell        name;
  xsbBucket  *next;
};

typedef struct hash_table  xsbHashTable;
struct hash_table {
  int           length;
  int           bucket_size;  
  xsbBool       initted;
  byte         *table;
};

enum  xsbHashSearchOp {hashtable_insert, hashtable_delete, hashtable_find};


extern xsbBucket *search_bucket(Cell name, xsbHashTable *tbl,
				enum xsbHashSearchOp search_op);

extern void destroy_hash_table(xsbHashTable *table);

extern void show_table_state(xsbHashTable *table);
