#define New_TSTN(TSTN,TrieType,NodeType,Symbol,Parent,Sibling)	\
   TSTN = new_tstn(CTXTc TrieType,NodeType,Symbol,Parent,Sibling)
   
/* For Hashed TSTNs
   ---------------- */
#define TSTN_SetTSIN(pTSTN,TSIN)    TSTN_TimeStamp(pTSTN) = (TimeStamp)(TSIN)
#define TSTN_GetTSIN(pTSTN)	    ((TSINptr)TSTN_TimeStamp(pTSTN))
#define TSTN_GetTSfromTSIN(pTSTN)   TSIN_TimeStamp(TSTN_GetTSIN(pTSTN))

/* For roots of TS Answer Tries
   ---------------------------- */
#define TSTRoot_SetHTList(pTST,pTSTHT)  TSTN_Sibling(pTST) = (TSTNptr)pTSTHT
#define TSTRoot_GetHTList(pTST) ((TSTHTptr)TSTN_Sibling(pTST))

#define EMPTY_TST_TIMESTAMP	0
#define TSTN_DEFAULT_TIMESTAMP	1

/* time stamped indexes */
#define IsTSindexHead(TSIN)		IsNULL(TSIN_Prev(TSIN))
#define IsTSindexTail(TSIN)		IsNULL(TSIN_Next(TSIN))