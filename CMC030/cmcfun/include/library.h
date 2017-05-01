/*
 * LBR calls given with VMS
 */
extern long  lbr$close(unsigned long *Channel);
extern long  lbr$delete_data();
extern long  lbr$delete_key();
extern long  lbr$find();
extern long  lbr$flush();
extern long  lbr$get_index();
extern long  lbr$get_record();
extern long  lbr$ini_control();
extern long  lbr$insert_key();
extern long  lbr$lookup_key();
extern long  lbr$open();
extern long  lbr$put_end();
extern long  lbr$put_record();
extern long  lbr$replace_key();
extern long  lbr$search();

#if 0
#define LIB$C_CREATE	0
#define LBR$C_READ	1
#define LBR$C_UPDATE	2

#define LIB$C_FLUSHALL	0
#define LBR$C_FLUSHDATA	1

#define LBR$C_TYP_TXT	4
#else
#include <lbrdef.h>
#endif

