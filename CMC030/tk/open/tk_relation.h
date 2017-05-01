/*
 * File Layout for: TK.TK_RELATION on 21-May-01
 *
 * Module Relation File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tk_relation_cdd
{
/* Element = MODNAME
   Description = Module name */
	char parent[39];
/* Element = MODNAME
   Description = Module name */
	char child[39];
/* Element =
   Description = Number submodules in the module */
	int quantity;
/* Element =
   Description = Defining reference */
	char defref[1];
/* Element = DATE
   Description = Creating date */
	char cdate[8];
/* Element = TIME
   Description = Creating time */
	char ctime[6];
};
#pragma member_alignment restore
