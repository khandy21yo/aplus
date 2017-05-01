/*
 * File Layout for: GL.GL_FINSTA on 21-May-01
 *
 * Financial Statement Command File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_finsta_cdd
{
/* Element =
   Description = */
	char prompt[6];
/* Element =
   Description = */
	char descr[30];
/* Element =
   Description = */
	char reptitle[50];
/* Element =
   Description = */
	char cmdfil[30];
/* Element =
   Description = */
	char fintype[1];
/* Element =
   Description = */
	char fincmd[9][20];
/* Element =
   Description = */
	char reptitlea[50];
/* Element =
   Description = */
	char reptitleb[50];
/* Element =
   Description = */
	char reptitlec[50];
/* Element =
   Description = */
	char reptitled[50];
};
#pragma member_alignment restore
