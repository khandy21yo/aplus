/*
 * File Layout for: UTL.UTL_STRING_PRINT on 21-May-01
 *
 * String Print Definitions
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_string_print_cdd
{
/* Element =
   Description = System For Reports (AR,PR,GL...) */
	char system[6];
/* Element =
   Description = User defined grouping */
	char grouping[6];
/* Element =
   Description = Report sequence number */
	char repseq[6];
/* Element =
   Description = Name to select by */
	char titles[20];
/* Element =
   Description = Number of report in report file */
	char repnum[6];
/* Element =
   Description = Settings Flag (Ignore,Set,Query) */
	char flags[10][1];
/* Element =
   Description = For query, code name to ask for */
	char codes[10][6];
/* Element =
   Description = For Query, title. For Set, value */
	char descrs[10][20];
/* Element =
   Description = Output Device */
	char outdev[20];
};
#pragma member_alignment restore
