/*
 * File Layout for: PR.PR_JWOD_CROSS_REF on 21-May-01
 *
 * Payroll JWOD Cross (Identify JWOD Jobs)
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_jwod_cross_ref_cdd
{
/* Element =
   Description = Sub account */
	char subacct[10];
/* Element =
   Description = J-JWOD, N-NONJWOD */
	char flag[1];
};
#pragma member_alignment restore
