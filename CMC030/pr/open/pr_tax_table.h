/*
 * File Layout for: PR.PR_TAX_TABLE on 08-Aug-03
 *
 * Payroll Tax Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_tax_table_cdd
{
/* Element =
   Description = Federal,State,County,Munic.,District */
	char auth[1];
/* Element =
   Description = Used only if AUTH <> "Federal" */
	char code[2];
/* Element =
   Description = */
	char tstatus[1];
/* Element =
   Description = Employer FICA percentage OASDI */
	int fica_empr_pct;
/* Element =
   Description = Employee FICA percent OASDI */
	int fica_empe_pct;
/* Element =
   Description = FICA limit OASDI */
	double fica_limit;
/* Element =
   Description = Federal or <blank> */
	char calc_basis[1];
/* Element =
   Description = */
	double basis_pct;
/* Element =
   Description = */
	double ot_anl_min;
/* Element =
   Description = */
	double ot_anl_pct;
/* Element =
   Description = */
	double ot_anl_max;
/* Element =
   Description = */
	double std_wh;
/* Element =
   Description = */
	double adj_grs_pct;
/* Element =
   Description = */
	double min_std_adj;
/* Element =
   Description = */
	double max_std_adj;
/* Element =
   Description = */
	double pr_ex;
/* Element =
   Description = */
	double over[12];
/* Element =
   Description = */
	double taxamt[12];
/* Element =
   Description = */
	double plus[12];
/* Element =
   Description = */
	double sui_min;
/* Element =
   Description = */
	double sui_pct;
/* Element =
   Description = */
	double sui_max;
/* Element =
   Description = */
	double ot_ded_max;
/* Element =
   Description = Employer FICA HI percentage */
	int fica_empr_pct_hi;
/* Element =
   Description = Employee FICA HI percentage */
	int fica_empe_pct_hi;
/* Element =
   Description = FICA HI limit */
	double fica_limit_hi;
/* Element =
   Description = Personal Exemption for Additional Exempt */
	double pr_ex_add;
/* Element =
   Description = How to round resulting tax */
	char drounding[1];
/* Element =
   Description = Old field: Federal Exemption Threshold */
	char threshold[7];
/* Element =
   Description = Minimum Annual Prorated Tax */
	double mintax;
/* Element =
   Description = Low income Exemption */
	double low_income;
/* Element =
   Description = Credit for Federal Witholding */
	double fed_credit_pct;
};
#pragma member_alignment restore
