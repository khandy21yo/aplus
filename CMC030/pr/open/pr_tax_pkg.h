/*
 * File Layout for: PR.PR_TAX_PKG on 21-May-01
 *
 * Payroll Tax Package Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_tax_pkg_cdd
{
/* Element =
   Description = Tax package number */
	char tax_pkg[2];
/* Element =
   Description = Datatype. FW=Fed, SW=state, etc. */
	char sttype[2];
/* Element =
   Description = State,city, county, school code */
	char code[2];
};
#pragma member_alignment restore
