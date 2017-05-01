/*
 * File Layout for: PR.PR_SKILLS on 21-May-01
 *
 * Skill Descriptions
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_skills_cdd
{
/* Element =
   Description = Employee Skill */
	char skill[6];
/* Element = DESCRIPTION
   Description = Skill Description */
	char description[40];
/* Element =
   Description = EEO Sort Order */
	char eeosort[2];
};
#pragma member_alignment restore
