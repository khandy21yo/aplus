/*
 * File Layout for: OS.OS_CONTROL on 21-May-01
 *
 * OS Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct os_control_cdd
{
/* Element = ORDNUM
   Description = Last Ticket Number */
	char last_ticket[10];
/* Element = DATE
   Description = Last Purge Date (YYYYMMDD) */
	char purgdate[8];
/* Element =
   Description = Activity Status */
	char status_flag[1];
/* Element = PRICETYPE
   Description = Price type for miscellaneous charges */
	char misctype[2];
/* Element = FLAG
   Description = Display Price */
	char dsplprice[1];
/* Element = FLAG
   Description = Display Balance */
	char dsplqty[1];
/* Element = PRICETYPE
   Description = List Price type */
	char listcode[2];
/* Element =
   Description = Wildcard for cust type without balances */
	char cusbal[20];
/* Element = YESNO
   Description = Is this item taxable? */
	char misctaxable[1];
/* Element =
   Description = What customer tax types are exempt */
	char miscexempt[6];
/* Element = PRICETYPE
   Description = Misc. (2) Price type */
	char misc2type[2];
/* Element = YESNO
   Description = Is this item taxable? */
	char misc2taxable[1];
/* Element =
   Description = What tax types are exempt */
	char misc2exempt[6];
};
#pragma member_alignment restore
