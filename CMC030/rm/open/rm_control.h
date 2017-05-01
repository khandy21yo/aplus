/*
 * File Layout for: RM.RM_CONTROL on 21-May-01
 *
 * Restaurant Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct rm_control_cdd
{
/* Element = TRANSTYPE
   Description = Transaction type code for issue */
	char ttissue[2];
/* Element = TRANSTYPE
   Description = Transaction type code fot receiver */
	char ttrec[2];
/* Element = TRANSTYPE
   Description = Transaction type code for promotionals */
	char ttprom[2];
/* Element = TRANSTYPE
   Description = Transaction type code for employee meal */
	char ttemp[2];
/* Element = TRANSTYPE
   Description = Transaction type code for sales units */
	char ttsales[2];
/* Element = TRANSTYPE
   Description = Transaction type code for waste */
	char ttwaste[2];
/* Element = PCTYPE
   Description = Price type code for menu price */
	char prcmenu[2];
/* Element = PCTYPE
   Description = Price type code for indented menu price */
	char indmenu[2];
/* Element = PCTYPE
   Description = Price type code for employee menu price */
	char prcemp[2];
/* Element = CONTROLFLAG
   Description = Status flag in the control files */
	char controlflag[1];
};
#pragma member_alignment restore
