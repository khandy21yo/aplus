1	%TITLE "Verify W-2 Tape"
	%SBTTL "PR_SPEC_TAX_VERIFY"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987, 1988 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies thereof may not be provided or otherwise made
	! available to any other person.  No title to and ownership of
	! the software is hereby transferred.
	!
	! The information in this software is subject to change without
	! notice and should not be construed as a commitment by
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! Abstract:HELP
	!	.p
	!	The ^*Verify W-2 Tape\* option in the W-2 Processing menu provides
	!	the means to verify the accuracy of the W-2 data written on
	!	magnetic tape, or into a text file for floppy disk or magnetic tape.
	!	.P
	!	If you are reading from a magtape,
	!	you need to load the W2 magtape in the
	!	drive and mount it (at the *$ prompt) using the following
	!	command:
	!	.b
	!	.lm +10
	!	^*MOUNT MUA0:/FOREIGN/BLOCKSIZE=6900\*
	!	.LM -10
	!	.B
	!	and then start up the program. Replace ^*MUA0:\* whith the proper
	!	drive name if it is different on your system.
	!	.p
	!	The program will then ask for the device name. Enter either the tape
	!	drive name, or the filname containing the information.
	!	.p
	!	After running this option, dismount the tape (again at the *$) using the
	!	following command:
	!	.b
	!	.lm +10
	!	^*DISMOUNT MUA0:\*
	!	.lm -10
	!	.b
	!	and take the tape out of the drive.
	!
	! Index:
	!	.x W-2 Tape>Verify
	!	.x Verify>W-2 Tape
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_TAX_VERIFY
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_SPEC_TAX_VERIFY, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_TAX_VERIFY.OBJ;*
	!
	! Author:
	!
	!	12/05/87 - Robert Peterson
	!
	! Modification history:
	!
	!	01/20/89 - Kevin Handy
	!		Modified to work with tape instead of a disk
	!		file.
	!
	!	02/10/89 - Kevin Handy
	!		Modified for changed in ENTR_NOLSTRING
	!
	!	09/23/89 - Kevin Handy
	!		Modified to use ENTR_ENTER instead of
	!		ENTR_NOLSTRING.
	!
	!	02/26/90 - Kevin Handy
	!		Modified to use begin/end pastboard update
	!		to speed up display of tape.
	!
	!	06/04/91 - Kevin Handy
	!		Removed junk in error trapping.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_FUNC_SUBJECT com definition.
	!
	!	02/11/92 - Kevin Handy
	!		Modified to use record instead of maps.
	!
	!	02/11/92 - Kevin Handy
	!		Modified to be able to read both tapes and
	!		text files.
	!
	!	02/24/93 - Kevin Handy
	!		Modified to also handle floppy disk formatted
	!		files. (Record length will be 128)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/05/99 - Kevin Handy
	!		Change field 'OPTIONAL' to 'XOPTIONAL'
	!
	!	06/09/99 - Kevin Handy
	!		Lose NoFile (Dead code)
	!
	!	10/12/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	01/16/2000 - Kevin Handy
	!		Inserted the w2 format instead of %including it
	!		so that I can keep a backup of the previous
	!		years format easier.
	!
	!	02/07/2005 - Kevin Handy
	!		Add record types for newer (512) character
	!		MREF formats.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Define maps
	!
 !	%INCLUDE "SOURCE:[PR.OPEN]PR_TAXTAPE.INC"

	!*******************************************************************
	! Define records for the various codes used by tax tape
	!*******************************************************************
	!
	!	01/15/96 - Kevin Handy
	!		Change a couble of state fields from 10 characters
	!		to two characters. Some numeric fields also ate
	!		a preceeding space.
	!
	! Transmitter record
	!
	RECORD CODE_A_STRUCT
		STRING RECID		= 1%
		STRING PAYYR		= 4%
		STRING EIN		= 9%
		STRING FILL		= 8%
		STRING FORADD		= 1%
		STRING ENAME		= 50%
		STRING ADD		= 40%
		STRING CITY		= 25%
		STRING ST		= 2%
		STRING FILL		= 13%
		STRING ZIP		= 5%
		STRING ZIPEXT		= 5%
		STRING FILL		= 113%
	END RECORD

	RECORD CODE_1A_STRUCT
		STRING RECID		= 2%
		STRING PAYYR		= 4%
		STRING EIN		= 9%
		STRING FILL		= 8%
		STRING FORADD		= 1%
		STRING ENAME		= 50%
		STRING ADD		= 40%
		STRING FILL		= 14%
	END RECORD

	RECORD CODE_2A_STRUCT
		STRING RECID		= 2%
		STRING CITY		= 25%
		STRING ST		= 2%
		STRING FILL		= 8%
		STRING ZIPEXT		= 5%
		STRING ZIP		= 5%
		STRING FILL		= 81%
	END RECORD

	!
	! Basic Authorization records
	!
	RECORD CODE_B_STRUCT
		STRING RECID		= 1%
		STRING PAYYR		= 4%
		STRING EIN		= 9%
		STRING COMTYP		= 8%
		STRING LABEL		= 2%
		STRING FILL		= 1%
		STRING DENSITY		= 2%
		STRING RECCOD		= 3%
		STRING FILL		= 115%
		STRING FORADD		= 1%
		STRING ENAME		= 44%
		STRING ADD		= 35%
		STRING CITY		= 20%
		STRING ST		= 2%
		STRING FILL		= 5%
		STRING ZIP		= 5%
		STRING ZIPEXT		= 5%
		STRING FILL		= 14%
	END RECORD

	RECORD CODE_1B_STRUCT
		STRING RECID		= 2%
		STRING PAYYR		= 4%
		STRING EIN		= 9%
		STRING COMTYP		= 8%
		STRING FILL		= 105%
	END RECORD

	RECORD CODE_2B_STRUCT
		STRING RECID		= 2%
		STRING FILL		= 13%
		STRING FORADD		= 1%
		STRING ENAME		= 44%
		STRING ADD		= 35%
		STRING CITY		= 20%
		STRING ST		= 2%
		STRING ZIPEXT		= 5%
		STRING ZIP		= 5%
		STRING FILL		= 1%
	END RECORD

	!
	! Employer Records
	!
	RECORD CODE_E_STRUCT
		STRING RECID		= 1%
		STRING PAYYR		= 4%
		STRING EIN		= 9%
		STRING ESIN		= 9%
		STRING ENAME		= 50%
		STRING ADD		= 40%
		STRING CITY		= 25%
		STRING STATE		= 2%
		STRING FILL		= 8%
		STRING ZIPEXT		= 5%
		STRING ZIP		= 5%
		STRING NAMCODE		= 1%
		STRING TYPEMP		= 1%
		STRING BLKFAC		= 2%
		STRING PRU		= 4%
		STRING FILL		= 1%
		STRING THIRDPARTY	= 12%
		STRING FILL		= 75%
		STRING SLL		= 1%
		STRING FORADD		= 1%
		STRING FILL		= 1%
		STRING OTHEREIN		= 9%
		STRING FILL		= 10%
	END RECORD

	RECORD CODE_1E_STRUCT
		STRING RECID		= 2%
		STRING PAYYR		= 4%
		STRING EIN		= 9%
		STRING ESIN		= 9%
		STRING ENAME		= 50%
		STRING ADD		= 40%
		STRING FORADD		= 1%
		STRING FILL		= 13%
	END RECORD

	RECORD CODE_2E_STRUCT
		STRING RECID		= 2%
		STRING CITY		= 25%
		STRING STATE		= 2%
		STRING FILL		= 8%
		STRING ZIPEXT		= 5%
		STRING ZIP		= 5%
		STRING NAMCODE		= 1%
		STRING TYPEMP		= 1%
		STRING FILL		= 2%
		STRING PRU		= 4%
		STRING SLL		= 1%
		STRING FILL		= 1%
		STRING OTHEREIN		= 9%
		STRING FILL		= 1%
		STRING THIRDPARTY	= 12%
		STRING FILL		= 49%
	END RECORD

	!
	! Employee Wage Records
	!
	RECORD CODE_W_STRUCT
		STRING RECID		= 1%
		STRING SSN		= 9%
		STRING ENAME		= 27%
		STRING ADD		= 40%
		STRING CITY		= 25%
		STRING ST		= 2%
		STRING FILL		= 8%
		STRING ZIPEXT		= 5%
		STRING ZIP		= 5%
		STRING SEC		= 1%
		STRING FICA_WAGE	= 7%
		STRING FILL		= 1%
		STRING FICA_TIP		= 7%
		STRING FILL		= 1%
		STRING ANNWAGE		= 9%
		STRING FILL		= 1%
		STRING FICA_WTHLD	= 6%
		STRING FED_WTHLD	= 9%
		STRING FILL		= 1%
		STRING ALLTIP		= 7%
		STRING FILL		= 1%
		STRING FR_BEN		= 9%
		STRING MWAT		= 9%
		STRING MTW		= 7%
		STRING FILL		= 8%
		STRING NQP457		= 9%
		STRING FILL		= 1%
		STRING NQPN457		= 9%
		STRING FILL		= 1%
		STRING DCB		= 7%
		STRING CN		= 7%
		STRING GTL		= 7%
		STRING UNCOLL_FICA	= 7%
		STRING EIC		= 7%
		STRING FILL		= 1%
		STRING PENPLAN_FLAG	= 1%
		STRING FILL		= 1%
		STRING DEFCOMP_FLAG	= 1%
		STRING FILL		= 1%
		STRING DEFCOMP		= 9%
		STRING FILL		= 1%
	END RECORD

	RECORD CODE_1W_STRUCT
		STRING RECID		= 2%
		STRING SSN		= 9%
		STRING ENAME		= 27%
		STRING ADD		= 40%
		STRING CITY		= 25%
		STRING ST		= 2%
		STRING FILL		= 8%
		STRING ZIPEXT		= 5%
		STRING ZIP		= 5%
		STRING SEC		= 1%
		STRING FILL		= 4%
	END RECORD

	RECORD CODE_2W_STRUCT
		STRING RECID		= 2%
		STRING FICA_WAGE	= 7%
		STRING FILL		= 1%
		STRING FICA_TIP		= 7%
		STRING FILL		= 1%
		STRING ANNWAGE		= 9%
		STRING FILL		= 1%
		STRING FICA_WTHLD	= 6%
		STRING FED_WTHLD	= 9%
		STRING NQP457		= 9%
		STRING FILL		= 1%
		STRING NQPN457		= 9%
		STRING CN		= 7%
		STRING GTL		= 7%
		STRING UNCOLL_FICA	= 7%
		STRING EIC		= 7%
		STRING ALLTIP		= 7%
		STRING FR_BEN		= 9%
		STRING FILL		= 1%
		STRING PENPLAN_FLAG	= 1%
		STRING FILL		= 1%
		STRING DEFCOMP_FLAG	= 1%
		STRING FILL		= 1%
		STRING DEFCOMP		= 9%
		STRING FILL		= 1%
		STRING DCB		= 7%
	END RECORD

	RECORD CODE_3W_STRUCT
		STRING RECID		= 2%
		STRING MWAT		= 9%
		STRING MTW		= 7%
		STRING FILL		= 110%
	END RECORD

	!
	! Supplimantal records
	!
	RECORD CODE_S_STRUCT
		STRING RECID		= 1%
		STRING SSN		= 9%
		STRING ENAME		= 27%
		STRING ADD		= 40%
		STRING CITY		= 25%
		STRING ST		= 2%
		STRING FILL		= 8%
		STRING ZIPEXT		= 5%
		STRING ZIP		= 5%
		STRING FILL		= 1%
		STRING STCODE1		= 2%
		STRING XOPTIONAL	= 2%
		STRING REPPER		= 4%
		STRING TOTWAG		= 9%
		STRING SUIWAG		= 9%
		STRING WEEKWORK		= 2%
		STRING HIREDATE		= 4%
		STRING FIREDATE		= 4%
		STRING ENTITYCODE1	= 5%
		STRING SEAN		= 12%
		STRING FILL		= 6%
		STRING STCODE2		= 2%
		STRING STWAGE		= 9%
		STRING STTAX		= 8%
		STRING OTHDATA		= 10%
		STRING TAXTYP		= 1%
		STRING ENTITYCODE2	= 5%
		STRING LOCWAGE		= 9%
		STRING LOCTAX		= 7%
		STRING STCONNUM		= 7%
		STRING FILL		= 36%
	END RECORD

	RECORD CODE_1S_STRUCT
		STRING RECID		= 2%
		STRING SSN		= 9%
		STRING ENAME		= 27%
		STRING ADD		= 40%
		STRING CITY		= 25%
		STRING ST		= 2%
		STRING FILL		= 8%
		STRING ZIPEXT		= 5%
		STRING ZIP		= 5%
		STRING FILL		= 1%
		STRING STCODE1		= 2%
		STRING XOPTIONAL	= 2%
	END RECORD

	RECORD CODE_2S_STRUCT
		STRING RECID		= 2%
		STRING SEAN		= 12%
		STRING REPPER		= 6%		! Was 4
		STRING TOTWAG		= 9%
		STRING SUIWAG		= 9%
		STRING WEEKWORK		= 2%
		STRING HIREDATE		= 6%		! Was 4
		STRING FIREDATE		= 6%		! Was 4
		STRING ENTITYCODE1	= 5%
		STRING STCODE2		= 2%
		STRING STWAGE		= 9%
		STRING STTAX		= 8%
		STRING OTHDATA		= 10%
		STRING TAXTYP		= 1%
		STRING ENTITYCODE2	= 5%
		STRING LOCWAGE		= 9%
		STRING LOCTAX		= 7%
		STRING STCONNUM		= 7%
		STRING FILL		= 19%
	END RECORD

	!
	! Intermediate Total
	!
	RECORD CODE_I_STRUCT
		STRING RECID		= 1%
		STRING FICA_WAGE	= 10%
		STRING FILL		= 1%
		STRING FICA_TIP		= 10%
		STRING FILL		= 1%
		STRING ANNWAGE		= 10%
		STRING FILL		= 1%
		STRING FICA_WTHLD	= 10%
		STRING FILL		= 1%
		STRING FED_WTHLD	= 10%
		STRING CONTNUM		= 7%
		STRING LIFINS		= 10%
		STRING UNCFICA		= 10%
		STRING EIC		= 11%
		STRING ALLTIP		= 10%
		STRING FR_BEN		= 10%
		STRING FILL		= 1%
		STRING DEFCOMP		= 10%
		STRING FILL		= 1%
		STRING DCB		= 10%
		STRING FILL		= 1%
		STRING NQP457		= 10%
		STRING FILL		= 1%
		STRING NQPN457		= 10%
		STRING FILL		= 1%
		STRING MWAT		= 11%
		STRING FILL		= 1%
		STRING MTW		= 10%
		STRING FILL		= 96%
	END RECORD

	RECORD CODE_1I_STRUCT
		STRING RECID		= 2%
		STRING FICA_WAGE	= 10%
		STRING FILL		= 1%
		STRING FICA_TIP		= 10%
		STRING FILL		= 1%
		STRING ANNWAGE		= 10%
		STRING FILL		= 1%
		STRING FICA_WTHLD	= 10%
		STRING FILL		= 1%
		STRING FED_WTHLD	= 10%
		STRING CONTNUM		= 7%
		STRING LIFINS		= 10%
		STRING UNCFICA		= 10%
		STRING EIC		= 11%
		STRING ALLTIP		= 10%
		STRING FR_BEN		= 10%
		STRING FILL		= 1%
		STRING DEFCOMP		= 10%
		STRING FILL		= 3%
	END RECORD

	RECORD CODE_2I_STRUCT
		STRING RECID		= 2%
		STRING DCB		= 10%
		STRING FILL		= 1%
		STRING NQP457		= 10%
		STRING FILL		= 1%
		STRING NQPN457		= 10%
		STRING FILL		= 1%
		STRING MWAT		= 11%
		STRING FILL		= 1%
		STRING MTW		= 10%
		STRING FILL		= 71%
	END RECORD

	!
	! Total Records
	!
	RECORD CODE_T_STRUCT
		STRING RECID		= 1%
		STRING NUM_OF_EMP	= 7%
		STRING FICA_WAGE	= 13%
		STRING FILL		= 1%
		STRING FICA_TIP		= 12%
		STRING ANNWAGE		= 13%
		STRING FILL		= 1%
		STRING FICA_WTHLD	= 12%
		STRING FILL		= 1%
		STRING FED_WTHLD	= 12%
		STRING LIFINS		= 12%
		STRING UNCFICA		= 12%
		STRING EIC		= 12%
		STRING ALLTIP		= 12%
		STRING FR_BEN		= 12%
		STRING FILL		= 1%
		STRING DEFCOMP		= 13%
		STRING FILL		= 1%
		STRING DCB		= 12%
		STRING FILL		= 1%
		STRING NQP457		= 13%
		STRING FILL		= 1%
		STRING NQPN457		= 13%
		STRING FILL		= 1%
		STRING MWAT		= 13%
		STRING FILL		= 1%
		STRING MTW		= 12%
		STRING FILL		= 61%
	END RECORD

	RECORD CODE_1T_STRUCT
		STRING RECID		= 2%
		STRING NUM_OF_EMP	= 7%
		STRING FICA_WAGE	= 13%
		STRING FILL		= 1%
		STRING FICA_TIP		= 12%
		STRING ANNWAGE		= 13%
		STRING FILL		= 1%
		STRING FICA_WTHLD	= 12%
		STRING FILL		= 1%
		STRING FED_WTHLD	= 12%
		STRING LIFINS		= 12%
		STRING UNCFICA		= 12%
		STRING EIC		= 12%
		STRING ALLTIP		= 12%
		STRING FILL		= 6%
	END RECORD

	RECORD CODE_2T_STRUCT
		STRING RECID		= 2%
		STRING FR_BEN		= 12%
		STRING FILL		= 1%
		STRING DEFCOMP		= 13%
		STRING FILL		= 1%
		STRING DCB		= 12%
		STRING FILL		= 1%
		STRING NQP457		= 13%
		STRING FILL		= 1%
		STRING NQPN457		= 13%
		STRING FILL		= 1%
		STRING MWAT		= 13%
		STRING FILL		= 1%
		STRING MTW		= 12%
		STRING FILL		= 32%
	END RECORD

	!
	! Final records
	!
	RECORD CODE_F_STRUCT
		STRING RECID		= 1%
		STRING NUM_OF_EMP	= 7%
		STRING FILL		= 1%
		STRING TOTALSSW		= 16%
		STRING FILL		= 1%
		STRING TOTALSST		= 16%
		STRING FILL		= 1%
		STRING TOTALATW		= 16%
		STRING FILL		= 1%
		STRING TOTALSSTW	= 16%
		STRING FILL		= 1%
		STRING TOTALFITW	= 16%
		STRING FILL		= 1%
		STRING TOTALAEIC	= 16%
		STRING FILL		= 166%
	END RECORD

	RECORD CODE_1F_STRUCT
		STRING RECID		= 2%
		STRING NUM_OF_EMP	= 7%
		STRING TOTALSSW		= 16%
		STRING FILL		= 1%
		STRING TOTALSST		= 16%
		STRING FILL		= 1%
		STRING TOTALATW		= 16%
		STRING FILL		= 1%
		STRING TOTALSSTW	= 16%
		STRING FILL		= 1%
		STRING TOTALFITW	= 16%
		STRING FILL		= 1%
		STRING TOTALAEIC	= 16%
		STRING FILL		= 18%
	END RECORD


	!*******************************************************************
	! MREF format
	!*******************************************************************
	!
	! Transmitter record
	!
	RECORD CODE_RA_STRUCT
		STRING RECID		= 2%
		STRING EIN		= 9%
		STRING PIN		= 17%
		STRING RESUB		= 1%
		STRING RESUBTLCN	= 6%
		STRING SOFTCODE		= 2%
		STRING ENAME		= 57%
		STRING LADD		= 22%
		STRING DADD		= 22%
		STRING CITY		= 22%
		STRING ST		= 2%
		STRING ZIP		= 5%
		STRING ZIPEXT		= 4%
		STRING FILL		= 5%
		STRING PROVINCE		= 23%
		STRING FZIP		= 15%
		STRING COUNTRY		= 2%
		STRING SUBMITTER	= 57%
		STRING SLADDRESS	= 22%
		STRING SDADDRESS	= 22%
		STRING SCITY		= 22%
		STRING SSTATE		= 2%
		STRING SZIP		= 5%
		STRING SZIPEXT		= 4%
		STRING FILL		= 5%
		STRING SPROVINCE	= 23%
		STRING SFZIP		= 15%
		STRING SCOUNTRY		= 2%
		STRING SCONTACT		= 27%
		STRING SPHONE		= 15%
		STRING SPHONEEXT	= 5%
		STRING FILL		= 3%
		STRING SEMAIL		= 40%
		STRING FILL		= 3%
		STRING SFAX		= 10%
		STRING PROBLEM		= 1%
		STRING PREPARER		= 1%
		STRING FILL		= 12%
	END RECORD

	!
	! Employer Records
	!
	RECORD CODE_RE_STRUCT
		STRING RECID		= 2%
		STRING PAYYR		= 4%
		STRING AGENT		= 1%
		STRING EIN		= 9%
		STRING ESIN		= 9%
		STRING TERMBUS		= 1%
		STRING ESTNO		= 4%
		STRING OTHEREIN		= 9%
		STRING ENAME		= 57%
		STRING LOCADD		= 22%
		STRING ADD		= 22%
		STRING CITY		= 22%
		STRING STATE		= 2%
		STRING ZIP		= 5%
		STRING ZIPEXT		= 4%
		STRING FILL		= 5%
		STRING FORSTATE		= 23%
		STRING FORZIP		= 15%
		STRING CTYCODE		= 2%
		STRING EMPCODE		= 1%
		STRING TAXJOUR		= 1%
		STRING THIRDPARTY	= 1%
		STRING FILL		= 291%
	END RECORD

	!
	! Employee Wage Records
	!
	RECORD CODE_RW_STRUCT
		STRING RECID		= 2%
		STRING SSN		= 9%
		STRING ENAME		= 15%
		STRING ENAMEMID		= 15%
		STRING ENAMELAST	= 20%
		STRING SUFFIX		= 4%
		STRING LOCADD		= 22%
		STRING ADD		= 22%
		STRING CITY		= 22%
		STRING ST		= 2%
		STRING ZIP		= 5%
		STRING ZIPEXT		= 4%
		STRING FILL		= 5%
		STRING FORSTATE		= 23%
		STRING FORZIP		= 15%
		STRING CTYCODE		= 2%
		STRING FED_WAGE		= 11%
		STRING FED_WTHLD	= 11%
		STRING FICA_WAGE	= 11%
		STRING FICA_WTHLD	= 11%
		STRING MED_WAGE		= 11%
		STRING MED_WTHLD	= 11%
		STRING FICA_TIP		= 11%
		STRING EIC		= 11%
		STRING DCB		= 11%
		STRING DEFCOMP401	= 11%
		STRING DEFCOMP403	= 11%
		STRING DEFCOMP408	= 11%
		STRING DEFCOMP457	= 11%
		STRING DEFCOMP501	= 11%
		STRING MEBQSCP		= 11%
		STRING NQP457		= 11%
		STRING ECHSA		= 11%
		STRING NQPN457		= 11%
		STRING FILL		= 22%
		STRING ECPGTLI		= 11%
		STRING IENSO		= 11%
		STRING FILL		= 56%
		STRING SEC		= 1%
		STRING FILL		= 1%
		STRING PENPLAN_FLAG	= 1%
		STRING TPSPI		= 1%
		STRING FILL		= 23%
	END RECORD

	!
	! Supplimantal records
	!
	RECORD CODE_RS_STRUCT
		STRING RECID		= 2%
		STRING STCODE1		= 2%
		STRING TAXINGENT	= 5%
		STRING SSN		= 9%
		STRING ENAME		= 15%
		STRING ENAMEMIDDLE	= 15%
		STRING ENAMELAST	= 20%
		STRING SUFFIX		= 4%
		STRING LOCADD		= 22%
		STRING ADD		= 22%
		STRING CITY		= 22%
		STRING ST		= 2%
		STRING ZIP		= 5%
		STRING ZIPEXT		= 4%
		STRING FILL		= 5%
		STRING FORSTATE		= 23%
		STRING FORZIP		= 15%
		STRING CTYCODE		= 2%
		STRING XOPTIONAL	= 2%
		STRING REPPER		= 6%
		STRING TOTWAG		= 11%
		STRING SUIWAG		= 11%
		STRING WEEKWORK		= 2%
		STRING HIREDATE		= 8%
		STRING FIREDATE		= 8%
		STRING FILL		= 5%
		STRING SEAN		= 20%
		STRING FILL		= 6%
		STRING STCODE2		= 2%
		STRING STWAGE		= 11%
		STRING STTAX		= 11%
		STRING OTHDATA		= 10%
		STRING TAXTYP		= 1%
		STRING LOCWAGE		= 11%
		STRING LOCTAX		= 11%
		STRING STCONNUM		= 7%
		STRING SUPDATA1		= 75%
		STRING SUPDATA2		= 75%
		STRING FILL		= 25%
	END RECORD

	!
	! Total Records
	!
	RECORD CODE_RT_STRUCT
		STRING RECID		= 2%
		STRING NUM_OF_EMP	= 7%
		STRING ANNWAGE		= 15%
		STRING FED_WTHLD	= 15%
		STRING FICA_WAGE	= 15%
		STRING FICA_WTHLD	= 15%
		STRING MED_WAGE		= 15%
		STRING MED_WTHLD	= 15%
		STRING FICA_TIP		= 15%
		STRING EIC		= 15%
		STRING DCB		= 15%
		STRING DEFCOMP401	= 15%
		STRING DEFCOMP403	= 15%
		STRING DEFCOMP408	= 15%
		STRING DEFCOMP457	= 15%
		STRING DEFCOMP501	= 15%
		STRING MEBQSC		= 15%
		STRING NQP457		= 15%
		STRING ECHSA		= 15%
		STRING NQPN457		= 15%
		STRING FILL		= 30%
		STRING LIFINS		= 15%
		STRING ITWTPP		= 15%
		STRING IENSO		= 15%
		STRING FILL		= 158%
	END RECORD

	RECORD CODE_RF_STRUCT
		STRING RECID		= 2%
		STRING FILL		= 5%
		STRING NUM_OF_EMP	= 9%
		STRING FILL		= 496%
	END RECORD


	MAP	(PR_CODE)	CODE$			= 276%
	MAP	(PR_CODE)	CODE_A_STRUCT		CODE_A
	MAP	(PR_CODE)	CODE_B_STRUCT		CODE_B
	MAP	(PR_CODE)	CODE_E_STRUCT		CODE_E
	MAP	(PR_CODE)	CODE_W_STRUCT		CODE_W
	MAP	(PR_CODE)	CODE_S_STRUCT		CODE_S
	MAP	(PR_CODE)	CODE_I_STRUCT		CODE_I
	MAP	(PR_CODE)	CODE_T_STRUCT		CODE_T
	MAP	(PR_CODE)	CODE_F_STRUCT		CODE_F

	MAP	(PR_FLOPPY)	FLOPPY$			= 512% ! Was 128
	MAP	(PR_FLOPPY)	CODE_1A_STRUCT		CODE_1A
	MAP	(PR_FLOPPY)	CODE_2A_STRUCT		CODE_2A
	MAP	(PR_FLOPPY)	CODE_1B_STRUCT		CODE_1B
	MAP	(PR_FLOPPY)	CODE_2B_STRUCT		CODE_2B
	MAP	(PR_FLOPPY)	CODE_1E_STRUCT		CODE_1E
	MAP	(PR_FLOPPY)	CODE_2E_STRUCT		CODE_2E
	MAP	(PR_FLOPPY)	CODE_1W_STRUCT		CODE_1W
	MAP	(PR_FLOPPY)	CODE_2W_STRUCT		CODE_2W
	MAP	(PR_FLOPPY)	CODE_3W_STRUCT		CODE_3W
	MAP	(PR_FLOPPY)	CODE_1S_STRUCT		CODE_1S
	MAP	(PR_FLOPPY)	CODE_2S_STRUCT		CODE_2S
	MAP	(PR_FLOPPY)	CODE_1I_STRUCT		CODE_1I
	MAP	(PR_FLOPPY)	CODE_2I_STRUCT		CODE_2I
	MAP	(PR_FLOPPY)	CODE_1T_STRUCT		CODE_1T
	MAP	(PR_FLOPPY)	CODE_2T_STRUCT		CODE_2T
	MAP	(PR_FLOPPY)	CODE_1F_STRUCT		CODE_1F

	MAP	(PR_FLOPPY)	CODE_RA_STRUCT		CODE_RA
	MAP	(PR_FLOPPY)	CODE_RE_STRUCT		CODE_RE
	MAP	(PR_FLOPPY)	CODE_RW_STRUCT		CODE_RW
	MAP	(PR_FLOPPY)	CODE_RS_STRUCT		CODE_RS
	MAP	(PR_FLOPPY)	CODE_RT_STRUCT		CODE_RT
	MAP	(PR_FLOPPY)	CODE_RF_STRUCT		CODE_RF


	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Set up screen
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

	!
	! Look up device
	!
	CALL  READ_DEVICE("TAPE_DEVICE", TAPE_DEVICE$, STAT%)

 AskTape:
	TOTAPE% = -1%

	SCOPE::PRG_ITEM = "FLDTAPEDEV"
	!++
	! Abstract:FLDTAPEDEV
	!	^*Tape Device\*
	!	.p
	!	The ^*Tape Device\* asks for user input of the type of tape used to store the
	!	information for transportation.
	!
	! Index:
	!	.x W-2 Tape>Tape Device
	!	.x Tape Device>W-2 Tape
	!
	!--
	TEMP$ = "Tape device <" + TAPE_DEVICE$ + "> "

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, TEMP$, 1%, 1%)

	JUNK$ = SPACE$(20%)
	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, LEN(TEMP$) + 2%, &
		JUNK$, -1%, 16% + 4096%)

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
		69%, 70%, 87%, SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO AskTape

	END SELECT

	IF JUNK$ <> ""
	THEN
		TAPE_DEVICE$ = JUNK$
	END IF

	IF TAPE_DEVICE$ = ""
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Please Enter Tape Device", 1%)
		GOTO AskTape
	END IF

	!***************************************************************
	! Open mag tape drive
	!***************************************************************

	CALL ASSG_CHANNEL(PRNT.CH%, STAT%)

	IF LEFT(TAPE_DEVICE$, 1%) = "M" AND INSTR(1%, TAPE_DEVICE$, ":")
	THEN
		!
		! Magtape (I Hope)
		!
		OPEN TAPE_DEVICE$ FOR INPUT AS FILE PRNT.CH%, &
			ACCESS READ, &
			RECORDSIZE 25% * 152%

		V% = MAGTAPE(3%, 0%, PRNT.CH%)
		TOTAPE% = -1%
	ELSE
		!
		! Text File (I Hope)
		!
		OPEN TAPE_DEVICE$ FOR INPUT AS FILE #PRNT.CH%, &
			RECORDSIZE 512%, &
			ACCESS READ, &
			ALLOW READ
		TOTAPE% = 0%
	END IF

	!
	! Paint instruction screen
	!
	ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Types  Description", 4%, 5%)
	ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"A      Transmitter Record", 5%, 5%)
	ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"B      Basic Authorization Record", 6%, 5%)
	ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"E      Employer/Establishment Record", 7%, 5%)
	ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"W      Employee Wage Record", 8%, 5%)
	ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"S      Supplemental State Record", 9%, 5%)
	ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"I      Intermediate Totals Record", 10%, 5%)
	ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"T      Total Record", 11%, 5%)
	ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"F      Final Record", 12%, 5%)
	ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Select Record types separated by commas", 14%, 5%)

 Verify:
	SCOPE::PRG_ITEM = "VERIFY"
	VERIFY$ = "*                   "

	VERIFY$ = ENTR_3STRING(SCOPE, SCOPE::SMG_OPTION, "", &
		"Record Types to Verify  ", &
		VERIFY$, 0%, "", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Verify
	END SELECT

	IF VERIFY$ = ""
	THEN
		GOTO Verify
	END IF

	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

2000	!*******************************************************************
	! Read through Payroll master file
	!*******************************************************************

	IF TOTAPE%
	THEN
		!
		! Magtape
		!
		WHEN ERROR IN
			GET #PRNT.CH%
		USE
			CONTINUE ExitTotal IF ERR = 11%
			FILENAME$ = TAPE_DEVICE$
			CONTINUE HelpError
		END WHEN

		MOVE FROM #PRNT.CH%, BUFF_TEXT$ = RECOUNT

		FOR TEXT_LOOP% = 1% TO LEN(BUFF_TEXT$) STEP 276%

			TEXT$ = MID(BUFF_TEXT$, TEXT_LOOP%, 276%)
			GOSUB 2100

		NEXT TEXT_LOOP%
	ELSE
		!
		! Text file
		!
		WHEN ERROR IN
			Linput #prnt.ch%, TEXT$
		USE
			CONTINUE ExitTotal IF ERR = 11%
			FILENAME$ = TAPE_DEVICE$
			CONTINUE HelpError
		END WHEN

		GOSUB 2100
		GOTO 2000
	END IF

	GOTO 2000

2100	IF LEN(TEXT$) <> 276%
	THEN
		SETCODE$ = LEFT(TEXT$, 2%)

		IF COMP_STRING(MID(SETCODE$, 2%, 1%), VERIFY$) <> 0%
		THEN
			SCOPE::SCOPE_EXIT = 0%

			GOSUB SetCodeFloppy

			SELECT SCOPE::SCOPE_EXIT
			CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
				GOTO ExitProgram
			END SELECT
		END IF
	ELSE
		SETCODE$ = LEFT(TEXT$, 1%)

		IF COMP_STRING(SETCODE$, VERIFY$) <> 0%
		THEN
			SCOPE::SCOPE_EXIT = 0%

			GOSUB SetCode

			SELECT SCOPE::SCOPE_EXIT
			CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
				GOTO ExitProgram
			END SELECT
		END IF
	END IF

	RETURN

	%Page

 ExitTotal:
	!*********************************************************************
	! totals
	!*********************************************************************

	CLOSE #PRNT.CH%

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 SetCode:
	!********************************************************************
	! Set codes records
	!*******************************************************************
	CODE$ = TEXT$

	V% = SMG$BEGIN_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)

	SELECT SETCODE$
	CASE "A"
		!****************************************************
		! Code A Transmitter record
		!****************************************************

		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Record Identifier     " + CODE_A::RECID, 2%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Payment Year          " + CODE_A::PAYYR, 3%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employer ID #         " + CODE_A::EIN, 4%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Foreign Address Ind.  " + CODE_A::FORADD, 5%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Transmitter Name      " + CODE_A::ENAME, 6%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Address               " + CODE_A::ADD, 7%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"City                  " + CODE_A::CITY, 8%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State                 " + CODE_A::ST, 9%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Zip                   " + CODE_A::ZIP, 11%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Zip code extension    " + CODE_A::ZIPEXT, 12%, 5%)

	CASE "B"
		!****************************************************
		! Code B Basic Authorization Record
		!****************************************************

		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Record Identifier     " + CODE_B::RECID, 2%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Payment Year          " + CODE_B::PAYYR, 3%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employer ID #         " + CODE_B::EIN, 4%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Computer              " + CODE_B::COMTYP, 5%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Internal Labeling     " + CODE_B::LABEL, 6%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Density               " + CODE_B::DENSITY, 7%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Recording Code        " + CODE_B::RECCOD, 8%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Foreign Address Ind.  " + CODE_B::FORADD, 9%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"File Return Name      " + CODE_B::ENAME, 10%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"File Return Address   " + CODE_B::ADD, 11%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"File Return City      " + CODE_B::CITY, 12%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"File Return State     " + CODE_B::ST, 13%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Zip                   " + CODE_B::ZIP, 14%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Zip Code Extension    " + CODE_B::ZIPEXT, 15%, 5%)

	CASE "E"
		!****************************************************
		! Code E Employer/Establishment
		!****************************************************

		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Record Identifier     " + CODE_E::RECID, 2%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Payment Year          " + CODE_E::PAYYR, 3%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employer ID #         " + CODE_E::EIN, 4%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State Local 69 Number " + CODE_E::ESIN, 5%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Name                  " + CODE_E::ENAME, 6%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Address               " + CODE_E::ADD, 7%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"City                  " + CODE_E::CITY, 8%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State                 " + CODE_E::STATE, 9%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Zip                   " + CODE_E::ZIP, 10%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Zip code extension    " + CODE_E::ZIPEXT, 11%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Name Code             " + CODE_E::NAMCODE, 12%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Type of Employment    " + CODE_E::TYPEMP, 13%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Blocking Factor       " + CODE_E::BLKFAC, 14%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Establishment #       " + CODE_E::PRU, 15%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Limitation of Lia     " + CODE_E::SLL, 16%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Foreign Address Ind.  " + CODE_E::FORADD, 17%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Other EIN             " + CODE_E::OTHEREIN, 18%, 5%)

	CASE "W"
		!****************************************************
		! Code W Employee Wage Record
		!****************************************************

		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Record Identifier     " + CODE_W::RECID, 1%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Social Security #     " + CODE_W::SSN, 2%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employee Name         " + CODE_W::ENAME, 3%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employee Address      " + CODE_W::ADD, 4%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employee City         " + CODE_W::CITY, 5%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employee State        " + CODE_W::ST, 6%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Zip Code Extension    " + CODE_W::ZIPEXT, 7%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employee Zip          " + CODE_W::ZIP, 8%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Statutory Employee    " + CODE_W::SEC, 9%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"FICA Wages            " + CODE_W::FICA_WAGE, 10%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"FICA Tips             " + CODE_W::FICA_TIP, 11%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Wages                 " + CODE_W::ANNWAGE, 12%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"FICA Withheld         " + CODE_W::FICA_WTHLD, 13%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Federal Withheld      " + CODE_W::FED_WTHLD, 14%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Allocated Tips        " + CODE_W::ALLTIP, 15%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Fringe Benefits       " + CODE_W::FR_BEN, 16%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Control Number        " + CODE_W::CN, 17%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Group Term Life Ins   " + CODE_W::GTL, 18%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Uncollected FICA      " + CODE_W::UNCOLL_FICA, 9%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Earned Income Credit  " + CODE_W::EIC, 10%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Pension Plan Flag     " + CODE_W::PENPLAN_FLAG, &
			11%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Deferred Comp Flag    " + CODE_W::DEFCOMP_FLAG, &
			12%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Deferred Comp Amount  " + CODE_W::DEFCOMP, 13%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Medicare Wages & Tips " + CODE_W::MWAT, 14%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Medicare Tax Witheld  " + CODE_W::MTW, 15%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Non Qual Plan Sec 457 " + CODE_W::NQP457, 16%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Non Qual Plan not 457 " + CODE_W::NQPN457, 17%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Dependent Care Ben.   " + CODE_W::DCB, 18%, 40%)

	CASE "S"
		!****************************************************
		! Code S Supplemental State Record
		!****************************************************

		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Record Identifier     " + CODE_S::RECID, 1%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Social Security #     " + CODE_S::SSN, 2%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employee Name         " + CODE_S::ENAME, 3%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employee Address      " + CODE_S::ADD, 4%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employee City         " + CODE_S::CITY, 5%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employee State        " + CODE_S::ST, 6%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Zip Code Extension    " + CODE_S::ZIPEXT, 7%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employee Zip          " + CODE_S::ZIP, 8%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State Code            " + CODE_S::STCODE1, 9%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Optional Code         " + CODE_S::XOPTIONAL, 10%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Reporting Period      " + CODE_S::REPPER, 11%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"SUI Total Wages       " + CODE_S::TOTWAG, 12%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"SUI Taxable Wages     " + CODE_S::SUIWAG, 13%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Weeks Worked          " + CODE_S::WEEKWORK, 14%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Hire Date             " + CODE_S::HIREDATE, 15%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Fire Date             " + CODE_S::FIREDATE, 16%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Taxing Entity Code    " + CODE_S::ENTITYCODE1, &
			17%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State Emp. Account No " + CODE_S::SEAN, 18%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State Code            " + CODE_S::STCODE2, 9%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State Taxable Wages   " + CODE_S::STWAGE, 10%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State Tax Withheld    " + CODE_S::STTAX, 11%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Other State Data      " + CODE_S::OTHDATA, 12%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Tax Type Code         " + CODE_S::TAXTYP, 13%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Taxing Entity Code    " + CODE_S::ENTITYCODE2, &
			14%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Local Taxable Wages   " + CODE_S::LOCWAGE, 15%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Local Tax Withheld    " + CODE_S::LOCTAX, 16%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State Control #       " + CODE_S::STCONNUM, 17%, 40%)

	CASE "I"
		!****************************************************
		! Code I Intermediate Totals Record
		!****************************************************

		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Record Identifier     " + CODE_I::RECID, 1%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"FICA Wages            " + CODE_I::FICA_WAGE, 2%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"FICA Tips             " + CODE_I::FICA_TIP, 3%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Annual Wages          " + CODE_I::ANNWAGE, 4%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"FICA Withheld         " + CODE_I::FICA_WTHLD, 5%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Federal Withheld      " + CODE_I::FED_WTHLD, 6%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Control Number        " + CODE_I::CONTNUM, 7%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Group Term Life Ins   " + CODE_I::LIFINS, 8%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Uncollected FICA      " + CODE_I::UNCFICA, 9%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Earned Income Credit  " + CODE_I::EIC, 10%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Allocated Tips        " + CODE_I::ALLTIP, 11%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Fringe Benefits       " + CODE_I::FR_BEN, 12%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Deferred Comp Amount  " + CODE_I::DEFCOMP, 13%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Dependent Care Ben.   " + CODE_I::DCB, 14%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Non Qual Plan Sec 457 " + CODE_I::NQP457, 15%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Non Qual Plan Not 457 " + CODE_I::NQPN457, 16%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Medicare Wages & Tips " + CODE_I::MWAT, 17%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Medicare Taxes        " + CODE_I::MTW, 18%, 5%)

	CASE "T"
		!****************************************************
		! Code T Total Record
		!****************************************************

		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Record Identifier     " + CODE_T::RECID, 1%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Number of Employee    " + CODE_T::NUM_OF_EMP, 2%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"FICA Wages            " + CODE_T::FICA_WAGE, 3%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"FICA Tips             " + CODE_T::FICA_TIP, 4%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Annual Wages          " + CODE_T::ANNWAGE, 5%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"FICA Withheld         " + CODE_T::FICA_WTHLD, 6%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Federal Withheld      " + CODE_T::FED_WTHLD, 7%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Group Term Life Ins   " + CODE_T::LIFINS, 8%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Uncollected FICA      " + CODE_T::UNCFICA, 9%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Earned Income Credit  " + CODE_T::EIC, 10%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Allocated Tips        " + CODE_T::ALLTIP, 11%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Fringe Benefits       " + CODE_T::FR_BEN, 12%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Deferred Comp Amount  " + CODE_T::DEFCOMP, 13%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Dependent Care Ben.   " + CODE_T::DCB, 14%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Non Qual Plan Sec 457 " + CODE_T::NQP457, 15%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Non Qual Plan Not 457 " + CODE_T::NQPN457, 16%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Medicare Wages & Tips " + CODE_T::MWAT, 17%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Medicare Tax          " + CODE_T::MTW, 18%, 5%)

	CASE "F"
		!****************************************************
		! Code F Final Record
		!****************************************************

		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Record Identifier     " + CODE_F::RECID, 1%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Number of Employees   " + CODE_F::NUM_OF_EMP, 2%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Soc. Sec. Wages       " + CODE_F::TOTALSSW, 3%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Soc. Sec. Tips        " + CODE_F::TOTALSST, 4%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Wages, Tips, Other    " + CODE_F::TOTALATW, 5%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Soc. Sec. Withheld    " + CODE_F::TOTALSSTW, 6%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Fed Tax Withheld      " + CODE_F::TOTALFITW, 7%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Earned Income Credit  " + CODE_F::TOTALAEIC, 8%, 5%)
	END SELECT

	V% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	!
	! Pause to read screen
	!
	CALL ENTR_3MESSAGE(SCOPE, "", 0%)

	RETURN

	%Page

 SetCodeFloppy:
	!********************************************************************
	! Set codes records
	!*******************************************************************
	FLOPPY$ = TEXT$

	V% = SMG$BEGIN_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)

	SELECT SETCODE$
		CASE "1A"
			!****************************************************
			! Code A Transmitter record
			!****************************************************

			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_1A::RECID, &
				2%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Payment Year          " + CODE_1A::PAYYR, &
				3%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employer ID #         " + CODE_1A::EIN, &
				4%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Foreign Address Ind.  " + CODE_1A::FORADD, &
				5%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Transmitter Name      " + CODE_1A::ENAME, &
				6%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Address               " + CODE_1A::ADD, &
				7%, 5%)

		CASE "2A"
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_2A::RECID, &
				2%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"City                  " + CODE_2A::CITY, &
				8%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State                 " + CODE_2A::ST, &
				9%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Zip                   " + CODE_2A::ZIP, &
				11%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Zip code extension    " + CODE_2A::ZIPEXT, &
				12%, 5%)

		CASE "1B"
			!****************************************************
			! Code B Basic Authorization Record
			!****************************************************

			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_1B::RECID, &
				2%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Payment Year          " + CODE_1B::PAYYR, &
				3%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employer ID #         " + CODE_1B::EIN, &
				4%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Computer              " + CODE_1B::COMTYP, &
				5%, 5%)

		CASE "2B"
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_2B::RECID, &
				2%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Foreign Address Ind.  " + CODE_2B::FORADD, &
				9%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"File Return Name      " + CODE_2B::ENAME, &
				10%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"File Return Address   " + CODE_2B::ADD, &
				11%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"File Return City      " + CODE_2B::CITY, &
				12%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"File Return State     " + CODE_2B::ST, &
				13%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Zip                   " + CODE_2B::ZIP, &
				14%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Zip Code Extension    " + CODE_2B::ZIPEXT, &
				15%, 5%)

		CASE "1E"
			!****************************************************
			! Code E Employer/Establishment
			!****************************************************

			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_1E::RECID, &
				2%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Payment Year          " + CODE_1E::PAYYR, &
				3%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employer ID #         " + CODE_1E::EIN, &
				4%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State Local 69 Number " + CODE_1E::ESIN, &
				5%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Name                  " + CODE_1E::ENAME, &
				6%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Address               " + CODE_1E::ADD, &
				7%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Foreign Address Ind.  " + CODE_1E::FORADD, &
				17%, 5%)

		CASE "2E"
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_2E::RECID, &
				2%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"City                  " + CODE_2E::CITY, &
				8%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State                 " + CODE_2E::STATE, &
				9%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Zip                   " + CODE_2E::ZIP, &
				10%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Zip code extension    " + CODE_2E::ZIPEXT, &
				11%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Name Code             " + CODE_2E::NAMCODE, &
				12%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Type of Employment    " + CODE_2E::TYPEMP, &
				13%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Establishment #       " + CODE_2E::PRU, &
				15%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Limitation of Lia     " + CODE_2E::SLL, &
				16%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Other EIN             " + CODE_2E::OTHEREIN, &
				18%, 5%)

		CASE "1W"
			!****************************************************
			! Code W Employee Wage Record
			!****************************************************

			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_1W::RECID, &
				1%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Social Security #     " + CODE_1W::SSN, &
				2%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee Name         " + CODE_1W::ENAME, &
				3%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee Address      " + CODE_1W::ADD, &
				4%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee City         " + CODE_1W::CITY, &
				5%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee State        " + CODE_1W::ST, &
				6%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Zip Code Extension    " + CODE_1W::ZIPEXT, &
				7%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee Zip          " + CODE_1W::ZIP, &
				8%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Statutory Employee    " + CODE_1W::SEC, &
				9%, 5%)

		CASE "2W"
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_2W::RECID, &
				1%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"FICA Wages            " + CODE_2W::FICA_WAGE, &
				10%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"FICA Tips             " + CODE_2W::FICA_TIP, &
				11%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Wages                 " + CODE_2W::ANNWAGE, &
				12%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"FICA Withheld         " + CODE_2W::FICA_WTHLD, &
				13%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Federal Withheld      " + CODE_2W::FED_WTHLD, &
				14%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Non Qual Plan Sec 457 " + CODE_2W::NQP457, &
				16%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Non Qual Plan not 457 " + CODE_2W::NQPN457, &
				17%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Control Number        " + CODE_2W::CN, &
				17%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Group Term Life Ins   " + CODE_2W::GTL, &
				18%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Uncollected FICA      " + CODE_2W::UNCOLL_FICA, &
				9%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Earned Income Credit  " + CODE_2W::EIC, &
				10%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Allocated Tips        " + CODE_2W::ALLTIP, &
				15%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Fringe Benefits       " + CODE_2W::FR_BEN, &
				16%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Pension Plan Flag     " + CODE_2W::PENPLAN_FLAG, &
				11%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Deferred Comp Flag    " + CODE_2W::DEFCOMP_FLAG, &
				12%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Deferred Comp Amount  " + CODE_2W::DEFCOMP, &
				13%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Dependent Care Ben.   " + CODE_2W::DCB, &
				18%, 40%)

		CASE "3W"
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_2W::RECID, &
				1%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Medicare Wages & Tips " + CODE_3W::MWAT, &
				14%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Medicare Tax Witheld  " + CODE_3W::MTW, &
				15%, 40%)

		CASE "1S"
			!****************************************************
			! Code S Supplemental State Record
			!****************************************************

			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_1S::RECID, &
				1%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Social Security #     " + CODE_1S::SSN, &
				2%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee Name         " + CODE_1S::ENAME, &
				3%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee Address      " + CODE_1S::ADD, &
				4%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee City         " + CODE_1S::CITY, &
				5%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee State        " + CODE_1S::ST, &
				6%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Zip Code Extension    " + CODE_1S::ZIPEXT, &
				7%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee Zip          " + CODE_1S::ZIP, &
				8%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State Code            " + CODE_1S::STCODE1, &
				9%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Optional Code         " + CODE_1S::XOPTIONAL, &
				10%, 5%)

		CASE "2S"
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_2S::RECID, &
				1%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Reporting Period      " + CODE_2S::REPPER, &
				11%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"SUI Total Wages       " + CODE_2S::TOTWAG, &
				12%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"SUI Taxable Wages     " + CODE_2S::SUIWAG, &
				13%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Weeks Worked          " + CODE_2S::WEEKWORK, &
				14%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Hire Date             " + CODE_2S::HIREDATE, &
				15%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Fire Date             " + CODE_2S::FIREDATE, &
				16%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Taxing Entity Code    " + CODE_2S::ENTITYCODE1, &
				17%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State Emp. Account No " + CODE_2S::SEAN, &
				18%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State Code            " + CODE_2S::STCODE2, &
				9%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State Taxable Wages   " + CODE_2S::STWAGE, &
				10%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State Tax Withheld    " + CODE_2S::STTAX, &
				11%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Other State Data      " + CODE_2S::OTHDATA, &
				12%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Tax Type Code         " + CODE_2S::TAXTYP, &
				13%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Taxing Entity Code    " + CODE_2S::ENTITYCODE2, &
				14%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Local Taxable Wages   " + CODE_2S::LOCWAGE, &
				15%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Local Tax Withheld    " + CODE_2S::LOCTAX, &
				16%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State Control #       " + CODE_2S::STCONNUM, &
				17%, 40%)

		CASE "1I"
			!****************************************************
			! Code I Intermediate Totals Record
			!****************************************************

			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_1I::RECID, &
				1%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"FICA Wages            " + CODE_1I::FICA_WAGE, &
				2%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"FICA Tips             " + CODE_1I::FICA_TIP, &
				3%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Annual Wages          " + CODE_1I::ANNWAGE, &
				4%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"FICA Withheld         " + CODE_1I::FICA_WTHLD, &
				5%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Federal Withheld      " + CODE_1I::FED_WTHLD, &
				6%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Control Number        " + CODE_1I::CONTNUM, &
				7%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Group Term Life Ins   " + CODE_1I::LIFINS, &
				8%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Uncollected FICA      " + CODE_1I::UNCFICA, &
				9%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Earned Income Credit  " + CODE_1I::EIC, &
				10%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Allocated Tips        " + CODE_1I::ALLTIP, &
				11%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Fringe Benefits       " + CODE_1I::FR_BEN, &
				12%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Deferred Comp Amount  " + CODE_1I::DEFCOMP, &
				13%, 5%)

		CASE "2I"
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_2I::RECID, &
				1%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Dependent Care Ben.   " + CODE_2I::DCB, &
				14%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Non Qual Plan Sec 457 " + CODE_2I::NQP457, &
				15%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Non Qual Plan Not 457 " + CODE_2I::NQPN457, &
				16%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Medicare Wages & Tips " + CODE_2I::MWAT, &
				17%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Medicare Taxes        " + CODE_2I::MTW, &
				18%, 5%)

		CASE "1T"
			!****************************************************
			! Code T Total Record
			!****************************************************

			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_1T::RECID, &
				1%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Number of Employee    " + CODE_1T::NUM_OF_EMP, &
				2%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"FICA Wages            " + CODE_1T::FICA_WAGE, &
				3%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"FICA Tips             " + CODE_1T::FICA_TIP, &
				4%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Annual Wages          " + CODE_1T::ANNWAGE, &
				5%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"FICA Withheld         " + CODE_1T::FICA_WTHLD, &
				6%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Federal Withheld      " + CODE_1T::FED_WTHLD, &
				7%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Group Term Life Ins   " + CODE_1T::LIFINS, &
				8%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Uncollected FICA      " + CODE_1T::UNCFICA, &
				9%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Earned Income Credit  " + CODE_1T::EIC, &
				10%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Allocated Tips        " + CODE_1T::ALLTIP, &
				11%, 5%)

		CASE "2T"
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_2T::RECID, &
				1%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Fringe Benefits       " + CODE_2T::FR_BEN, &
				12%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Deferred Comp Amount  " + CODE_2T::DEFCOMP, &
				13%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Dependent Care Ben.   " + CODE_2T::DCB, &
				14%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Non Qual Plan Sec 457 " + CODE_2T::NQP457, &
				15%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Non Qual Plan Not 457 " + CODE_2T::NQPN457, &
				16%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Medicare Wages & Tips " + CODE_2T::MWAT, &
				17%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Medicare Tax          " + CODE_2T::MTW, &
				18%, 5%)

		CASE "1F"
			!****************************************************
			! Code F Final Record
			!****************************************************

			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_1F::RECID, &
				1%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Number of Employees   " + CODE_1F::NUM_OF_EMP, &
				2%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Soc. Sec. Wages       " + CODE_1F::TOTALSSW, &
				3%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Soc. Sec. Tips        " + CODE_1F::TOTALSST, &
				4%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Wages, Tips, Other    " + CODE_1F::TOTALATW, &
				5%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Soc. Sec. Withheld    " + CODE_1F::TOTALSSTW, &
				6%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Fed Tax Withheld      " + CODE_1F::TOTALFITW, &
				7%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Earned Income Credit  " + CODE_1F::TOTALAEIC, &
				8%, 5%)

		CASE "RA"
			!****************************************************
			! Code A Transmitter record
			!****************************************************

			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_RA::RECID, &
				2%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employer ID #         " + CODE_RA::EIN, &
				4%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Transmitter Name      " + CODE_RA::ENAME, &
				6%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Address               " + CODE_RA::LADD, &
				7%, 5%)


			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"City                  " + CODE_RA::CITY, &
				8%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State                 " + CODE_RA::ST, &
				9%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Zip                   " + CODE_RA::ZIP, &
				11%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Zip code extension    " + CODE_RA::ZIPEXT, &
				12%, 5%)

		CASE "RE"

			!****************************************************
			! Code E Employer/Establishment
			!****************************************************

			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_RE::RECID, &
				2%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Payment Year          " + CODE_RE::PAYYR, &
				3%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employer ID #         " + CODE_RE::EIN, &
				4%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State Local 69 Number " + CODE_RE::ESIN, &
				5%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Name                  " + CODE_RE::ENAME, &
				6%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Address               " + CODE_RE::ADD, &
				7%, 5%)


			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"City                  " + CODE_RE::CITY, &
				8%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State                 " + CODE_RE::STATE, &
				9%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Zip                   " + CODE_RE::ZIP, &
				10%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Zip code extension    " + CODE_RE::ZIPEXT, &
				11%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Emp Code              " + CODE_RE::EMPCODE, &
				12%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Other EIN             " + CODE_RE::OTHEREIN, &
				18%, 5%)


		CASE "RW"

			!****************************************************
			! Code W Employee Wage Record
			!****************************************************

			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_RW::RECID, &
				1%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Social Security #     " + CODE_RW::SSN, &
				2%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee Name         " + CODE_RW::ENAME, &
				3%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee Address      " + CODE_RW::ADD, &
				4%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee City         " + CODE_RW::CITY, &
				5%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee State        " + CODE_RW::ST, &
				6%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Zip Code Extension    " + CODE_RW::ZIPEXT, &
				7%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee Zip          " + CODE_RW::ZIP, &
				8%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Statutory Employee    " + CODE_RW::SEC, &
				9%, 5%)


			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"FICA Wages            " + CODE_RW::FICA_WAGE, &
				10%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"FICA Tips             " + CODE_RW::FICA_TIP, &
				11%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Wages                 " + CODE_RW::FED_WAGE, &
				12%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"FICA Withheld         " + CODE_RW::FICA_WTHLD, &
				13%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Federal Withheld      " + CODE_RW::FED_WTHLD, &
				14%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Non Qual Plan Sec 457 " + CODE_RW::NQP457, &
				16%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Non Qual Plan not 457 " + CODE_RW::NQPN457, &
				17%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Earned Income Credit  " + CODE_RW::EIC, &
				10%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Allocated Tips        " + CODE_RW::FICA_TIP, &
				15%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Pension Plan Flag     " + CODE_RW::PENPLAN_FLAG, &
				11%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Deferred Comp 401     " + CODE_RW::DEFCOMP401, &
				13%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Dependent Care Ben.   " + CODE_RW::DCB, &
				18%, 40%)


			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Medicare Wages & Tips " + CODE_RW::MED_WAGE, &
				14%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Medicare Tax Witheld  " + CODE_RW::MED_WTHLD, &
				15%, 40%)


		CASE "RS"

			!****************************************************
			! Code S Supplemental State Record
			!****************************************************

			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_RS::RECID, &
				1%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Social Security #     " + CODE_RS::SSN, &
				2%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee Name         " + CODE_RS::ENAME, &
				3%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee Address      " + CODE_RS::ADD, &
				4%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee City         " + CODE_RS::CITY, &
				5%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee State        " + CODE_RS::ST, &
				6%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Zip Code Extension    " + CODE_RS::ZIPEXT, &
				7%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Employee Zip          " + CODE_RS::ZIP, &
				8%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State Code            " + CODE_RS::STCODE1, &
				9%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Optional Code         " + CODE_RS::XOPTIONAL, &
				10%, 5%)


			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Reporting Period      " + CODE_RS::REPPER, &
				11%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"SUI Total Wages       " + CODE_RS::TOTWAG, &
				12%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"SUI Taxable Wages     " + CODE_RS::SUIWAG, &
				13%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Weeks Worked          " + CODE_RS::WEEKWORK, &
				14%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Hire Date             " + CODE_RS::HIREDATE, &
				15%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Fire Date             " + CODE_RS::FIREDATE, &
				16%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State Emp. Account No " + CODE_RS::SEAN, &
				18%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State Code            " + CODE_RS::STCODE2, &
				9%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State Taxable Wages   " + CODE_RS::STWAGE, &
				10%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State Tax Withheld    " + CODE_RS::STTAX, &
				11%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Other State Data      " + CODE_RS::OTHDATA, &
				12%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Tax Type Code         " + CODE_RS::TAXTYP, &
				13%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Local Taxable Wages   " + CODE_RS::LOCWAGE, &
				15%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Local Tax Withheld    " + CODE_RS::LOCTAX, &
				16%, 40%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"State Control #       " + CODE_RS::STCONNUM, &
				17%, 40%)


		CASE "RT"

			!****************************************************
			! Code T Total Record
			!****************************************************

			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_RT::RECID, &
				1%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Number of Employee    " + CODE_RT::NUM_OF_EMP, &
				2%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"FICA Wages            " + CODE_RT::FICA_WAGE, &
				3%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"FICA Tips             " + CODE_RT::FICA_TIP, &
				4%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Annual Wages          " + CODE_RT::ANNWAGE, &
				5%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"FICA Withheld         " + CODE_RT::FICA_WTHLD, &
				6%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Federal Withheld      " + CODE_RT::FED_WTHLD, &
				7%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Group Term Life Ins   " + CODE_RT::LIFINS, &
				8%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Earned Income Credit  " + CODE_RT::EIC, &
				10%, 5%)


			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Deferred Comp 401     " + CODE_RT::DEFCOMP401, &
				13%, 45%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Dependent Care Ben.   " + CODE_RT::DCB, &
				14%, 45%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Non Qual Plan Sec 457 " + CODE_RT::NQP457, &
				15%, 45%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Non Qual Plan Not 457 " + CODE_RT::NQPN457, &
				16%, 45%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Medicare Wages & Tips " + CODE_RT::MED_WAGE, &
				17%, 45%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Medicare Tax          " + CODE_RT::MED_WTHLD, &
				18%, 45%)

		CASE "RF"
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Record Identifier     " + CODE_RF::RECID, &
				1%, 5%)
			ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				"Number of Employees   " + CODE_RF::NUM_OF_EMP, &
				2%, 5%)

	END SELECT

	V% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	!
	! Pause to read screen
	!
	CALL ENTR_3MESSAGE(SCOPE, "", 0%)

	RETURN

	%Page

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME 19990

 HelpError:
19990	!
	! This moved from inside error to outside so that errors occuring
	! at lower levels could be trapped.  Basic will not allow any
	! error to occur inside of an error no matter if it is in a
	! different module.
	!
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

	!
	! Handle the cases where a file couldn't be opened
	!
 ! NoFile:
 !	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
 !		"E", ERN$, FILENAME$, NUM1$(ERR))
 !
 !	GOTO ExitProgram

32767	END
