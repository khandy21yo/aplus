1	%TITLE "Customer Name/Address Mailmerge Program"
	%SBTTL "AR_SPEC_MAILMERGE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.
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
	! ID:AR050
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program generates a mailmerge file for
	!	WordPerfect from the customer file.
	!	.lm -5
	!
	! Index:
	!	.x Mail Merge>Customer
	!	.x Customer>Mail Merge
	!
	! Option:
	!
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Example:
	!
	!
	! Author:
	!
	!	01/28/91 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_SPEC_MAILMERGE/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_SPEC_MAILMERGE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_SPEC_MAILMERGE.OBJ;*
	!
	! Modification history:
	!
	!	08/25/92 - Kevin Handy
	!		Finished writing, and clean up. (Seems to work
	!		but doesn't return correctly back to the menu)
	!
	!	08/25/92 - Kevin Handy
	!		Attempt to fix chain back to menu.
	!
	!	09/16/92 - Kevin Handy
	!		Fixed several bugs in generated WP codes.
	!		Fixed bug in output routine (18000)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	07/23/96 - Kevin Handy
	!		Added zip code field.
	!		Reformat source code.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
	!
	!	09/08/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTACT.HB"
	MAP (AR_CONTACT)	AR_CONTACT_CDD		AR_CONTACT

	MAP (TEST) BUFFER$ = 512%

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initilize report
	!
	CALL READ_INITIALIZE
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL ASSG_CHANNEL(FOUT.CH%, STAT%)

	LINPUT "Output file"; FOUT$

	OPEN FOUT$ FOR OUTPUT AS FILE 1%, &
		ORGANIZATION SEQUENTIAL FIXED, &
		RECORDSIZE 512%, &
		MAP TEST

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN


330	!

	%PAGE

	PREBUFFER$ = ""

	BOLDON$ = '195'C + '12'C + '195'C
	BOLDOFF$ = '196'C + '12'C + '196'C

	!
	! Field Name Codes (defining the names of the fields)
	!
	FIELDNAMEON$ = '222'C+'98'C+'4'C+'0'C+'4'C+'0'C+'98'C+'222'C
	FIELDNAMEOFF$ = '222'C+'52'C+'6'C+'0'C+'1'C+'0'C+'6'C+'0'C+'52'C+'222'C + &
		'212'C+'0'C+'28'C+'0'C+'0'C+'22'C+'0'C+'0'C+'1'C+'0'C+'0'C+ &
		'0'C+'0'C+'0'C+'0'C+'0'C+'0'C+'0'C+'48'C+'42'C+'48'C+'42'C+ &
		'48'C+'42'C+'176'C+'4'C+'176'C+'4'C+'28'C+'0'C+'0'C+'212'C+ &
		'12'C

	!
	! Record mapping stuff
	!
	ENDFIELD$ = '222'C+'49'C+'4'C+'0'C+'4'C+'0'C+'49'C+'222'C+'10'C
	ENDRECORD$ =  '222'C+'52'C+'6'C+'0'C+'0'C+'0'C+'6'C+'0'C+'52'C+'222'C+ &
		'212'C+'0'C+'28'C+'0'C+'0'C+'22'C+'0'C+'0'C+'1'C+'0'C+'0'C+ &
		'0'C+'0'C+'0'C+'0'C+'0'C+'0'C+'0'C+'48'C+'42'C+'48'C+'42'C+ &
		'48'C+'42'C+'176'C+'4'C+'176'C+'4'C+'28'C+'0'C+'0'C+'212'C+ &
		'12'C

	!*******************************************************************
	! Generate Headers for WP file
	!*******************************************************************

	!
	! Tell it it is a WP file
	!
	NEWTEXT$ = '255'c+'87'c+'80'c+'67'c+'76'c+'0'c+'0'c+'0'c+'1'c+'10'c+ &
		'0'c+'0'c+'0'c+'0'c+'0'c+'0'c+'251'c+'255'c+'5'c+'0'c+ &
		'50'c+'0'c+'0'c+'0'c+'0'c+'0'c+'6'c+'0'c+'8'c+'0'c+ &
		'0'c+'0'c+'66'c+'0'c+'0'c+'0'c+'8'c+'0'c+'2'c+'0'c+ &
		'0'c+'0'c+'74'c+'0'c+'0'c+'0'c+'0'c+'0'c+'0'c+'0'c+ &
		'0'c+'0'c+'0'c+'0'c+'0'c+'0'c+'0'c+'0'c+'0'c+'0'c+ &
		'0'c+'0'c+'0'c+'0'c+'0'c+'0'c+'8'c+'0'c+'124'c+'0'c+ &
		'120'c+'0'c+'0'c+'0'c+'0'c+'0'c

	GOSUB AddText

	!
	! Define the fields we will be outputing
	!
	NEWTEXT$ = FIELDNAMEON$ + &
		"Number~Name~Add~Phone~Alpha~Zip~~" + &
		FIELDNAMEOFF$
	GOSUB AddText

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #AR_35CUSTOM.CH%
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		CONTINUE 17025 IF ERR = 155%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

17025	!
	! Check current record
	!

	!
	! Print out one line
	!
	NEWTEXT$ = EDIT$(AR_35CUSTOM::CUSNUM, 4% + 8% + 128%) +  ENDFIELD$
	GOSUB AddText
	NEWTEXT$ = EDIT$(AR_35CUSTOM::CUSNAM, 4% + 8% + 128%) + ENDFIELD$
	GOSUB AddText

	NEWTEXT$ = ""
	NEWTEXT$ = NEWTEXT$ + &
		EDIT$(AR_35CUSTOM::ADD1, 4% + 8% + 128%) + '10'C &
		IF EDIT$(AR_35CUSTOM::ADD1, 4% + 8% + 128%) <> ""
	NEWTEXT$ = NEWTEXT$ + &
		EDIT$(AR_35CUSTOM::ADD2, 4% + 8% + 128%) + '10'C &
		IF EDIT$(AR_35CUSTOM::ADD2, 4% + 8% + 128%) <> ""
	NEWTEXT$ = NEWTEXT$ + &
		EDIT$(AR_35CUSTOM::ADD3, 4% + 8% + 128%) + '10'C &
		IF EDIT$(AR_35CUSTOM::ADD3, 4% + 8% + 128%) <> ""
	NEWTEXT$ = NEWTEXT$ + &
		EDIT$(AR_35CUSTOM::CITY, 4% + 8% + 128%) + ", " + &
		EDIT$(AR_35CUSTOM::STATE, 4% + 8% + 128%) + "  " + &
		EDIT$(AR_35CUSTOM::ZIP, 4% + 8% + 128%) + ENDFIELD$
	GOSUB AddText

	NEWTEXT$ = EDIT$(PRNT_PHONE(AR_35CUSTOM::PHONE, 0%), 4% + 8% + 128%) + &
		ENDFIELD$
	GOSUB AddText
	NEWTEXT$ = EDIT$(AR_35CUSTOM::ALPSRT, 4% + 8% + 128%) + ENDFIELD$
	GOSUB AddText
	NEWTEXT$ = EDIT$(AR_35CUSTOM::ZIP, 4% + 8% + 128%) + ENDFIELD$
	GOSUB AddText
	NEWTEXT$ = ENDRECORD$
	GOSUB AddText

17040	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	! (This forces out the last bit of the file, and fills the end
	! of the file full of NULL's, which makes WP happy)
	!
	NEWTEXT$ = STRING$(511%, 0%)
	GOSUB AddText

 ExitProgram:
	CLOSE #FOUT.CH%

	!
	! Exit to next program or menu
	!
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

	!*******************************************************************
	! Add text to output file
	!*******************************************************************

 AddText:
	PREBUFFER$ = PREBUFFER$ + NEWTEXT$

18000	IF LEN(PREBUFFER$) > 512%
	THEN
		!
		! Prebuffer overflow, force out to WP file.
		!
		BUFFER$ = PREBUFFER$
		PUT #1%
		PREBUFFER$ = RIGHT(PREBUFFER$, 513%)
		GOTO 18000
	END IF

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
