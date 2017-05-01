1	%TITLE "Assign Batch Number Program"
	%SBTTL "ASSG_BATCH"
	%IDENT "V3.6a Calico"

	SUB ASSG_BATCH(UTL_BATCH.CH%, &
		JOUR_DEV_NAME$, &
		OPTION$, &
		BATCH_NUMBER$, &
		BFILE$, &
		DESCR$, &
		STAT$, &
		UTLFILE$, &
		U1FILE$, &
		EXIT.STATUS%)

	!
	! COPYRIGHT (C) 1986 BY
	! Computer Management Center
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Assigns a batch number to a post/update.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	UTL_BATCH.CH%
	!		The passed variable for the
	!		batch channel for the BATCH file.
	!
	!	OPTION$
	!		The options to modify the files.
	!	.table
	!			"I" Initilize for POST
	!			"M" Modify POST record
	!			"F" Finish/Cancel POST (deletes record)
	!	.endtable
	!
	!	DESCR$
	!		The passed description of the file.
	!
	!	UTL_FILE$
	!		The passed file to be used.
	!
	!	EXIT.STATUS% gets sets as follows:
	!	.table
	!		1%	- This process has been interrupted somehow
	!
	!		2%	- Batch control record is missing
	!
	!		4%	- Error - abort post
	!	.endtable
	!
	!	Returned value
	!		A created UTL_BATCH file for a specified Journal file.
	!
	! Example:
	!
	!	ASSG_BATCH(9%, 'ASSIGN', 'I', '001000', '100000', &
	!		'BATCH CONTROL', '', 'UTL_LIB', '', 4%)
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ASSG_BATCH
	!	$ LIB FUNC_LIB:CMCFUN/REP ASSG_BATCH
	!	$ DELETE ASSG_BATCH.OBJ;*
	!
	! AUTHOR:
	!
	!	12/15/86 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	02/09/88 - Robert Peterson
	!		Add the journal device para so that utl batch
	!		file can be create on the directory that the
	!		journal file exists on.  Get around problem of
	!		working on different directories and having
	!		the post process get confessed.
	!
	!	03/14/92 - Kevin Handy
	!		Clean up vars (checkvar)
	!
	!	08/14/92 - Kevin Handy
	!		Attempt to fix someone's changing all '#'s to
	!		tab characters. (Who knows why they did it)
	!
	!	03/11/95 - Kevin Handy
	!		Trap error at 3000 if unable to find batch
	!		record to delete.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/23/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/04/99 - Kevin Handy
	!		Changed field "program" to "programname"
	!
	!	03/08/99 - Kevin Handy
	!		Use "WHEN ERROR IN"
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ routines
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_BATCH_CONTROL.HB"
	MAP (UTL_BATCH_CONTROL)	UTL_BATCH_CONTROL_CDD	UTL_BATCH_CONTROL

	RECORD UTL_BATCH
		STRING	FILE_NAME = 6%
		RFA	RFA_RFA
	END RECORD UTL_BATCH

	MAP (UTL_BATCH) &
		UTL_BATCH	UTL_BATCH

	%PAGE

	PRGNAM$ = READ_SYSPN

100	!
	! Open main file (existing) for modification
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_BATCH_CONTROL.MOD"
	USE
		CONTINUE 150 IF ERR = 5%
		EXIT HANDLER
	END WHEN

	GOTO 200

150	CALL ENTR_3MESSAGE(SCOPE, "Creating new batch control file", 1%)
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_BATCH_CONTROL.CRE"

	UTL_BATCH_CONTROL::BATCH	= "000000"
	UTL_BATCH_CONTROL::PROGRAMNAME	= ""
	UTL_BATCH_CONTROL::BFILE	= "100000"
	UTL_BATCH_CONTROL::DSTART	= DATE_TODAY
	UTL_BATCH_CONTROL::TSTART	= TIME_NOW
	UTL_BATCH_CONTROL::USTATUS	= ""
	UTL_BATCH_CONTROL::DESCR	= "Batch control record"
	UTL_BATCH_CONTROL::UTLFILE	= ""
	UTL_BATCH_CONTROL::U1FILE	= ""

	PUT #UTL_BATCH_CONTROL.CH%

	%PAGE

200	!*******************************************************************
	! Handle whatever the user wants to do
	!*******************************************************************

	EXIT.STATUS% = 0%

	SELECT LEFT(OPTION$, 1%)

	!
	! Initilize for POST
	!
	CASE "I"
		GOSUB 1000

	!
	! Modify POST record
	!
	CASE "M"
		GOSUB 2000

	!
	! Finish/Cancel POST (deletes record)
	!
	CASE "F"
		GOSUB 3000

	!
	! Undefined option
	!
	CASE ELSE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Undefined option in batch assign process (" + &
			OPTION$ + ")", 0%)
		EXIT.STATUS% = EXIT.STATUS% OR 4%

	END SELECT

	CLOSE UTL_BATCH_CONTROL.CH%

	CALL ASSG_FREECHANNEL(UTL_BATCH_CONTROL.CH%)

 ExitSubroutine:

	EXIT SUB

	%PAGE

1000	!*******************************************************************
	! Get a batch number
	!*******************************************************************

	RESET #UTL_BATCH_CONTROL.CH%

	BATCH_IN_PROCESS% = 0%

1025	WHEN ERROR IN
		GET #UTL_BATCH_CONTROL.CH%, REGARDLESS
	USE
		CONTINUE 1030
	END WHEN

	GOTO 1025 IF UTL_BATCH_CONTROL::BATCH = "000000"

	IF (UTL_BATCH_CONTROL::PROGRAMNAME = PRGNAM$) AND &
		(UTL_BATCH_CONTROL::BFILE = LEFT(BFILE$, 8%)) AND &
		(UTL_BATCH_CONTROL::UTLFILE = LEFT(UTLFILE$, 7%)) AND &
		(UTL_BATCH_CONTROL::U1FILE = LEFT(U1FILE$, 7%))
	THEN
		BATCH_NUMBER$ = UTL_BATCH_CONTROL::BATCH
		EXIT.STATUS% = EXIT.STATUS% OR 1%
	ELSE
		BATCH_IN_PROCESS% = BATCH_IN_PROCESS% + 1%
	END IF

	GOTO 1025

1030	!
	! Notify if more postings are in process
	!
	IF BATCH_IN_PROCESS%
	THEN
		TEMP_IDENT$ = SCOPE::PRG_IDENT
		TEMP_ITEM$ = SCOPE::PRG_ITEM

		SCOPE::PRG_IDENT = "FUNC"
		SCOPE::PRG_ITEM = "OTHER_POST"

		CALL ENTR_3MESSAGE(SCOPE, NUM1$(BATCH_IN_PROCESS%) + &
			" other posting(s) in process", 0%)

		SELECT SCOPE::SCOPE_EXIT
		!
		! Exit
		!
		CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			EXIT.STATUS% = EXIT.STATUS% OR 4%

		!
		! Normal key typed
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO, SMG$K_TRM_F7

		!
		! Bad key typed
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 1030

		END SELECT

		SCOPE::PRG_IDENT = TEMP_IDENT$
		SCOPE::PRG_ITEM = TEMP_ITEM$
	END IF

1040	IF EXIT.STATUS% = 0%
	THEN
		WHEN ERROR IN
			!
			! Get control file control record
			!
			GET #UTL_BATCH_CONTROL.CH%, KEY #0% EQ "000000"

			!
			! Allocate the next batch number
			!
			BATCH_NUMBER$, UTL_BATCH_CONTROL::BFILE = &
				FORMAT$(VAL%(UTL_BATCH_CONTROL::BFILE) + 1%, &
				"<0>#####")

			UPDATE #UTL_BATCH_CONTROL.CH%

			!
			! Create a header record
			!
			UTL_BATCH_CONTROL::BATCH	= BATCH_NUMBER$
			UTL_BATCH_CONTROL::PROGRAMNAME	= PRGNAM$
			UTL_BATCH_CONTROL::BFILE	= BFILE$
			UTL_BATCH_CONTROL::DSTART	= DATE_TODAY
			UTL_BATCH_CONTROL::TSTART	= TIME_NOW
			UTL_BATCH_CONTROL::USTATUS	= STAT$
			UTL_BATCH_CONTROL::DESCR	= DESCR$
			UTL_BATCH_CONTROL::UTLFILE	= UTLFILE$
			UTL_BATCH_CONTROL::U1FILE	= U1FILE$

			PUT #UTL_BATCH_CONTROL.CH%
		USE
			IF ERR = 155%
			THEN
				EXIT.STATUS% = EXIT.STATUS% OR 2%
				CONTINUE ExitSubroutine
			END IF
			EXIT HANDLER
		END WHEN
	END IF

1050	!
	! Open UTL_BATCH if it exists otherwise create it
	!
 !	%INCLUDE "SOURCE:[UTL.OPEN]UTL_BATCH.CRE"

	CALL ASSG_CHANNEL(UTL_BATCH.CH%, STAT%)
	CALL READ_DEVICE("UTL_BATCH", UTL_BATCH.DEV$, STAT%)
	CALL READ_PROTECTION("UTL_BATCH", UTL_BATCH.PRO$, STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$, STAT%)
	CALL WRIT_CURPROTECTION(UTL_BATCH.PRO$, STAT%)

	UTL_BATCH.NAME$ = UTL_BATCH.DEV$ + "UTL_BATCH_" + BATCH_NUMBER$ + ".CTR"

	OPEN UTL_BATCH.NAME$ AS FILE UTL_BATCH.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP UTL_BATCH, &
		PRIMARY KEY &
			UTL_BATCH::FILE_NAME DUPLICATES, &
		ACCESS MODIFY, ALLOW MODIFY

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)


1090	RETURN

	%PAGE

2000	!*******************************************************************
	! Modify the record
	!*******************************************************************

	WHEN ERROR IN
		GET #UTL_BATCH_CONTROL.CH%, KEY #0% EQ BATCH_NUMBER$
	USE
		EXIT HANDLER
	END WHEN

	!
	! Create a header record
	!
	UTL_BATCH_CONTROL::BFILE	= BFILE$
	UTL_BATCH_CONTROL::USTATUS	= STAT$
	UTL_BATCH_CONTROL::DESCR	= DESCR$
	UTL_BATCH_CONTROL::UTLFILE	= UTLFILE$
	UTL_BATCH_CONTROL::U1FILE	= U1FILE$

	WHEN ERROR IN
		UPDATE #UTL_BATCH_CONTROL.CH%
	USE
		EXIT HANDLER
	END WHEN

	RETURN

	%PAGE

3000	!*******************************************************************
	! Finish up POST by deleting control record
	!*******************************************************************

	!
	! Delete the UTL_BATCH file
	!
	WHEN ERROR IN
		GET #UTL_BATCH_CONTROL.CH%, KEY #0% EQ BATCH_NUMBER$

		DELETE #UTL_BATCH_CONTROL.CH%

		CLOSE UTL_BATCH.CH%
	USE
		CONTINUE 3050 IF ERR = 155%
		EXIT HANDLER
	END WHEN

3050	CALL READ_DEVICE(JOUR_DEV_NAME$, UTL_BATCH.DEV$, STAT%)
	UTL_BATCH.NAME$ = UTL_BATCH.DEV$ + "UTL_BATCH_" + BATCH_NUMBER$ + ".CTR"
	SMG_STATUS% = LIB$DELETE_FILE(UTL_BATCH.NAME$)

	RETURN

32767	END SUB
