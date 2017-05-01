1	%TITLE "Sub Process to Create File Documenation File"
	%SBTTL "TK_SUBR_FILEDOCU"
	%IDENT "V3.6a Calico"

	SUB TK_SUBR_FILEDOCU(TK_FILEDICT_CDD TK_FILEDICT, &
		STRING PAR_RETURNFILE)

	!
	! COPYRIGHT (C) 1987 BY
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
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
	!	.p
	!	A process to aid in the documentation of file.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SUBR_FILEDOCU/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_SUBR_FILEDOCU
	!	$ DELETE TK_SUBR_FILEDOCU.OBJ;*
	!
	! Author:
	!
	!	05/01/87 - Robert Peterson
	!
	! Modification history:
	!
	!	07/09/87 - Kevin Handy
	!		Modified to use library files.
	!
	!	04/05/89 - Robert Peterson
	!		Major rewrite to prepare documentation.
	!
	!	07/26/89 - Aaron Redd
	!		Modified to accomodate changes in TK_FILEDICT structure.
	!
	!	07/13/90 - Frank Starman
	!		Compress output format and do bolding.
	!
	!	06/15/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	06/03/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/13/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$, STR$
	!
	!	11/30/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$LIBDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_DDL.HB"
	DECLARE  SMG_DDL_CDD DDL

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_OPN.HB"
	DECLARE  SMG_OPN_CDD OPN

	%INCLUDE "SOURCE:[TK.OPEN]TK_FILEDICT.HB"

	%INCLUDE "SOURCE:[TK.OPEN]TK_FOREIGN.HB"
	MAP	(TK_FOREIGN)	TK_FOREIGN_CDD	TK_FOREIGN

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION LIBR_NODIGSR

	DIM FILE_TYPE$(50%), &
		KEY_SIZE%(40%), &
		LOC_KEYFLD$(40%), &
		LOC_DOCLINE$(6000%), &
		LOC_FORFLD$(40%), &
		LOC_FORSTR$(40%), &
		LOC_FORREL$(40%)

	%PAGE

	!********************************
	! Temporary alteration  UT => UTL
	!********************************
	IF (TK_FILEDICT::SYSTEM = "UT")
	THEN
		SYSTEM$ = "UTL"
	ELSE
		SYSTEM$ = TK_FILEDICT::SYSTEM
	END IF

100	!
	! Get the channels from VMS
	!
	SMG_STATUS% = LIB$GET_LUN(LOC_FILE.CH%)
	GOTO ErrorGetCh IF SMG_STATUS% = LIB$_INSLUN

	GOTO 300

 ErrorGetCh:
	CALL ENTR_3MESSAGE(SCOPE, &
		"No free channels from VMS:  " + NUM1$(SMG_STATUS%), 0%)
	GOTO ExitProgram

300	!
	! Open the tk foreign file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[TK.OPEN]TK_FOREIGN.OPN"
	USE
		CONTINUE 350
	END WHEN

310	!
	! Search for first record of this structure
	!
	WHEN ERROR IN
		FIND #TK_FOREIGN.CH%, &
			KEY #1% GE TRM$(TK_FILEDICT::FILENAME), &
			REGARDLESS
	USE
		CONTINUE 350
	END WHEN

320	!
	! Get foreign key record
	!
	WHEN ERROR IN
		GET #TK_FOREIGN.CH%, REGARDLESS
	USE
		CONTINUE 350
	END WHEN

	IF TRM$(TK_FILEDICT::FILENAME) = TRM$(TK_FOREIGN::SUBSTRUCT)
	THEN
		LOC_FORCNT% = LOC_FORCNT% + 1%
		LOC_FORFLD$(LOC_FORCNT%) = TK_FOREIGN::FLDNAMES
		LOC_FORSTR$(LOC_FORCNT%) = TK_FOREIGN::STRUCT
		LOC_FORREL$(LOC_FORCNT%) = TK_FOREIGN::SUBASSOCIATE + ":" + &
			TK_FOREIGN::ASSOCIATE

		GOTO 320
	END IF

350	!
	! Close channel
	!
	CALL ASSG_FREECHANNEL(TK_FOREIGN.CH%)
	TK_FOREIGN.CH% = 0%

	!
	! Get the ddl file
	!
	CALL TK_SUBR_DDLEXTRACT(DDL, SYSTEM$, TK_FILEDICT::FILENAME, &
		FDE_STATUS%)

	CALL ENTR_3MESSAGE(SCOPE, "Creating " + &
		TRM$(TK_FILEDICT::FILENAME) + " documentation", 1%)

400	!
	! Get all open include file names
	!
	CALL FIND_FILE("SOURCE:[" + TRM$(SYSTEM$) + ".OPEN]" + &
		TRM$(TK_FILEDICT::FILENAME) + ".*", &
		FILE_TYPE$(), 0% + 1% + 2% + 4% + 8% + 16% + 32%, "", "")

	OPN::EXTENSION = ""
	OPN::FILE_NAME = ""
	OPN::ORGNIZATION = ""
	OPN::STRCTURE = ""

	OPENS% = VAL%(FILE_TYPE$(0%))
	GOTO 500 IF OPENS% = 0%

	FOR LOOP% = 1% TO OPENS%
		TEMP1%, TEMP% = INSTR(1%, FILE_TYPE$(LOOP%), ".")

		WHILE TEMP%
			TEMP% = INSTR(TEMP% + 1%, FILE_TYPE$(LOOP%), ".")
			TEMP1% = TEMP% IF TEMP% <> 0%
		NEXT

		FILE_TYPE$(LOOP%) = RIGHT(FILE_TYPE$(LOOP%), TEMP1% + 1%)
	NEXT LOOP%

	!
	! Extract all information possible from open file
	!
	CALL TK_SUBR_EXTRACTOPEN(OPN, TK_FILEDICT::FILENAME, &
		FILE_TYPE$(1%), "SOURCE:[" + SYSTEM$ + ".OPEN]")

500	!
	! Determine file and key sizes
	!
	LOC_FLDCNT% = DDL::FIELD_NUM
	LOC_KEYCNT% = OPN::KEYS_NUM

510	FOR LOOP% = 1% TO LOC_FLDCNT%
		TEMP$ = "ARRAY"
		TEMP% = INSTR(1%, DDL::FIELD_ATTRIBUTE(LOOP%), TEMP$)
		IF TEMP% <> 0%
		THEN
			TEMP$ = TRM$(RIGHT(DDL::FIELD_ATTRIBUTE(LOOP%), &
				LEN(TEMP$) + TEMP%))
			TEMP% = INSTR(1%, TEMP$, ":")
			IF TEMP%
			THEN
				START_ARRAY% = VAL%(LEFT(TEMP$,TEMP% - 1%))
				WHEN ERROR IN
					END_ARRAY% = VAL%(RIGHT(TEMP$, TEMP% + 1%))
				USE
					END_ARRAY% = 0%
				END WHEN

			END IF
		END IF

520	NEXT LOOP%

530	!
	! Determine the size of the keys
	!
	FOR LOC_KEYLOOP% = 0% TO LOC_KEYCNT%
		LOC_COLON% = INSTR(1%, OPN::KEYS(LOC_KEYLOOP%), "::")

		WHILE LOC_COLON% > 0%
			LOC_PREN% = STR$FIND_FIRST_IN_SET(RIGHT( &
				OPN::KEYS(LOC_KEYLOOP%), LOC_COLON% + 2%), &
				" ,)")
			LOC_TMPKEYFLD$ = MID(OPN::KEYS(LOC_KEYLOOP%), &
				LOC_COLON% + 2%, LOC_PREN% - 1%)
			LOC_TEST% =  0%
			LOC_TEST% = LOC_FLDLOOP% &
				IF LOC_TMPKEYFLD$ = &
				EDIT$(DDL::FIELD_NAME(LOC_FLDLOOP%), -1%) &
				FOR LOC_FLDLOOP% = 1% TO LOC_FLDCNT%
			IF LOC_TEST% <> 0%
			THEN
				LOC_PLUS$ = ""
				LOC_PLUS$ = " + " &
					IF LOC_KEYFLD$(LOC_KEYLOOP%) <> ""
				LOC_KEYFLD$(LOC_KEYLOOP%) = &
					LOC_KEYFLD$(LOC_KEYLOOP%) + &
					LOC_PLUS$ + &
					TRM$(TK_FILEDICT::FILENAME) + "::" + &
					LOC_TMPKEYFLD$
				WHEN ERROR IN
					KEY_SIZE%(LOC_KEYLOOP%) = &
						KEY_SIZE%(LOC_KEYLOOP%) + &
						VAL%(DDL::FIELD_SIZE(LOC_TEST%))
				USE
					KEY_SIZE%(LOC_KEYLOOP%) = &
						KEY_SIZE%(LOC_KEYLOOP%)
				END WHEN

			END IF

			LOC_COLON% = INSTR(LOC_COLON% + LOC_PREN% + 2%, &
				OPN::KEYS(LOC_KEYLOOP%), "::")
540		NEXT

	NEXT LOC_KEYLOOP%

1000	!
	! Open the document for output
	!
	WHEN ERROR IN
		OPEN PAR_RETURNFILE FOR OUTPUT AS FILE #LOC_FILE.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to open documentation file", 0%)
		CONTINUE ExitProgram
	END WHEN

1100	!
	! Create documentation
	!
	!PRINT #LOC_FILE.CH%, "^*DATABASE\* - "; FN_RNO$(TRM$(SYSTEM$))
	PRINT #LOC_FILE.CH%, ".title " + FN_RNO$(TRM$(TK_FILEDICT::DESCR))
	PRINT #LOC_FILE.CH%, ".subtitle"
	PRINT #LOC_FILE.CH%, ".date"
	PRINT #LOC_FILE.CH%, ".flags bold"
	PRINT #LOC_FILE.CH%, ".enable bolding"
	PRINT #LOC_FILE.CH%, "^*DATABASE\* - RMS"
	PRINT #LOC_FILE.CH%, ".b"

	!
	! Look up structure definition in library
	!
	ST% = LIBR_NODIGSR("REF:HELP_" + TRM$(SYSTEM$), &
		TRM$(TK_FILEDICT::FILENAME) + "_CDD", &
		LOC_DOCLINE$())

	LOC_DOCLINE$(0%) = "0" IF LOC_DOCLINE$(0%) = "-1"

	LOC_DOCLINE% = VAL%(LOC_DOCLINE$(0%))

	!
	! Print file structure documentation.
	!
	PRINT #LOC_FILE.CH%, "^*Table Description\* - "; &
		FN_RNO$(TRM$(TK_FILEDICT::DESCR))
	PRINT #LOC_FILE.CH%, ".LM +5"

	!
	! For index
	!
	PRINT #LOC_FILE.CH%, ".x ";TRM$(SYSTEM$); ">"; &
		TRM$(TK_FILEDICT::FILENAME)
	PRINT #LOC_FILE.CH%, ".x ";TRM$(TK_FILEDICT::FILENAME); ">"; &
		TRM$(SYSTEM$)

	!
	! If structure documentation is missing then
	! fill in with best effort
	!
	IF LOC_DOCLINE%
	THEN
		GOSUB ExternalDocu
	END IF

	!
	! Reset margin
	!
	PRINT #LOC_FILE.CH%, ".b"
	PRINT #LOC_FILE.CH%, ".LM -5"


	!
	! Print File attribute documentation
	!
	PRINT #LOC_FILE.CH%, "^*Table attributes\*"
	PRINT #LOC_FILE.CH%, ".br"
	PRINT #LOC_FILE.CH%, ".lm +5"

	PRINT #LOC_FILE.CH%, "Table name - "; &
		FN_RNO$(TRM$(TK_FILEDICT::FILENAME))
	PRINT #LOC_FILE.CH%, ".br"

	!
	! Is there an extension
	!
	IF TRM$(OPN::EXTENSION) <> ""
	THEN
		PRINT #LOC_FILE.CH%, "Table Extension - "; &
			FN_RNO$(TRM$(OPN::EXTENSION))
		PRINT #LOC_FILE.CH%, ".br"
	END IF

	!
	! Is there an organization
	!
	IF TRM$(OPN::ORGNIZATION) <> ""
	THEN
		PRINT #LOC_FILE.CH%, "Table Organization - "; &
			FN_RNO$(TRM$(OPN::ORGNIZATION))
		PRINT #LOC_FILE.CH%, ".br"
	END IF

	!
	! Is there a structure
	!
	IF TRM$(OPN::STRCTURE) <> ""
	THEN
		PRINT #LOC_FILE.CH%, "Table Structure - "; &
			FN_RNO$(TRM$(OPN::STRCTURE))
	END IF
	PRINT #LOC_FILE.CH%, ".lm -5"
	PRINT #LOC_FILE.CH%, ".b"

	!
	! Print Field name documentation
	!
	PRINT #LOC_FILE.CH%, "^*Field Name(s)\*"

	PRINT #LOC_FILE.CH%, ".br"
	PRINT #LOC_FILE.CH%, ".lm +5"

	!
	! Loop thru all field names
	!
	FOR LOC_FLDLOOP% = 1% TO LOC_FLDCNT%

		! Add to cross reference
		PRINT #LOC_FILE.CH%, ".x "; &
			FN_RNO$(TRM$(TK_FILEDICT::FILENAME)); ">"; &
			FN_RNO$(TRM$(DDL::FIELD_NAME(LOC_FLDLOOP%)))

		PRINT #LOC_FILE.CH%, ".x "; &
			FN_RNO$(TRM$(DDL::FIELD_NAME(LOC_FLDLOOP%))); ">"; &
			FN_RNO$(TRM$(TK_FILEDICT::FILENAME))

		! Print field name and description
		PRINT #LOC_FILE.CH%,FN_RNO$(TRM$(TK_FILEDICT::FILENAME)); &
			"::"; FN_RNO$(TRM$(DDL::FIELD_NAME(LOC_FLDLOOP%))); &
			" - "; FN_RNO$(TRM$(DDL::FIELD_DESC(LOC_FLDLOOP%)))

		PRINT #LOC_FILE.CH%, ".br"
		PRINT #LOC_FILE.CH%, ".lm +12"

		! Print field classifier
		IF TRM$(DDL::FIELD_ELEMENT(LOC_FLDLOOP%)) <> ""
		THEN
			PRINT #LOC_FILE.CH%, "Classifier - "; &
				FN_RNO$(TRM$(DDL::FIELD_ELEMENT(LOC_FLDLOOP%)));", "
		END IF

		! Print field type
		IF TRM$(DDL::FIELD_TYPE(LOC_FLDLOOP%)) <> ""
		THEN
			PRINT #LOC_FILE.CH%, "Type - "; &
				FN_RNO$(TRM$(DDL::FIELD_TYPE(LOC_FLDLOOP%))); &
				", "
		END IF

		! Print field size
		IF TRM$(DDL::FIELD_SIZE(LOC_FLDLOOP%)) <> ""
		THEN
			PRINT #LOC_FILE.CH%, "Size - "; &
				FN_RNO$(TRM$(DDL::FIELD_SIZE(LOC_FLDLOOP%))); &
				", "
		END IF

		! Print array if array
		IF TRM$(DDL::FIELD_ATTRIBUTE(LOC_FLDLOOP%)) <> ""
		THEN
			PRINT #LOC_FILE.CH%, "Array - "; &
				FN_RNO$(TRM$(DDL::FIELD_ATTRIBUTE(LOC_FLDLOOP%)));", "
		END IF

		!
		! Look up field definition in library
		!
		ST% = LIBR_NODIGSR("REF:HELP_" + TRM$(SYSTEM$), &
			TRM$(DDL::FIELD_NAME(LOC_FLDLOOP%)), &
			LOC_DOCLINE$())

		LOC_DOCLINE$(0%) = "0" IF LOC_DOCLINE$(0%) = "-1"

		LOC_DOCLINE% = VAL%(LOC_DOCLINE$(0%))

		!
		! If there is documentation the add to file
		!
		IF LOC_DOCLINE%
		THEN
			GOSUB ExternalDocu
		END IF

		!
		! Now set margin for next field name
		!
		PRINT #LOC_FILE.CH%, ".br"
		PRINT #LOC_FILE.CH%, ".lm -12"

	NEXT LOC_FLDLOOP%

	!
	! Set margin back
	!
	PRINT #LOC_FILE.CH%, ".lm -5"

	!
	! Print indices if the file organization is not relative
	!
	IF OPN::ORGNIZATION = "INDEXED"
	THEN
		!
		! Print indices documentation
		!
		PRINT #LOC_FILE.CH%, ".b"
		PRINT #LOC_FILE.CH%, "^*Keys\*"
		PRINT #LOC_FILE.CH%, ".br"
		PRINT #LOC_FILE.CH%, ".lm +5"

		!
		! Print text for each key
		!
		FOR LOC_KEYLOOP% = 0% TO LOC_KEYCNT%
			IF LOC_KEYLOOP% = 0%
			THEN
				! If primary key then print this
				PRINT #LOC_FILE.CH%, ".b"
				PRINT #LOC_FILE.CH%, "Primary Key"
			ELSE
				! If alternate key then print this
				PRINT #LOC_FILE.CH%, ".b"
				PRINT #LOC_FILE.CH%, "Alternate Key "; &
					NUM1$(LOC_KEYLOOP%)
			END IF

			!
			! Print key size and fields included
			PRINT #LOC_FILE.CH%, ".lm +5"
			PRINT #LOC_FILE.CH%, "Size - "; &
				NUM1$(KEY_SIZE%(LOC_KEYLOOP%))
			PRINT #LOC_FILE.CH%, ".br"
			!
			! Are duplicates allowed
			!
			IF INSTR(1%, EDIT$(OPN::KEYS(LOC_KEYLOOP%), -1%), &
				"DUPLICATES")
			THEN
				PRINT #LOC_FILE.CH%, "Duplicates Allowed"
				PRINT #LOC_FILE.CH%, ".br"
			END IF

			!
			! Are changes allowed
			!
			IF INSTR(1%, EDIT$(OPN::KEYS(LOC_KEYLOOP%), -1%), &
				"CHANGES")
			THEN
				PRINT #LOC_FILE.CH%, "Changes Allowed"
				PRINT #LOC_FILE.CH%, ".br"
			END IF


			!
			! Print all fields include in key
			!
			IF LOC_KEYFLD$(LOC_KEYLOOP%) <> ""
			THEN
				PRINT #LOC_FILE.CH%, "Field(s) - "; &
					FN_RNO$(LOC_KEYFLD$(LOC_KEYLOOP%))
			END IF

			PRINT #LOC_FILE.CH%, ".br"
			PRINT #LOC_FILE.CH%, ".lm -5"
		NEXT LOC_KEYLOOP%
		!
		! Set margin back 5
		!
		PRINT #LOC_FILE.CH%, ".lm -5"
	END IF

	!
	! If there are foreign keys then print associations
	!
	IF LOC_FORCNT%
	THEN
		!
		! Print Associations documentation
		!
		PRINT #LOC_FILE.CH%, "^*Association(s)\*"
		PRINT #LOC_FILE.CH%, ".b"
		PRINT #LOC_FILE.CH%, ".lm +5"

		FOR LOC_FORLOOP% = 1% TO LOC_FORCNT%
			PRINT #LOC_FILE.CH%, "Foreign key "; NUM1$(LOC_FORLOOP%)
			PRINT #LOC_FILE.CH%, ".br"
			PRINT #LOC_FILE.CH%, ".lm +5"
			PRINT #LOC_FILE.CH%, "Field(s) - "; FN_RNO$( &
				LOC_FORFLD$(LOC_FORLOOP%))
			PRINT #LOC_FILE.CH%, ".br"
			PRINT #LOC_FILE.CH%, "Structure - "; FN_RNO$( &
				LOC_FORSTR$(LOC_FORLOOP%))
			PRINT #LOC_FILE.CH%, ".br"
			PRINT #LOC_FILE.CH%, "Relationship - "; FN_RNO$( &
				LOC_FORREL$(LOC_FORLOOP%))
			PRINT #LOC_FILE.CH%, ".br"
			PRINT #LOC_FILE.CH%, ".lm -5"
		NEXT LOC_FORLOOP%
		!
		! Set margin back 5
		!
		PRINT #LOC_FILE.CH%, ".lm -5"
	END IF

 ExitProgram:
	CLOSE #LOC_FILE.CH%

	CALL ENTR_3MESSAGE(SCOPE, "Documentation for " + &
		TRM$(TK_FILEDICT::FILENAME) + " has been created", 1%)

	SMG_STATUS% = LIB$FREE_LUN(LOC_FILE.CH%)

1220	EXIT SUB

 ExternalDocu:
	!********************************************************************
	! Print external documentation to local file
	!********************************************************************
	LOC_BLTEST% = 0%

	FOR LOOP% = 1% TO LOC_DOCLINE%
		LOC_TEMP$ = EDIT$(LEFT(LOC_DOCLINE$(LOOP%), 2%), -1%)

		SELECT LOC_TEMP$
		CASE ".B"
			IF LOOP% <> 1% AND LOC_BLTEST% = 0%
			THEN
				PRINT #LOC_FILE.CH%, LOC_DOCLINE$(LOOP%)
				LOC_BLTEST% = -1%
			END IF
		CASE "^*"
			LOC_TEMP% = LEN(LOC_DOCLINE$(LOOP%))
			IF RIGHT(LOC_DOCLINE$(LOOP%), LOC_TEMP% - 1%) <> "\*"
			THEN
				PRINT #LOC_FILE.CH%, LOC_DOCLINE$(LOOP%)
				LOC_BLTEST% = 0%
			END IF
		CASE ".X"
			! Don't print at all
		CASE ELSE
			PRINT #LOC_FILE.CH%, LOC_DOCLINE$(LOOP%)
			LOC_BLTEST% = 0%
		END SELECT
	NEXT LOOP%

	RETURN

	%Page

18000	!*******************************************************************
	! Runoff text conversion function.
	!
	! Converts text into a format so that runoff will display
	! it as is, and not convert all of hte stars, underlines,
	! and whatnot.
	!*******************************************************************

	DEF FN_RNO$(A$)

		X$ = FN_FIX$(A$, "_", "__")
		X$ = FN_FIX$(X$, "*", "_*")
		X$ = FN_FIX$(X$, "^", "_^")
		X$ = FN_FIX$(X$, "&", "_&")
		X$ = FN_FIX$(X$, "$", "_$")
		X$ = FN_FIX$(X$, "%", "_%")

		FN_RNO$ = X$
	FNEND

	%PAGE

	!*******************************************************************
	! FN_FIX function is used by FN_RNO
	!*******************************************************************

	DEF FN_FIX$(A$, B$, C$)

		X$ = A$

		ILL% = 0%

 FnfixLoop:	ILL% = INSTR(ILL% + 1%, X$, B$)

		IF ILL%
		THEN
			X$ = LEFT(X$, ILL% - 1%) + C$ + &
				RIGHT(X$, ILL% + LEN(B$))
			ILL% = ILL% + LEN(C$) - 1%
			GOTO FnfixLoop
		END IF

		FN_FIX$ = X$
	FNEND

32767	END SUB
