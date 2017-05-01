1	%TITLE "MCL Interpreter"
	%SBTTL "FUNC_MCLCOMMAND"
	%IDENT "V3.6a Calico"

	FUNCTION LONG FUNC_MCLCOMMAND(SCOPE_STRUCT SCOPE, &
		STRING COMMAND(), &
		LONG CMCMACRO, &
		STRING COMMAND_IN, &
		STRING COMMAND_OUT)

	!
	! COPYRIGHT (C) 1989 BY
	! Computer Management Center
	!
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
	!	.p
	!	This function translates a MCL command.
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:FUNC_MCLCOMMAND/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP FUNC_MCLCOMMAND
	!	$ DELETE FUNC_MCLCOMMAND.OBJ;*
	!
	! AUTHOR:
	!
	!	02/14/90 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	08/14/91 - Kevin Handy
	!		Removed A+.
	!
	!	03/14/92 - Kevin Handy
	!		Changed "CMC$_ERROR" to "CMC$_UNTERR".
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/17/95 - Kevin Handy
	!		Fix call to HELP_34MESSAGE.
	!
	!	09/18/95 - Kevin Handy
	!		Remove extra include of constants.inc.
	!
	!	09/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/04/99 - Kevin Handy
	!		Changed variable 'PROGRAM' to 'PROGRAMNAME'
	!--
	%PAGE

	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	DECLARE STRING SUBCOMMAND(80%)
	DECLARE STRING MCLCOMMAND
	DECLARE STRING PROGRAMNAME
	DECLARE LONG   EXIT_STATUS
	DECLARE LONG   MACROTYPE
	DECLARE WORD   SPC
	DECLARE WORD   MATCH
	DECLARE WORD   SLASH
	DECLARE WORD   I
	DECLARE WORD   J
	DECLARE WORD   LEVEL

	PROGRAMNAME = "FUNC_MCLCOMMAND"

	!
	! Assume a good command
	!
	EXIT_STATUS = CMC$_NORMAL

	MATCH, LEVEL   = 0%
	COMMAND_OUT = ""

	SUBCOMMAND(0%) = EDIT$(COMMAND_IN, 8% + 128%) + "/"
	SLASH = INSTR(1%, SUBCOMMAND(0%), "/")

	WHILE SLASH
		LEVEL = LEVEL + 1%
		SUBCOMMAND(LEVEL) = LEFT(SUBCOMMAND(0%), SLASH - 1%)
		SUBCOMMAND(0%) = RIGHT(SUBCOMMAND(0%), SLASH + 1%)
		SLASH = INSTR(1%, SUBCOMMAND(0%), "/")
	NEXT

 CompCommand:
	I = I + 1%
	J = 1%
	WHILE (COMMAND(J) <> "")
		IF LEFT(COMMAND(J), LEN(SUBCOMMAND(I))) = SUBCOMMAND(I)
		THEN
			COMMAND_OUT = COMMAND_OUT + COMMAND(J)
			MATCH = MATCH + 1%
			MACROTYPE = J
		END IF

		J = J + 1%
	NEXT

	SELECT MATCH
	CASE 1%
		!
		! Found a good verb
		!
		SUBCOMMAND(I) = COMMAND_OUT

		IF LEVEL >= I + 1%
		THEN
			MATCH = 0%
			COMMAND_OUT = COMMAND_OUT + "/"

			SELECT SUBCOMMAND(1%)

			CASE "BATCH"
				COMMAND(1%) = "MONITOR"
				COMMAND(2%) = ""

			CASE "CARRIER"
				COMMAND(1%) = "LIST"
				COMMAND(2%) = ""

			CASE "COUNTRY"
				COMMAND(1%) = "LIST"
				COMMAND(2%) = ""

			CASE "DEVICE"
				COMMAND(1%) = "LIST"
				COMMAND(2%) = ""

			CASE "MACRO"
				COMMAND(1%) = "DELETE"
				COMMAND(2%) = "LIST"
				COMMAND(3%) = ""

			CASE "PKFORM"
				COMMAND(1%) = "LIST"
				COMMAND(2%) = ""

			CASE "PKMATERIAL"
				COMMAND(1%) = "LIST"
				COMMAND(2%) = ""

			CASE "PROFILE"
				COMMAND(1%) = "PERIOD"
				COMMAND(2%) = "STRUCTURE"
				COMMAND(3%) = ""

			CASE "REPORT"
				COMMAND(1%) = "DESTINATION"
				COMMAND(2%) = "PRINT"
				COMMAND(3%) = "SETTINGS"
				COMMAND(4%) = ""

			CASE "SYSTEM"
				COMMAND(1%) = "INSTALL"
				COMMAND(2%) = "LIST"
				COMMAND(3%) = ""

			CASE "TERMS"
				COMMAND(1%) = "LIST"
				COMMAND(2%) = ""

			END SELECT

			IF SUBCOMMAND(I + 1%) = ""
			THEN
				EXIT_STATUS = CMC$_WARNING

				CALL HELP_34MESSAGE(SCOPE, &
					"qualifier name is missing " + &
					COMMAND_IN, &
					"W", PROGRAMNAME, "", "NOKEYW")
	!++
	! Warning:NOKEYW
	!	^*Qualifier Name is Missing\*
	!	.b
	!	.lm +5
	!	^*Explanation:\*
	!	.b
	!	A slash character (/) is present in a command but is not
	!	followed by a qualifier keyword name.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Re-enter the command specifying the qualifier or removing
	!	the slash.
	!	.lm -5
	!
	! Index:
	!
	!--
				GOTO ExitFunction
			END IF

			IF I>2%
			THEN
				EXIT_STATUS = CMC$_WARNING

				CALL HELP_34MESSAGE(SCOPE, &
					"conflicting qualifiers " + COMMAND_IN, &
					"F", PROGRAMNAME, "", "CONFQUAL")
				GOTO ExitFunction
			END IF

			GOTO CompCommand
		END IF

	CASE 0%
		!
		! Not found
		!
		EXIT_STATUS = CMC$_WARNING

		IF I = 1%
		THEN
			CALL HELP_34MESSAGE(SCOPE, &
				"unrecognized command verb " + COMMAND_IN, &
				"W", PROGRAMNAME, "", "IVVERB")
	!++
	! Warning:IVVERB
	!	^*Unrecognized Command Verb\*
	!	.b
	!	.lm +5
	!	^*Explanation:\*
	!	.b
	!	The first word of the command is not a valid MCL command
	!	or a symbol name equated with command.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Check the spelling of the command name or symbol and
	!	re-enter the command.
	!	.lm -5
	!
	! Index:
	!
	!--
		ELSE
			CALL HELP_34MESSAGE(SCOPE, &
				"unrecognized qualifier " + COMMAND_IN, &
				"W", PROGRAMNAME, "", "IVQUAL")
	!++
	! Warning:IVQUAL
	!	^*Unrecognized Qualifier \*
	!	.b
	!	.lm +5
	!	^*Explanation:\*
	!	.b
	!	The qualifier is spelled incorrectly or is improperly
	!	placed in the command line.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Check validity, spelling, and placement and correct
	!	the command line.
	!	.lm -5
	!
	! Index:
	!
	!--

		END IF

		GOTO ExitFunction

	CASE ELSE

		EXIT_STATUS = CMC$_WARNING

		IF I = 1%
		THEN
			CALL HELP_34MESSAGE(SCOPE, "ambiguous command " + &
				"verb " + COMMAND_IN, &
				"W", PROGRAMNAME, "", "ABVERB")
	!++
	! Warning:ABVERB
	!	^*Ambiguous Command Verb\*
	!	.b
	!	.lm +5
	!	^*Explanation:\*
	!	.b
	!	Too few characters were used to truncate a command name to
	!	make the command name unique.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Re-enter the command. Specify at least four characters
	!	of the command name.
	!	.lm -5
	!
	! Index:
	!
	!--

		ELSE
			CALL HELP_34MESSAGE(SCOPE, "ambiguous qualifier " + &
				" or keyword " + COMMAND_IN, &
				"W", PROGRAMNAME, "", "ABKEYW")

	!++
	! Warning:ABKEYW
	!	^*Ambiguous Qualifier or Keyword\*
	!	.b
	!	.lm +5
	!	^*Explanation:\*
	!	.b
	!	Too few characters were used to truncate a qualifier or
	!	keyword name to make the qualifier or keyword name unique.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Re-enter the command. Specify at least four characters of the
	!	qualifier or keyword name.
	!	.lm -5
	!
	! Index:
	!
	!--

		END IF

		GOTO ExitFunction
	END SELECT

	SELECT COMMAND_OUT

	CASE "AMORTIZATION"
		COMMAND_OUT = "CMC:UT_SPEC_AMORTIZATION"

	CASE "BATCH"
		COMMAND_OUT = "CMC:UTL_MAST_BATCH_CONTROL"

	CASE "BYE"
		COMMAND_OUT = "LOGOUT"

	CASE "CARRIER"
		COMMAND_OUT = "CMC:UT_MAST_CARRIER"

	CASE "CARRIER/LIST"
		COMMAND_OUT = "CMC:UTL_REPORT/CUT090"

	CASE "COMPRESS"
		COMMAND_OUT = "CMC:UTL_SPEC_COMPRESS"

	CASE "COUNTRY"
		COMMAND_OUT = "CMC:UTL_MAST_COUNTRY"

	CASE "COUNTRY/LIST"
		COMMAND_OUT = "CMC:UTL_REPORT/CUT008"

	CASE "DEVICE"
		COMMAND_OUT = "CMC:UTL_MAST_DEVICE"

	CASE "DEVICE/LIST"
		COMMAND_OUT = "CMC:UTL_REPORT/CUT011"

	CASE "END"
		COMMAND_OUT = "EXIT"

	CASE "MACRO/LIST"
		COMMAND_OUT = "CMC:UTL_REPORT/CUT021"

	CASE "PKFORM"
		COMMAND_OUT = "CMC:UTL_MAST_PACKFORM"

	CASE "PKFORM/LIST"
		COMMAND_OUT = "CMC:UTL_REPORT/CUT012"

	CASE "PKMATERIAL"
		COMMAND_OUT = "CMC:UTL_MAST_PACKMAT"

	CASE "PKMATERIAL/LIST"
		COMMAND_OUT = "CMC:UTL_REPORT/CUT013"

	CASE "PROFILE"
		COMMAND_OUT = "CMC:UTL_MAST_PROFILE"

	CASE "PROFILE/STRUCTURE"
		COMMAND_OUT = "CMC:UTL_REPORT/CUT007"

	CASE "PROFILE/PERIOD"
		COMMAND_OUT = "CMC:UTL_REPORT/CUT006"

	CASE "QUIT"
		COMMAND_OUT = "DCL"

	CASE "REPORT"
		COMMAND_OUT = "CMC:UTL_BATCH_REPORT"

	CASE "REPORT/DESTINATION"
		COMMAND_OUT = "CMC:UTL_MAST_DOC_DEST"

	CASE "REPORT/LIST"
		COMMAND_OUT = "CMC:UTL_REPORT/CUT002"

	CASE "REPORT/SETTINGS"
		COMMAND_OUT = "CMC:UTL_PRINT"

	CASE "STRING"
		COMMAND_OUT = "CMC:UTL_MAST_STRING_PRINT"

	CASE "STRING/PRINT"
		COMMAND_OUT = "CMC:UTL_SPEC_STRING_PRINT"

	CASE "STRING/LIST"
		COMMAND_OUT = "CMC:UTL_REPORT/CUT018"

	CASE "SYSTEM/INSTALL"
		COMMAND_OUT = "CMC:UT_SPEC_MENUMAINT"

	CASE "SYSTEM/LIST"
		COMMAND_OUT = "CMC:UTL_REPORT/CUT001"

	CASE "TERMS"
		COMMAND_OUT = "CMC:UT_MAST_TERMS"

	CASE "TERMS/LIST"
		COMMAND_OUT = "CMC:UTL_REPORT/CUT091"

	CASE "TIMEOUT"
		COMMAND_OUT = "CMC:UTL_MAST_TIMEOUT"

	CASE "UOM"
		COMMAND_OUT = "CMC:UTL_MAST_MEASURE"

	CASE "UOM/LIST"
		COMMAND_OUT = "CMC:UTL_REPORT/CUT014"

	CASE "DCL"
		SPC = INSTR(1%, COMMAND_IN, " ")
		IF TRM$(RIGHT(COMMAND_IN, SPC + 1%)) <> ""
		THEN
			COMMAND_OUT = RIGHT(COMMAND_IN, SPC + 1%)
		END IF

	CASE "SYSTEM", "CALENDAR", "LOGOUT", "EXIT", &
		"BATCH/MONITOR", "HELP", "MACRO", "MACRO/DELETE"
		!
		! More good commands
		!

	CASE ELSE
		!
		! test for macro
		!
		IF MACROTYPE > CMCMACRO AND I = 1%
		THEN
			SPC = INSTR(1%, COMMAND(MACROTYPE), " ")
			COMMAND_OUT = RIGHT(COMMAND(MACROTYPE), SPC + 1%)
			IF EDIT$(COMMAND_OUT, -1%) = ""
			THEN
				EXIT_STATUS = CMC$_UNTERROR

				CALL HELP_34MESSAGE(SCOPE, &
					"missing parameter in " + COMMAND_IN, &
					"F", PROGRAMNAME, "", "MISSPAR")
			END IF
	!++
	! FatalError:MISSPAR
	!
	!	^*Missing Parameter\*
	!	.b
	!	^*Explanation:\*
	!	.p
	!	The MCL command doesn't have a parameter.
	!	.b
	!	^*User Action:\*
	!	.p
	!	Add the missing parameter. Refer to the ^~ CMC
	!	Software User's Guide and Reference Manual \~ .
	!--
			EXIT_STATUS = CMC$_MACRO

		ELSE
			!
			! Bad combination
			!

			EXIT_STATUS = CMC$_UNTERROR

			CALL HELP_34MESSAGE(SCOPE, &
				"conflicting qualifiers " + COMMAND_IN, &
				"F", PROGRAMNAME, "", "CONFQUAL")

	!++
	! FatalError:CONFQUAL
	!
	!	^*Conflicting Qualifiers\*
	!	.b
	!	^*Explanation:\*
	!	.p
	!	Two or more conflicting qualifiers were specified.
	!	.b
	!	^*User Action:\*
	!	.p
	!	Refer to the ^~ CMC Software User's Guide and
	!	Reference Manual \~ to determine the qualifiers
	!	in conflict and reenter the command.
	!--
		END IF

	END SELECT

 ExitFunction:
	FUNC_MCLCOMMAND = EXIT_STATUS

32767	END FUNCTION
	!+-+-+
	!++
	! Fatal Error:CONFQUAL
	!	^*Conflicting Qualifiers\*
	!	.p
	!	^*Explanation:\*
	!	.p
	!	Two or more conflicting qualifiers were specified.
	!	.p
	!	^*User Action:\*
	!	.p
	!	Refer to the ^~ CMC Software User's Guide and
	!	Reference Manual \~ to determine the qualifiers
	!	in conflict and reenter the command.
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Fatal Error:CONFQUAL
	!	^*Conflicting Qualifiers\*
	!	.p
	!	^*Explanation:\*
	!	.p
	!	Two or more conflicting qualifiers were specified.
	!	.p
	!	^*User Action:\*
	!	.p
	!	Refer to the ^~CMC Software User's Guide and
	!	Reference Manual\~ to determine the qualifiers
	!	in conflict and reenter the command.
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Fatal Error:MISSPAR
	!	^*Missing Parameter\*
	!	.p
	!	^*Explanation:\*
	!	.p
	!	The MCL command doesn't have a parameter.
	!	.p
	!	^*User Action:\*
	!	.p
	!	Add the missing parameter. Refer to the ^~CMC
	!	Software User's Guide and Reference Manual\~ .
	!
	! Index:
	!
	!--
