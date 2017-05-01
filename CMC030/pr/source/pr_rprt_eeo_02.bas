1	%TITLE "Employee EEO Report"
	%SBTTL "PR_RPRT_EEO_02"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1996 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:PR082
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Employee EEO Report\* is a list of all the employees
	!	within specified parameters.
	!	.lm -5
	!
	! Index:
	!
	!	.x Report>EEO
	!	.x EEO>Report
	!
	! Option:
	!
	! Author:
	!
	!	08/26/96 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_EEO_02
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_EEO_02, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_EEO_02.OBJ;*
	!
	! Modification history:
	!
	!	09/10/96 - Kevin Handy
	!		Clean up error trapping
	!
	!	09/12/96 - Kevin Handy
	!		Hook in Skills table.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_SKILLS.HB"
	MAP (PR_SKILLS) PR_SKILLS_CDD PR_SKILLS

	RECORD PR_TEMP_CDD
		STRING	EEOSORT = 2%
		STRING	SKILL = 6%
		STRING	SEX = 1%
		STRING	RACE = 1%
		STRING	DISABLED = 1%
		STRING	EMPNUM = 10%
		STRING	EMPNAME = 30%
	END RECORD

	MAP (PR_TEMP) PR_TEMP_CDD PR_TEMP

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	!
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.p
	!	The ^*From Item\* entered in this setting will cause the printing
	!	to begin with a particular item.
	!	.p
	!	A blank field will cause it to start with the first item in the file.
	!
	! Index:
	!	.x From Item>EEO Report
	!	.x EEO Report>From Item
	!
	! Datatype:TEXT
	! Size:20
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	!
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.p
	!	The ^*To Item\* entered in this setting will cause the printing
	!	to end with a particular item in the file.
	!	.p
	!	A blank field causes the report to end with the last item in the file.
	!
	! Index:
	!	.x To Item>EEO Report
	!	.x EEO Report>To Item
	!
	! Datatype:TEXT
	! Size:20
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)
	!++
	!
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.p
	!	The ^*Wildcard\* setting enables the user to print a report including selected
	!	employees only using the wildcarding techniques.
	!	.p
	!	Valid wildcard characters are an asterisk (*) or a question mark (?). An
	!	asterisk (*) indicates all employees will be selected. A question mark (?) in
	!	a field position indicates an employee  with any character in that
	!	equivalent position will be selected.
	!
	! Index:
	!	.x Wildcard>EEO Report
	!	.x EEO Report>Wildcard
	!
	! Datatype:TEXT
	! Size:20
	!--

	PRDATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(4%))
	!++
	!
	! Abstract:FLD05
	!	^*(05) EEO Date\*
	!	.p
	!
	! Index:
	!	.x EEO Date>EEO Report
	!	.x EEO Report>EEO Date
	!
	! Datatype:TEXT
	! Size:20
	!--

	PAGE_FLAG$ = LEFT(UTL_REPORTX::OPTDEF(5%), 1%)
	!++
	!
	! Abstract:FLD06
	!	^*(04) Page After Section\*
	!	.b
	!	This field allows page breaks to occur after the department/location
	!	changes.
	!
	! Index:
	!
	!--

	PRINT_INFO$ = LEFT(UTL_REPORTX::OPTDEF(6%), 2%)
	!++
	!
	! Abstract:FLD07
	!	^*(06) Print Information\*
	!	.b
	!	This field allows selecting what type of information will be printed on the
	!	report. Valid options are:
	!	.b
	!	*T*D Termination Date
	!	.br
	!	*W*D Weekly Dollars
	!
	! Index:
	!
	!--

300	!
	! Open employee master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_SKILLS.OPN"
	USE
		FILENAME$ = "PR_SKILLS"
		CONTINUE HelpError
	END WHEN

	%PAGE

500	!*******************************************************************
	! Generate work file
	!*******************************************************************

	CALL ASSG_CHANNEL(PR_TEMP.CH%, STATUS%)
	WHEN ERROR IN
		OPEN "PR_TEMP.TMP" FOR OUTPUT AS FILE PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			TEMPORARY, &
			MAP PR_TEMP, &
			PRIMARY KEY (PR_TEMP::EEOSORT, PR_TEMP::SKILL, PR_TEMP::SEX, &
				PR_TEMP::RACE, PR_TEMP::EMPNAME) DUPLICATES, &
			ACCESS MODIFY, &
			ALLOW NONE
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

510	WHEN ERROR IN
		RESET #PR_EMP_MASTER.CH%
	USE
		CONTINUE 590
	END WHEN

520	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%
	USE
		CONTINUE 590 IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	!
	! From/to
	!
	GOTO 520 IF (PR_EMP_MASTER::EMP_SKILL > TO_ITEM$) AND &
		TO_ITEM$ <> ""
	GOTO 520 IF (PR_EMP_MASTER::EMP_SKILL < FROM_ITEM$)

	!
	! Must be active in the right date
	!
	GOTO 520 IF (PR_EMP_MASTER::HIREDAY > PRDATE$) OR &
		(PR_EMP_MASTER::TERMDAY > "00000000")

	!
	! Wildcard
	!
	IF (WLDCRD$ <> "")
	THEN
		GOTO 520 IF COMP_STRING(PR_EMP_MASTER::EMP_SKILL, WLDCRD$) = 0%
	END IF

530	IF PR_SKILLS::SKILL <> PR_EMP_MASTER::EMP_SKILL
	THEN
		PR_SKILLS::SKILL = PR_EMP_MASTER::EMP_SKILL
		PR_SKILLS::DESCRIPTION = ""
		PR_SKILLS::EEOSORT = ""

		WHEN ERROR IN
			GET #PR_SKILLS.CH%, &
				KEY #0% EQ PR_EMP_MASTER::EMP_SKILL, &
				REGARDLESS
		USE
			CONTINUE 550
		END WHEN
	END IF

550	PR_TEMP::EEOSORT = PR_SKILLS::EEOSORT
	PR_TEMP::SKILL = PR_EMP_MASTER::EMP_SKILL
	PR_TEMP::SEX = PR_EMP_MASTER::SEX
	PR_TEMP::RACE = PR_EMP_MASTER::RACE
	PR_TEMP::DISABLED = PR_EMP_MASTER::DISABLED
	PR_TEMP::EMPNUM = PR_EMP_MASTER::EMPNUM
	PR_TEMP::EMPNAME = PR_EMP_MASTER::EMPNAME

	PUT #PR_TEMP.CH%

	GOTO 520

590	!

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "EEO Report"
	TITLE$(2%) = "."
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = "EmployeNum EmployeeName                   " + &
		"SocSecNo    HireDate  Rate WeeklyRate"
	TITLE$(5%) = "."

	!
	! Layout for printed lines
	!
	LYT_LINE$ = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #PR_TEMP.CH%
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	CURRENT_SKILL$ = "~~~~~~~~~~~"
	CURRENT_SEX$ = "~~"
	CURRENT_RACE$ = "~~"

	SKILL_COUNT% = 0%
	SEX_COUNT% = 0%
	RACE_COUNT% = 0%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Did we change skills?
	!
	IF CURRENT_SKILL$ <> PR_TEMP::SKILL
	THEN
		GOSUB RaceTotal
		GOSUB SexTotal
		GOSUB SkillTotal

		!
		! Force a page break?
		!
		IF PAGE_FLAG$ = "Y"
		THEN
			CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, &
				TITLE$(), "", 3000%)
		END IF
	END IF

	IF CURRENT_SEX$ <> PR_TEMP::SEX
	THEN
		GOSUB RaceTotal
		GOSUB SexTotal
	END IF

	IF CURRENT_RACE$ <> PR_TEMP::RACE
	THEN
		GOSUB RaceTotal
	END IF

17030	!
	! Get skill description
	!
	IF PR_SKILLS::SKILL <> PR_TEMP::SKILL
	THEN
		PR_SKILLS::SKILL = PR_TEMP::SKILL
		PR_SKILLS::DESCRIPTION = ""
		PR_SKILLS::EEOSORT = ""

		WHEN ERROR IN
			GET #PR_SKILLS.CH%, &
				KEY #0% EQ PR_TEMP::SKILL, &
				REGARDLESS
		USE
			CONTINUE 17050
		END WHEN
	END IF

17050	!
	! Print any necessary titles
	!
	IF PRINT_INFO$ = "Y"
	THEN
		IF SKILL_COUNT% = 0%
		THEN
			TEXT$ = "Skill: " + &
				PR_TEMP::SKILL + " " + &
				PR_SKILLS::DESCRIPTION

			CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, &
				TITLE$(), TEXT$, 5%)
		END IF

		IF SEX_COUNT% = 0%
		THEN
			SELECT PR_TEMP::SEX
			CASE "M", "m"
				TEXT1$ = "Male"
			CASE "F", "f"
				TEXT1$ = "Female"
			CASE ELSE
				TEXT1$ = "?Undefined?"
			END SELECT

			TEXT$ = "  Sex: " + &
				PR_TEMP::SEX + " " + TEXT1$

			CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, &
				TITLE$(), TEXT$, 4%)
		END IF

		IF RACE_COUNT% = 0%
		THEN
			SELECT PR_TEMP::RACE
			CASE "W", "w"
				TEXT1$ = "White"
			CASE "B", "b"
				TEXT1$ = "Black"
			CASE "H", "h"
				TEXT1$ = "Hispanic"
			CASE "A", "a"
				TEXT1$ = "Asian"
			CASE "N", "n", "I", "i"
				TEXT1$ = "Native American"
			CASE "O", "o"
				TEXT1$ = "Oriental"
			CASE "X", "x"
				TEXT1$ = "Other"
			CASE ELSE
				TEXT1$ = "?Undefined?"
			END SELECT

			TEXT$ = "    Race: " + &
				PR_TEMP::RACE + " " + TEXT1$

			CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, &
				TITLE$(), TEXT$, 3%)
		END IF
	END IF

	!
	! Print out this employee
	!
	IF PRINT_INFO$ = "Y"
	THEN
		TEXT$ = "      " + PR_TEMP::EMPNUM + " " + &
			PR_TEMP::EMPNAME

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	SKILL_COUNT% = SKILL_COUNT% + 1%
	SEX_COUNT% = SEX_COUNT% + 1%
	RACE_COUNT% = RACE_COUNT% + 1%

17350	GOTO  GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB RaceTotal
	GOSUB SexTotal
	GOSUB SkillTotal

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

	!*******************************************************************
	! Total for race
	!*******************************************************************

 RaceTotal:

	IF RACE_COUNT% <> 0%
	THEN
		TEXT$ = "    RACE TOTAL " + &
			" Skill: " + CURRENT_SKILL$ + &
			" Sex: " + CURRENT_SEX$ + &
			" Race: " + CURRENT_RACE$ + &
			" Total: " + NUM1$(RACE_COUNT%)
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, -1%)
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", -2%)
	END IF

	CURRENT_RACE$ = PR_TEMP::RACE
	RACE_COUNT% = 0%

	RETURN

	%PAGE

	!*******************************************************************
	! Total for Sex
	!*******************************************************************

 SexTotal:

	IF SEX_COUNT% <> 0%
	THEN
		TEXT$ = "  SEX TOTAL    " + &
			" Skill: " + CURRENT_SKILL$ + &
			" Sex: " + CURRENT_SEX$ + &
			"        " + &
			" Total: " + NUM1$(SEX_COUNT%)

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, -1%)
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", -2%)
	END IF

	CURRENT_SEX$ = PR_TEMP::SEX
	SEX_COUNT% = 0%

	RETURN

	%PAGE

	!*******************************************************************
	! Total for Skill
	!*******************************************************************

 SkillTotal:

	IF SKILL_COUNT% <> 0%
	THEN
		TEXT$ = "SKILL TOTAL    " + &
			" Skill: " + CURRENT_SKILL$ + &
			"       " + &
			"        " + &
			" Total: " + NUM1$(SKILL_COUNT%)

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, -1%)
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", -2%)
	END IF

	CURRENT_SKILL$ = PR_TEMP::SKILL
	SKILL_COUNT% = 0%

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
