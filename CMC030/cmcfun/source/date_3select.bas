1	%TITLE "Select a Date from a Calendar"
	%SBTTL "DATE_3SELECT"
	%IDENT "V3.6a Calico"

	FUNCTION STRING DATE_3SELECT(SCOPE_STRUCT SCOPE, &
		STRING STARTING_DATE)

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
	! Abstract:COMMAND
	!	^*Select a Date from a Calendar\*
	!	^*CALENDAR\*
	!	.b
	!	.lm +5
	!	^*Calendar\* displays a calendar by month from the selected starting date.
	!	.b
	!	^*Format: CALENDAR [date]\*
	!	.b
	!	^*Example:\*
	!	.table 3,25
	!	.te
	!	Menu Command Level> /CALENDAR 19900809
	!	.te
	!	Menu Command Level> /CALENDAR
	!	.end table
	!	.lm -10
	!
	! Index:
	!	.x Calendar>Select
	!	.x Select>Calendar
	!	.x Date>Select
	!	.x Select>Date
	!
	! Parameters:
	!
	!	SCOPE
	!		Structure created by window initilization.
	!
	!	STARTING_DATE
	!		The passed date at which to position into the calendar.
	!		Uses current date if this is blank.
	!		Must be in YYYYMMDD format.
	!
	!	The function returns the selected date in YYYYMMDD format.
	!
	! Example:
	!
	!	DATE_3SELECT("19880214")
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:DATE_3SELECT
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP DATE_3SELECT
	!	$ DELETE DATE_3SELECT.OBJ;*
	!
	! Author:
	!
	!	05/24/88 - Kevin Handy
	!
	! Modification history:
	!
	!	12/22/89 - Kevin Handy
	!		Made sharable version.
	!
	!	03/15/90 - Frank F. Starman
	!		Change key for help message
	!
	!	05/21/90 - Frank F. Starman
	!		Added COMMAND help message.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/09/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! Save original help items
	!
	TEMP_IDENT$ = SCOPE::PRG_IDENT + ""
	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM + ""
	TEMP_ITEM$ = SCOPE::PRG_ITEM + ""

	SCOPE::PRG_IDENT = "H"
	SCOPE::PRG_PROGRAM = "DATE_3SELECT"
	SCOPE::PRG_ITEM = "HELP"

	!
	! Use either the date passes in, or the current date
	! if that is blank.
	!
	IF STARTING_DATE = ""
	THEN
		THIS_DATE$ = DATE_TODAY
	ELSE
		THIS_DATE$ = EDIT$(STARTING_DATE, -1%)
	END IF

	!
	! Create a window to use to display this calendar
	!
	ST% = SMG$CREATE_VIRTUAL_DISPLAY(9%, 29%, SMG_CALENDAR%, SMG$M_BORDER, &
		SMG$M_REVERSE)

	ST% = SMG$PUT_CHARS(SMG_CALENDAR%, "Sun Mon Tue Wed Thr Fri Sat", &
		3%, 2%)

	ST% = SMG$BEGIN_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	ST% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_CALENDAR%, SCOPE::SMG_PBID, 4%, 25%)

	GOTO 1010

	%PAGE

1000	!*******************************************************************
	! Load in current month's calendar
	!*******************************************************************

	ST% = SMG$BEGIN_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

1010	!
	! Pull off the initial month, day, and year
	!
	THIS_YEAR% = VAL%(LEFT(THIS_DATE$, 4%))
	THIS_MONTH% = VAL%(MID(THIS_DATE$, 5%, 2%))
	THIS_DAY% = VAL%(MID(THIS_DATE$, 7%, 2%))

	!
	! Erase current date information
	!
	ST% = SMG$ERASE_DISPLAY(SMG_CALENDAR%, 4%, 1%)

	!
	! Calculate number of days in this month
	!
	IF THIS_MONTH% = 12%
	THEN
		NEXT_MONTH% = 1%
		NEXT_YEAR% = THIS_YEAR% + 1%
	ELSE
		NEXT_MONTH% = THIS_MONTH% + 1%
		NEXT_YEAR% = THIS_YEAR%
	END IF

	TOTAL_DAYS% = &
		DATE_DAYCODE(FORMAT$(NEXT_YEAR%, "<0>###") + &
			FORMAT$(NEXT_MONTH%, "<0>#") + &
			"01") &
		- &
		DATE_DAYCODE(FORMAT$(THIS_YEAR%, "<0>###") + &
			FORMAT$(THIS_MONTH%, "<0>#") + &
			"01")

	!
	! Find the day of week this month starts on
	!
	STARTING_DATE% = DATE_DAYOFWEEK( &
		DATE_DAYCODE(FORMAT$(THIS_YEAR%, "<0>###") + &
		FORMAT$(THIS_MONTH%, "<0>#") + &
		"01"))
	STARTING_DATE% = 0% IF STARTING_DATE% = 7%

	!
	! Place title of month across the top
	!
	THIS_MONTH$ = PRNT_MONTHYYYY(FORMAT$(THIS_YEAR%, "<0>###") + &
		FORMAT$(THIS_MONTH%, "<0>#") + &
		"01")

	THIS_MONTH$ = SPACE$((29% - LEN(THIS_MONTH$)) / 2%) + THIS_MONTH$

	ST% = SMG$ERASE_LINE(SMG_CALENDAR%, 1%, 1%)
	ST% = SMG$PUT_CHARS(SMG_CALENDAR%, THIS_MONTH$, 1%, 1%)

	!
	! Place all days on the screen
	!
	FOR LOOP% = 1% TO TOTAL_DAYS%
		THIS_LINE% = (LOOP% + STARTING_DATE% - 1%) / 7%
		THIS_COL% = 3% + (LOOP% + STARTING_DATE% - 1% - &
			THIS_LINE% * 7%) * 4%

		ST% = SMG$PUT_CHARS(SMG_CALENDAR%, FORMAT$(LOOP%, "##"), &
			THIS_LINE% + 4%, THIS_COL%)

	NEXT LOOP%

	!
	! Now let user see the changes
	!
	ST% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	%PAGE

2000	!*******************************************************************
	! Main loop inside of a month
	!*******************************************************************

	!
	! Force this date to be legal
	!
	THIS_DAY% = TOTAL_DAYS% IF THIS_DAY% > TOTAL_DAYS%

	!
	! Highlight current date
	!
	THIS_LINE% = (THIS_DAY% + STARTING_DATE% - 1%) / 7%
	THIS_COL% = 3% + (THIS_DAY% + STARTING_DATE% - 1% - &
		THIS_LINE% * 7%) * 4%

	ST% = SMG$PUT_CHARS(SMG_CALENDAR%, FORMAT$(THIS_DAY%, "##"), &
		THIS_LINE% + 4%, THIS_COL%, , , SMG$M_REVERSE)

	!
	! Enter a character from the user
	!
	CALL ENTR_3MESSAGENEWWINDOW(SCOPE, &
		"Press <Exit> to exit, <Help> for help.", 0%)

	!
	! Un-highlight current date
	!
	ST% = SMG$PUT_CHARS(SMG_CALENDAR%, FORMAT$(THIS_DAY%, "##"), &
		THIS_LINE% + 4%, THIS_COL%)

	!
	! Handle the possibilities
	!
	SELECT SCOPE::SCOPE_EXIT

	!
	! Select this date
	!
	CASE SMG$K_TRM_SELECT
		GOTO 8000

	!
	! Uparrow
	!
	CASE SMG$K_TRM_UP
		OFFSET% = -7%
		GOTO OffsetDate

	!
	! Down-arrow
	!
	CASE SMG$K_TRM_DOWN
		OFFSET% = 7%
		GOTO OffsetDate

	!
	! left arrow
	!
	CASE SMG$K_TRM_LEFT
		OFFSET% = -1%
		GOTO OffsetDate

	!
	! Right arrow
	!
	CASE SMG$K_TRM_RIGHT
		OFFSET% = 1%
		GOTO OffsetDate

	!
	! Prev screen
	!
	CASE SMG$K_TRM_PREV_SCREEN
		OFFSET% = -1%
		GOTO OffsetMonth

	!
	! Next screen
	!
	CASE SMG$K_TRM_NEXT_SCREEN
		OFFSET% = 1%
		GOTO OffsetMonth

	!
	! Exit
	!
	CASE SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLC, &
		SMG$K_TRM_CTRLZ

		DATE_3SELECT = STARTING_DATE
		GOTO ExitFunction

	!
	! Unused keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)

	END SELECT

	GOTO 2000

	%PAGE

5000	!*******************************************************************
	! Subroutine to move around by a specified number of days
	!*******************************************************************

 OffsetDate:
	THIS_AGE% = DATE_DAYCODE(FORMAT$(THIS_YEAR%, "<0>###") + &
		FORMAT$(THIS_MONTH%, "<0>#") + &
		FORMAT$(THIS_DAY%, "<0>#"))
	NEW_DATE$ = DATE_INVDCODE(THIS_AGE% + OFFSET%)

	IF LEFT(NEW_DATE$, 6%) <> LEFT(THIS_DATE$, 8%)
	THEN
		THIS_DATE$ = NEW_DATE$
		GOTO 1000
	ELSE
		THIS_DAY% = VAL%(MID(THIS_DATE$, 7%, 2%))
		GOTO 2000
	END IF

5100	!*******************************************************************
	! Handle offset by a number of months (Max 12)
	!*******************************************************************

 OffsetMonth:

	THIS_MONTH% = THIS_MONTH% + OFFSET%

	IF THIS_MONTH% > 12%
	THEN
		THIS_MONTH% = THIS_MONTH% - 12%
		THIS_YEAR% = THIS_YEAR% + 1%
	END IF

	IF THIS_MONTH% < 1%
	THEN
		THIS_MONTH% = THIS_MONTH% + 12%
		THIS_YEAR% = THIS_YEAR% - 1%
	END IF

	THIS_DATE$ = FORMAT$(THIS_YEAR%, "<0>###") + &
		FORMAT$(THIS_MONTH%, "<0>#") + &
		FORMAT$(THIS_DAY%, "<0>#")

	GOTO 1000

	%PAGE

8000	!*******************************************************************
	! Return back the date
	!*******************************************************************

	DATE_3SELECT = FORMAT$(THIS_YEAR%, "<0>###") + &
		FORMAT$(THIS_MONTH%, "<0>#") + &
		FORMAT$(THIS_DAY%, "<0>#")

 ExitFunction:
	ST% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_CALENDAR%)

	!
	! Restore original help items
	!
	SCOPE::PRG_IDENT = TEMP_IDENT$
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$
	SCOPE::PRG_ITEM = TEMP_ITEM$

	END FUNCTION
