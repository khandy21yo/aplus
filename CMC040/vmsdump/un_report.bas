1	%TITLE "User Report Settings"

	! Compile:
	!
	!	$ BAS UN_REPORT/LINE
	!	$ LINK/EXE=UTL_EXE: UN_REPORT
	!	$ DELETE UN_REPORT.OBJ;*
	!
	! AUTHOR:
	!
	!	0229/2018 - Kevin Handy
	!
	!--

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT) UTL_REPORT_CDD UTL_REPORT

	%PAGE


300	WHEN ERROR IN
		UTL_REPORT.CH% = 10%
		UTL_REPORT.NAME$ = "UTL_REPORT.IDX"

		OPEN UTL_REPORT.NAME$ FOR INPUT AS FILE UTL_REPORT.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP UTL_REPORT, &
			PRIMARY KEY &
				UTL_REPORT::REPNUM, &
			ALTERNATE KEY &
			( &
				UTL_REPORT::SYSTEM, &
				UTL_REPORT::REPNUM &
			)	DUPLICATES CHANGES, &
			ALTERNATE KEY &
			( &
				UTL_REPORT::SUBSYS, &
				UTL_REPORT::REPNUM &
			)	DUPLICATES CHANGES, &
			ACCESS READ, ALLOW MODIFY
	USE
		PRINT "Cannot open REPORT file"
		STOP
	END WHEN


	RESET #UTL_REPORT.CH%

loop:
	WHEN ERROR IN
		GET #UTL_REPORT.CH%
	USE
		CONTINUE endloop
	END WHEN

	PRINT 	"('"; TRM$(UTL_REPORT::SYSTEM); "','"; &
		TRM$(UTL_REPORT::SUBSYS); "','";  &
		TRM$(UTL_REPORT::REPNUM); "','";  &
                TRM$(UTL_REPORT::REPDES); "','";  &
                TRM$(UTL_REPORT::PRODEV); "','";  &
                TRM$(UTL_REPORT::PRONAM); "'," 
	PRINT	"'"; UTL_REPORT::CANSPOOL; "','";  &
                UTL_REPORT::CANDISP; "','";  &
                UTL_REPORT::CANDEV; "','";  &
                UTL_REPORT::CANFILE; "','";  &
                UTL_REPORT::CANDET; "','";  &
                UTL_REPORT::REPYN; "',"
        PRINT   "'"; TRM$(UTL_REPORT::DESCR(I%)); "'," FOR I% = 0% TO 9%
	PRINT	"'"; TRM$(UTL_REPORT::OPTTYPE(I%)); "'," FOR I% = 0% TO 9%
	PRINT   UTL_REPORT::OPTLEN(I%); "," FOR I% = 0% TO 9%
	PRINT	"'"; TRM$(UTL_REPORT::VALID(I%)); "'," FOR I% = 0% TO 9%
	PRINT   "'"; UTL_REPORT::REQUIRE(I%); "'," FOR I% = 0% TO 9%
	PRINT	"'"; TRM$(UTL_REPORT::SPOOL); "',"
	PRINT   "'"; TRM$(UTL_REPORT::OPTDEF(I%)); "'," FOR I% = 0% TO 9%
	PRINT   "'"; TRM$(UTL_REPORT::DEFOUT); "',"
	PRINT   "'"; TRM$(UTL_REPORT::ITEMGROUP(I%)); "'," FOR I% = 0% TO 9%
	PRINT   "'"; TRM$(UTL_REPORT::ITEM(I%)); "'," FOR I% = 0% TO 9%
	PRINT   "'"; TRM$(UTL_REPORT::CHAINTO); "','"; &
		TRM$(UTL_REPORT::PRINTTYPE); "','"; &
		UTL_REPORT::LASTRUNDATE; "','"; &
		UTL_REPORT::LASTRUNTIME; "','"; &
		UTL_REPORT::BASERUNDATE; "','"; &
		UTL_REPORT::RUNFREQ; "',"; &
		UTL_REPORT::REPWID; ",'"; &
		TRM$(UTL_REPORT::SPOOLFORM); "'),";

	GOTO loop

endloop:
	CLOSE UTL_REPORT.CH%

32767	END
