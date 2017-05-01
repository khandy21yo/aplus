1	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	MAP (PR_TRN_PAY) PR_TRN_PAY_CDD PR_TRN_PAY

	MAP (PR_EMP_MASTER) PR_EMP_MASTER_CDD PR_EMP_MASTER

100	!
	!======================================================================
	! PR_EMP_MASTER file (open read only)
	!======================================================================

	PR_EMP_MASTER.CH% = 5%
	PR_EMP_MASTER.DEV$ = "$DISK2:[USER]"

	PR_EMP_MASTER.NAME$ = PR_EMP_MASTER.DEV$+"PR_EMP_MASTER.MAS"

	OPEN PR_EMP_MASTER.NAME$ FOR INPUT AS FILE PR_EMP_MASTER.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_EMP_MASTER, &
		PRIMARY KEY &
			PR_EMP_MASTER::EMPNUM, &
 !		ALTERNATE KEY &
 !			PR_EMP_MASTER::EMPNAME &
 !			DUPLICATES CHANGES, &
 !		ALTERNATE KEY &
 !			PR_EMP_MASTER::SORT &
 !			DUPLICATES CHANGES, &
 !		ALTERNATE KEY &
 !			PR_EMP_MASTER::SSN &
 !			DUPLICATES CHANGES, &
 !		ALTERNATE KEY &
 !		( &
 !			PR_EMP_MASTER::LOCATION, &
 !			PR_EMP_MASTER::DEPT, &
 !			PR_EMP_MASTER::WORK_CENTER, &
 !			PR_EMP_MASTER::SORT &
 !		)	DUPLICATES CHANGES, &
		ACCESS READ, ALLOW MODIFY


110	!
	!======================================================================
	! PR_TRN_PAY file (open read only)
	!======================================================================

	PR_TRN_PAY_OLD.CH% = 6%
	PR_TRN_PAY.DEV$ = "$DISK2:[USER]"
	BATCH_NO$ = "19891014"

	PR_TRN_PAY.NAME$ = PR_TRN_PAY.DEV$+"PR_TRN_PAY_"+BATCH_NO$+".JRL_OLD"

	OPEN PR_TRN_PAY.NAME$ FOR INPUT AS FILE PR_TRN_PAY_OLD.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_TRN_PAY, &
		PRIMARY KEY &
		( &
			PR_TRN_PAY::EMPNUM, &
			PR_TRN_PAY::PR_END_DATE &
		)	DUPLICATES, &
		ALTERNATE KEY &
		( &
			PR_TRN_PAY::SUBACC, &
			PR_TRN_PAY::OPER, &
			PR_TRN_PAY::ACCT &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			PR_TRN_PAY::LOCATION, &
			PR_TRN_PAY::DEPT, &
			PR_TRN_PAY::WORK_CENTER, &
			PR_TRN_PAY::EMPNUM, &
			PR_TRN_PAY::PR_END_DATE &
		)	DUPLICATES CHANGES, &
		ACCESS READ, ALLOW MODIFY


120	!
	!======================================================================
	! PR_TRN_PAY file (create, open read/write)
	!======================================================================

	PR_TRN_PAY.CH% = 8%

	PR_TRN_PAY.NAME$ = PR_TRN_PAY.DEV$+"PR_TRN_PAY_"+BATCH_NO$+".JRL"

	OPEN PR_TRN_PAY.NAME$ AS FILE PR_TRN_PAY.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PR_TRN_PAY, &
		BUFFER 32%, &
		PRIMARY KEY &
		( &
			PR_TRN_PAY::EMPNUM, &
			PR_TRN_PAY::PR_END_DATE &
		)	DUPLICATES, &
		ALTERNATE KEY &
		( &
			PR_TRN_PAY::SUBACC, &
			PR_TRN_PAY::OPER, &
			PR_TRN_PAY::ACCT &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			PR_TRN_PAY::LOCATION, &
			PR_TRN_PAY::DEPT, &
			PR_TRN_PAY::WORK_CENTER, &
			PR_TRN_PAY::EMPNUM, &
			PR_TRN_PAY::PR_END_DATE &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY


1000	ON ERROR GOTO 19000

	RESET #PR_TRN_PAY_OLD.CH%

1100	GET #PR_TRN_PAY_OLD.CH%, REGARDLESS

1110	PUT #PR_TRN_PAY.CH%
	PRINT "G";
	PRINT IF CCPOS(0%) > 50%
	THIS_EMPNUM$ = PR_TRN_PAY::EMPNUM + ""

	GOTO 1100

1500	GET #PR_EMP_MASTER.CH%, KEY #0% GT THIS_EMPNUM$
	PRINT "X";THIS_EMPNUM$
	THIS_EMPNUM$ = PR_EMP_MASTER::EMPNUM

1510	GET #PR_TRN_PAY_OLD.CH%, KEY#0% GE THIS_EMPNUM$
	GOTO 1110

2000	CLOSE 5%, 6%, 8%

	GOTO 32767

19000	SELECT ERL
		CASE 1100%
			RESUME 2000 IF ERR = 11%
			RESUME 1500
		CASE 1500%
			RESUME 2000
		CASE 1510%
			RESUME 2000 IF ERR = 11%
			RESUME 1500
	END SELECT

	ON ERROR GOTO 0

32767	END
