1	!
	! Dump chart of accounts in to an ascii file
	!
	!++
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_ASCI_CLOSE/LINE
	!	$ LINK/EXEC:AR_EXE AR_ASCI_CLOSE,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_ASCI_CLOSE.OBJ;*
	!
	! History:
	!
	!	07/19/2001 - Kevin Handy
	!		Added compile statement
	!		Lose CDD requirement
	!--

	%include "source:[ar.open]ar_CLOSED.hb"
	map (ar_CLOSED) ar_CLOSED_cdd ar_CLOSED

	on error goto 19000

1000	%include "source:[ar.open]ar_CLOSED.opn"

1010	open "arCLOSED.txt" for output as file 1%, &
		recordsize 511%

2000	reset #ar_CLOSED.ch%

2100	when error in
		get #ar_CLOSED.ch%, regardless
	use
		continue 32767 if err = 11% and erl = 2100%
		exit handler
	end when

	text$ = edit$(AR_CLOSED::CUSNUM, 132%) + "	" + &
		edit$(AR_CLOSED::INVNUM, 132%) + "	" + &
		edit$(AR_CLOSED::TRATYP, 132%) + "	" + &
		edit$(AR_CLOSED::TRADAT, 132%) + "	" + &
		format$(AR_CLOSED::SALAMT, "######.##") + "	" + &
		format$(AR_CLOSED::DISAMT, "######.##") + "	" + &
		format$(AR_CLOSED::OTHCHG, "######.##") + "	" + &
		edit$(AR_CLOSED::RECNUM, 132%) + "	" + &
		edit$(AR_CLOSED::CHKNUM, 132%) + "	" + &
		edit$(AR_CLOSED::ARACCT, 132%) + "	" + &
		edit$(AR_CLOSED::SUBACC, 132%) + "	" + &
		edit$(AR_CLOSED::DESCR, 132%) + "	" + &
		edit$(AR_CLOSED::SALNUM, 132%) + "	" + &
		edit$(AR_CLOSED::BATCH, 132%) + "	" + &
		edit$(AR_CLOSED::UPDATED, 132%) + "	" + &
		edit$(AR_CLOSED::CLOSEDATE, 132%) + "	" + &
		edit$(AR_CLOSED::DUEDATE, 132%) + "	" + &
		edit$(AR_CLOSED::DISCOUNTDATE, 132%)

	print #1%, text$

	goto 2100

19000	on error goto 0

32767	end
