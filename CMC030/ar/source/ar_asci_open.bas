1	!
	! Dump chart of accounts in to an ascii file
	!
	!
	!++
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_ASCI_OPEN/LINE
	!	$ LINK/EXEC:AR_EXE AR_ASCI_OPEN,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_ASCI_OPEN.OBJ;*
	!
	! History:
	!
	!	07/19/2001 - Kevin Handy
	!		Added compile statement
	!		Lose CDD requirement
	!--
	%include "source:[ar.open]ar_open.hb"
	map (ar_open) ar_open_cdd ar_open

1000	%include "source:[ar.open]ar_open.opn"

1010	open "aropen.txt" for output as file 1%, &
		recordsize 511%

2000	reset #ar_open.ch%

2100	WHEN ERROR IN
		get #ar_open.ch%, regardless
	USE
		CONTINUE 32767 if err = 11%
		EXIT HANDLER
	END WHEN

	text$ = edit$(AR_OPEN::CUSNUM, 132%) + "	" + &
		edit$(AR_OPEN::INVNUM, 132%) + "	" + &
		edit$(AR_OPEN::TRATYP, 132%) + "	" + &
		edit$(AR_OPEN::TRADAT, 132%) + "	" + &
		format$(AR_OPEN::SALAMT, "######.##") + "	" + &
		format$(AR_OPEN::DISAMT, "######.##") + "	" + &
		format$(AR_OPEN::OTHCHG, "######.##") + "	" + &
		edit$(AR_OPEN::RECNUM, 132%) + "	" + &
		edit$(AR_OPEN::CHKNUM, 132%) + "	" + &
		edit$(AR_OPEN::ARACCT, 132%) + "	" + &
		edit$(AR_OPEN::SUBACC, 132%) + "	" + &
		edit$(AR_OPEN::DESCR, 132%) + "	" + &
		edit$(AR_OPEN::SALNUM, 132%) + "	" + &
		edit$(AR_OPEN::BATCH, 132%) + "	" + &
		edit$(AR_OPEN::UPDATED, 132%) + "	" + &
		edit$(AR_OPEN::CLOSEDATE, 132%) + "	" + &
		edit$(AR_OPEN::DUEDATE, 132%) + "	" + &
		edit$(AR_OPEN::DISCOUNTDATE, 132%)

	print #1%, text$

	goto 2100

32767	end
