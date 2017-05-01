1	%TITLE "Show error messages"
	%SBTTL "SHOERR"
	%IDENT "V3.6a Calico"

	!++
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:SHOERR/LINE
	!	$ LINK/EXECUTABLE=UTL_EXE: SHOERR, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SHOERR.OBJ;*
	!
	! Modification history:
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG)

	EXTERNAL INTEGER FUNCTION SYS$GETMSG

	COMMON	(MSGCOM) MESSAGE$ = 80

10	WHEN ERROR IN
		INPUT A$
		IF LEFT(A$, 1%) = "0"
		THEN
			!
			! Convert from hex
			!	Shouldn't there be a function to do this?
			!
			A% = 0%
			FOR I% = 1% TO LEN(A$)
				A% = A% * 16% + &
					INSTR(1%, "0123456789ABCDEF", &
					MID(A$, I%, 1%)) - 1%
			NEXT I%
		ELSE
			A% = VAL%(A$)
		END IF
	USE
		CONTINUE 32767
	END WHEN

	SYS_STATUS% = SYS$GETMSG(A% BY VALUE, RSN_LEN%, MESSAGE$,,,)

	PRINT A%; "= "; LEFT(MESSAGE$, RSN_LEN%)
	GOTO 10

32767	END
