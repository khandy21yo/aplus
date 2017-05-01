

	FUNCTION LONG PR_FUNC_BANKCODE_BOA(STRING PRDATE, &
		GFLOAT AMOUNT, LONG FIXEDCODE)

	TOTAL% = FIXEDCODE%

	!
	! Year
	!
	SELECT VAL%(MID(PRDATE, 5%, 2%))
	CASE 1%
		TOTAL% = TOTAL% + 158%
	CASE 2%
		TOTAL% = TOTAL% + 44%
	CASE 3%
		TOTAL% = TOTAL% + 213%
	CASE 4%
		TOTAL% = TOTAL% + 182%
	CASE 5%
		TOTAL% = TOTAL% + 37%
	CASE 6%
		TOTAL% = TOTAL% + 161%
	CASE 7%
		TOTAL% = TOTAL% + 12%
	CASE 8%
		TOTAL% = TOTAL% + 103%
	CASE 9%
		TOTAL% = TOTAL% + 96%
	CASE 10%
		TOTAL% = TOTAL% + 85%
	CASE 11%
		TOTAL% = TOTAL% + 227%
	CASE 12%
		TOTAL% = TOTAL% + 149%
	END SELECT


	!
	! Month
	!
	SELECT VAL%(MID(PRDATE, 7%, 2%))
	CASE 1%
		TOTAL% = TOTAL% + 112%
	CASE 2%
		TOTAL% = TOTAL% + 108%
	CASE 3%
		TOTAL% = TOTAL% + 49%
	CASE 4%
		TOTAL% = TOTAL% + 173%
	CASE 5%
		TOTAL% = TOTAL% + 274%
	CASE 6%
		TOTAL% = TOTAL% + 41%
	CASE 7%
		TOTAL% = TOTAL% + 137%
	CASE 8%
		TOTAL% = TOTAL% + 70%
	CASE 9%
		TOTAL% = TOTAL% + 68%
	CASE 10%
		TOTAL% = TOTAL% + 56%
	CASE 11%
		TOTAL% = TOTAL% + 203%
	CASE 12%
		TOTAL% = TOTAL% + 118%
	CASE 13%
		TOTAL% = TOTAL% + 22%
	CASE 14%
		TOTAL% = TOTAL% + 226%
	CASE 15%
		TOTAL% = TOTAL% + 199%
	CASE 16%
		TOTAL% = TOTAL% + 55%
	CASE 17%
		TOTAL% = TOTAL% + 211%
	CASE 18%
		TOTAL% = TOTAL% + 158%
	CASE 19%
		TOTAL% = TOTAL% + 27%
	CASE 20%
		TOTAL% = TOTAL% + 140%
	CASE 21%
		TOTAL% = TOTAL% + 152%
	CASE 22%
		TOTAL% = TOTAL% + 135%
	CASE 23%
		TOTAL% = TOTAL% + 81%
	CASE 24%
		TOTAL% = TOTAL% + 260%
	CASE 25%
		TOTAL% = TOTAL% + 113%
	CASE 26%
		TOTAL% = TOTAL% + 61%
	CASE 27%
		TOTAL% = TOTAL% + 110%
	CASE 28%
		TOTAL% = TOTAL% + 98%
	CASE 29%
		TOTAL% = TOTAL% + 32%
	CASE 30%
		TOTAL% = TOTAL% + 251%
	CASE 31%
		TOTAL% = TOTAL% + 274%
	END SELECT

	!
	! 100,000,000,000
	!
 !	ABSAMOUNT% = INT(ABS(AMOUNT))
 !	AMT% = MOD((ABSAMOUNT% / 100000000000%), 10%)
 !
 !	SELECT AMT%
 !	CASE 1%
 !		TOTAL% = TOTAL% + 22%
 !	CASE 2%
 !		TOTAL% = TOTAL% + 14%
 !	CASE 3%
 !		TOTAL% = TOTAL% + 21%
 !	CASE 4%
 !		TOTAL% = TOTAL% + 8%
 !	CASE 5%
 !		TOTAL% = TOTAL% + 20%
 !	CASE 6%
 !		TOTAL% = TOTAL% + 7%
 !	CASE 7%
 !		TOTAL% = TOTAL% + 2%
 !	CASE 8%
 !		TOTAL% = TOTAL% + 11%
 !	CASE 9%
 !		TOTAL% = TOTAL% + 4%
 !	END SELECT

	!
	! 10,000,000,000
	!
	AMT% = MOD(ABSAMOUNT% / 1000000000%, 10%)

	SELECT AMT%
	CASE 1%
		TOTAL% = TOTAL% + 51%
	CASE 2%
		TOTAL% = TOTAL% + 91%
	CASE 3%
		TOTAL% = TOTAL% + 8%
	CASE 4%
		TOTAL% = TOTAL% + 1%
	CASE 5%
		TOTAL% = TOTAL% + 5%
	CASE 6%
		TOTAL% = TOTAL% + 8%
	CASE 7%
		TOTAL% = TOTAL% + 6%
	CASE 8%
		TOTAL% = TOTAL% + 184%
	CASE 9%
		TOTAL% = TOTAL% + 2%
	END SELECT

	!
	! 1,000,000,000
	!
	AMT% = MOD(ABSAMOUNT% / 100000000%, 10%)

	SELECT AMT%
	CASE 1%
		TOTAL% = TOTAL% + 64%
	CASE 2%
		TOTAL% = TOTAL% + 301%
	CASE 3%
		TOTAL% = TOTAL% + 218%
	CASE 4%
		TOTAL% = TOTAL% + 151%
	CASE 5%
		TOTAL% = TOTAL% + 135%
	CASE 6%
		TOTAL% = TOTAL% + 97%
	CASE 7%
		TOTAL% = TOTAL% + 277%
	CASE 8%
		TOTAL% = TOTAL% + 94%
	CASE 9%
		TOTAL% = TOTAL% + 182%
	END SELECT

	!
	! 100,000,000
	!
	AMT% = MOD(ABSAMOUNT% / 100000000%, 10%)

	SELECT AMT%
	CASE 1%
		TOTAL% = TOTAL% + 54%
	CASE 2%
		TOTAL% = TOTAL% + 291%
	CASE 3%
		TOTAL% = TOTAL% + 208%
	CASE 4%
		TOTAL% = TOTAL% + 141%
	CASE 5%
		TOTAL% = TOTAL% + 125%
	CASE 6%
		TOTAL% = TOTAL% + 87%
	CASE 7%
		TOTAL% = TOTAL% + 167%
	CASE 8%
		TOTAL% = TOTAL% + 84%
	CASE 9%
		TOTAL% = TOTAL% + 172%
	END SELECT

	!
	! 10,000,000
	!
	AMT% = MOD(ABSAMOUNT% / 10000000%, 10%)

	SELECT AMT%
	CASE 1%
		TOTAL% = TOTAL% + 44%
	CASE 2%
		TOTAL% = TOTAL% + 281%
	CASE 3%
		TOTAL% = TOTAL% + 198%
	CASE 4%
		TOTAL% = TOTAL% + 131%
	CASE 5%
		TOTAL% = TOTAL% + 125%
	CASE 6%
		TOTAL% = TOTAL% + 77%
	CASE 7%
		TOTAL% = TOTAL% + 257%
	CASE 8%
		TOTAL% = TOTAL% + 74%
	CASE 9%
		TOTAL% = TOTAL% + 162%
	END SELECT

	!
	! 1,000,000
	!
	AMT% = MOD(ABSAMOUNT% / 1000000%, 10%)

	SELECT AMT%
	CASE 1%
		TOTAL% = TOTAL% + 280%
	CASE 2%
		TOTAL% = TOTAL% + 167%
	CASE 3%
		TOTAL% = TOTAL% + 110%
	CASE 4%
		TOTAL% = TOTAL% + 136%
	CASE 5%
		TOTAL% = TOTAL% + 75%
	CASE 6%
		TOTAL% = TOTAL% + 61%
	CASE 7%
		TOTAL% = TOTAL% + 126%
	CASE 8%
		TOTAL% = TOTAL% + 151%
	CASE 9%
		TOTAL% = TOTAL% + 98%
	END SELECT

	!
	! 100,000
	!
	AMT% = MOD(ABSAMOUNT% / 100000%, 10%)

	SELECT AMT%
	CASE 1%
		TOTAL% = TOTAL% + 80%
	CASE 2%
		TOTAL% = TOTAL% + 118%
	CASE 3%
		TOTAL% = TOTAL% + 133%
	CASE 4%
		TOTAL% = TOTAL% + 189%
	CASE 5%
		TOTAL% = TOTAL% + 111%
	CASE 6%
		TOTAL% = TOTAL% + 156%
	CASE 7%
		TOTAL% = TOTAL% + 177%
	CASE 8%
		TOTAL% = TOTAL% + 114%
	CASE 9%
		TOTAL% = TOTAL% + 137%
	END SELECT

	!
	! 10,000
	!
	AMT% = MOD(ABSAMOUNT% / 10000%, 10%)

	SELECT AMT%
	CASE 1%
		TOTAL% = TOTAL% + 30%
	CASE 2%
		TOTAL% = TOTAL% + 46%
	CASE 3%
		TOTAL% = TOTAL% + 119%
	CASE 4%
		TOTAL% = TOTAL% + 141%
	CASE 5%
		TOTAL% = TOTAL% + 208%
	CASE 6%
		TOTAL% = TOTAL% + 82%
	CASE 7%
		TOTAL% = TOTAL% + 191%
	CASE 8%
		TOTAL% = TOTAL% + 205%
	CASE 9%
		TOTAL% = TOTAL% + 159%
	END SELECT

	!
	! 1,000
	!
	AMT% = MOD(ABSAMOUNT% / 1000%, 10%)

	SELECT AMT%
	CASE 1%
		TOTAL% = TOTAL% + 140%
	CASE 2%
		TOTAL% = TOTAL% + 72%
	CASE 3%
		TOTAL% = TOTAL% + 91%
	CASE 4%
		TOTAL% = TOTAL% + 161%
	CASE 5%
		TOTAL% = TOTAL% + 112%
	CASE 6%
		TOTAL% = TOTAL% + 63%
	CASE 7%
		TOTAL% = TOTAL% + 214%
	CASE 8%
		TOTAL% = TOTAL% + 122%
	CASE 9%
		TOTAL% = TOTAL% + 243%
	END SELECT

	!
	! 100
	!
	AMT% = MOD(ABSAMOUNT% / 100%, 10%)

	SELECT AMT%
	CASE 1%
		TOTAL% = TOTAL% + 152%
	CASE 2%
		TOTAL% = TOTAL% + 213%
	CASE 3%
		TOTAL% = TOTAL% + 99%
	CASE 4%
		TOTAL% = TOTAL% + 207%
	CASE 5%
		TOTAL% = TOTAL% + 135%
	CASE 6%
		TOTAL% = TOTAL% + 148%
	CASE 7%
		TOTAL% = TOTAL% + 154%
	CASE 8%
		TOTAL% = TOTAL% + 252%
	CASE 9%
		TOTAL% = TOTAL% + 73%
	END SELECT

	!
	! 10
	!
	AMT% = MOD(ABSAMOUNT% / 10%, 10%)

	SELECT AMT%
	CASE 1%
		TOTAL% = TOTAL% + 187%
	CASE 2%
		TOTAL% = TOTAL% + 228%
	CASE 3%
		TOTAL% = TOTAL% + 183%
	CASE 4%
		TOTAL% = TOTAL% + 232%
	CASE 5%
		TOTAL% = TOTAL% + 164%
	CASE 6%
		TOTAL% = TOTAL% + 155%
	CASE 7%
		TOTAL% = TOTAL% + 132%
	CASE 8%
		TOTAL% = TOTAL% + 71%
	CASE 9%
		TOTAL% = TOTAL% + 22%
	END SELECT

	!
	! 10
	!
	AMT% = MOD(ABSAMOUNT%, 10%)

	SELECT AMT%
	CASE 1%
		TOTAL% = TOTAL% + 140%
	CASE 2%
		TOTAL% = TOTAL% + 68%
	CASE 3%
		TOTAL% = TOTAL% + 172%
	CASE 4%
		TOTAL% = TOTAL% + 214%
	CASE 5%
		TOTAL% = TOTAL% + 160%
	CASE 6%
		TOTAL% = TOTAL% + 133%
	CASE 7%
		TOTAL% = TOTAL% + 182%
	CASE 8%
		TOTAL% = TOTAL% + 79%
	CASE 9%
		TOTAL% = TOTAL% + 131%
	END SELECT

	PR_FUNC_BANKCODE_BOA = TOTAL%

	END FUNCTION
