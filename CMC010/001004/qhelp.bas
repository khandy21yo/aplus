10	!
 	! DEMO2 - PART TWO OF DEMO.BAS
 	!
 	! AUG 83 - BILL LOVEGROVE
 	!
 
20	  EXTEND
30	  ON ERROR GOTO 19000
 
40	  V$=SYS(CHR$(6%)+CHR$(-7%))
 
100	  OPEN "KB:" AS FILE 12%
 
110	  V$=SYS(CHR$(3%))
 
120	  PRINT #12%,CHR$(155%)+"<";
 
300	  CLS$=CHR$(155%)+"[2J"
310	  DEF FNP$(X%,Y%)=CHR$(155%)+"["+NUM1$(X%)+";"+NUM1$(Y%)+"H"
 
320	  RON$=CHR$(155%)+"[7m"
 	\ ROFF$=CHR$(155%)+"[0m"
 	\ GON$=CHR$(155%)+"[10m"
 	\ GOFF$=CHR$(155%)+"[11m"
 
330	  CLL$=CHR$(155%)+"[0K"
 
340	  ARROW$="*"
 
400	  CUR.WEEK$=DATE$(0%)
 
500	  V$=SYS(CHR$(7%))
 	\ DLY%=ASCII(V$)
 	\ DLY2%=ASCII(MID(V$,2%,1%))
 
510	  NEXT.WEEK$=RIGHT(V$,3%)
 
515	  GOSUB 17000
 
520	  K$=NEXT.WEEK$
 	\ K$=FNNXTD$(K$,7%) FOR L%=1% TO 18%
 	\ FINAL.WEEK$=K$
 
1000	!
 	! CONTINUATION OF DEMO
 	!
 
1010	  PRINT CLS$;
 	\ PRINT FNP$(5%,10%);
 	\ PRINT FNSL$("     That, by the way is an important consideration")
 	\ PRINT FNP$(6%,10%);
 	\ PRINT FNSL$("when comparing computer systems - look beyond")
 	\ PRINT FNP$(7%,10%);
 	\ PRINT FNSL$("just the traffic system and ask how well they can")
 	\ PRINT FNP$(8%,10%);
 	\ PRINT FNSL$("support your accounting needs as well.  TV-TRAFFIC")
 	\ PRINT FNP$(9%,10%);
 	\ PRINT FNSL$("is designed to make the most of that connection")
 	\ PRINT FNP$(10%,10%);
 	\ PRINT FNSL$("with accounting.")
 	\ SLEEP 3%
 
1020	  PRINT CLS$;
 	\ PRINT FNP$(5%,10%);
 	\ PRINT FNSL$("     Getting back to the system, we'll now look")
 	\ PRINT FNP$(6%,10%);
 	\ PRINT FNSL$("at how TV-TRAFFIC handles commercial orders. . .")
 	\ SLEEP 5%
 
1030	  PRINT CLS$;
 	\ PRINT "CMC's TV-TRAFFIC System  V2.0"
 	\ PRINT
 	\ PRINT "*> ";
 	\ PRINT CHR$(155%)+"[>1h";FNP$(25%,60%);RON$;" ";
 		NEXT.WEEK$;" ";ROFF$;FNP$(3%,1%);
 	\ SLEEP 3%
 
1040	PRINT FNBX$(8%,13%,10%,70%);
 	\ PRINT FNP$(10%,15%);
 	\ PRINT FNSL$("     The command we'll start with is ENTER")
 	\ PRINT FNP$(11%,15%);
 	\ PRINT FNSL$("COMMERCIAL (tricky names, right?) . . .")
 
1050	  PRINT FNP$(3%,4%);
 	\ PRINT FNTP$("ENTER COMMERCIAL")
 	\ SLEEP 3%
 
3030	  PRINT CLS$;
3100	! SET UP SCREEN &
	&

3110	  PRINT "Customer Code  ";RON$;SPACE$(6%);ROFF$ &
	\ PRINT &
	\ PRINT " Form #  Type      Source    Conflict  Priority  "; &
		  "Start   - End       Prod." &

3120	  PRINT RON$;FNP$(1%,16%);SPACE$(6%);CLL$; &
	\ PRINT FNP$(4%,2%);SPACE$(5%);FNP$(4%,10%);SPACE$(4%);FNP$(4%,20%); &
		  SPACE$(4%);FNP$(4%,30%);SPACE$(8%);FNP$(4%,42%);SPACE$(1%); &
		  FNP$(4%,50%);SPACE$(8%);FNP$(4%,60%);SPACE$(8%); &
		  FNP$(4%,70%);SPACE$(6%); &
		  ROFF$; &
	\ FOR L%=5% TO 23% &
		\ PRINT &
		\ PRINT CLL$; &
	\ NEXT L% &

3200	  PRINT FNP$(1%,15%);ARROW$;RON$; &
	\ PRINT FNTP$("LOTGSD"); &
	\ PRINT ROFF$; &

3210	  PRINT " - ";"THE GREASY SPOON DRIVE IN" &

3230	  PRINT FNP$(1%,15%);" ";FNP$(4%,1%);ARROW$;RON$; &
	\ K$="12345" &
	\ PRINT FNTP$(K$);ROFF$; &

3250	  PRINT FNP$(4%,1%);" ";FNP$(4%,9%);ARROW$;RON$; &
	\ K$="CM" &
	\ PRINT FNTP$(K$);ROFF$; &

3260	  PRINT FNP$(4%,9%);" ";FNP$(4%,19%);ARROW$;RON$; &
	\ PRINT FNTP$("R");ROFF$; &

3270	  PRINT FNP$(4%,19%);" ";FNP$(4%,29%);ARROW$;RON$; &
	\ PRINT FNTP$("FFOOD");ROFF$; &

3280	  PRINT FNP$(4%,29%);" ";FNP$(4%,41%);ARROW$;RON$; &
	\ PRINT FNTP$("3");ROFF$; &

3290	  PRINT FNP$(5%,29%);CLL$; &

3292	  PRINT FNP$(4%,41%);" ";FNP$(4%,49%);ARROW$;RON$; &
	\ K$="07.15.83" &

3295	  PRINT FNP$(4%,50%);FNTP$(K$);ROFF$;FNP$(5%,29%);CLL$ &

3300	  PRINT FNP$(4%,49%);" ";FNP$(4%,59%);ARROW$;RON$; &
	\ K$="08.19.83" &

3302	  PRINT FNP$(4%,60%);FNTP$(K$);ROFF$; &

3304	  PRINT FNP$(4%,59%);" ";FNP$(4%,69%);ARROW$;RON$; &
	\ PRINT FNTP$("75.00");ROFF$;FNP$(4%,69%);" "; &

3320	  PRINT FNP$(4%,59%);" " &

3400	  PRINT FNP$(6%,1%);"Comments :";FNP$(7%,13%);ARROW$; &
	\ I%,I2%=0% &

3410	  PRINT FNTP$("FALL PROMOTION"); &

3450	  J1%,J2%=0% &
	\ PRINT ROFF$;FNP$(11%,35%);"Enter Schedule # ";1%;":" &
	\ PRINT "   Flight Dates:";FNP$(13%,13%);ARROW$; &

3460	  PRINT CLL$; &
	\ PRINT FNTP$("W/O 07.25.83"); &

3500	  PRINT FNP$(13%+I%,13%);" "; &
	\ PRINT FNP$(18%,2%);"Length"; FNP$(18%,14%);"Time Slot";FNP$(18%,39%); &
		  "Rotate";FNP$(18%,52%); &
		  "MO TU WE TH FR SA SU  Rate" &
	\ I%=1% &
	\ I2%=0% &

3505	  F%=0% &
	\ LSET C1$(3%)="3" &
	\ LSET C1$(4%)=CHR$(I9%) &
	\ LSET C1$(5%)="2" &

3510	  PRINT FNP$(19%,2%);ARROW$;CLL$; &
	\ K$="30" &
	\ PRINT FNTP$("30"); &
	\ PRINT FNP$(19%,2%);" ";FNP$(19%,13%);ARROW$; &
	\ PRINT FNTP$("8.00-10.00"); &

3512	  PRINT FNP$(19%,13%);" ";FNP$(19%,39%);ARROW$; &
	\ PRINT FNTP$("4"); &
	\ PRINT FNP$(19%,39%);" "; &

3514	  FOR LL%=1% TO 7% &
		\ PRINT FNP$(18%+I%,49%+LL%*3%); &
		\ K$="1" &
		\ K$="" IF LL%>5% &
		\ PRINT FNTP$(K$); &
	\ NEXT LL% &
	\ PRINT FNP$(18%+I%,74%); &
	\ K$="125.00" &
	\ PRINT FNTP$(K$) &

4000	  PRINT FNBX$(5%,10%,30%,75%);
 	\ PRINT FNP$(6%,35%);
 	\ PRINT FNSL$("     We won't take time now to examine")
 	\ PRINT FNP$(7%,35%);
 	\ PRINT FNSL$("what each of those items means, but")
 	\ PRINT FNP$(8%,35%);
 	\ PRINT FNSL$("notice a couple of features. . .")
 	\ SLEEP 3%
 
4010	  PRINT FNP$(L%,35%);SPACE$(39%); FOR L%=6% TO 8%
 	\ PRINT FNP$(6%,35%);
 	\ PRINT FNSL$("     Notice the conflict code - this")
 	\ PRINT FNP$(7%,35%);
 	\ PRINT FNSL$("allows flexible control over conflicts.")
 	\ SLEEP 3%
 
4020	  PRINT FNP$(L%,35%);SPACE$(39%); FOR L%=6% TO 7%
 	\ PRINT FNP$(6%,35%);
 	\ PRINT FNSL$("     Notice also the priority code -")
 	\ PRINT FNP$(7%,35%);
 	\ PRINT FNSL$("allows you to develop a system of")
 	\ PRINT FNP$(8%,35%);
 	\ PRINT FNSL$("priorities for commercials.")
 	\ SLEEP 5%
 
4030	  PRINT FNP$(L%,35%);SPACE$(39%); FOR L%=6% TO 8%
 
4040	  PRINT FNP$(6%,35%);
 	\ PRINT FNSL$("     That item titled 'Prod.' is")
 	\ PRINT FNP$(7%,35%);
 	\ PRINT FNSL$("production charge.  If you choose,")
 	\ PRINT FNP$(8%,35%);
 	\ PRINT FNSL$("you can bill production automatically.")
 	\ SLEEP 3%
 	\ PRINT FNP$(L%,35%);SPACE$(39%); FOR L%=6% TO 8%
 
4050	  SLEEP 3%
 	\ PRINT FNP$(6%,35%);
 	\ PRINT FNSL$("     We could have entered several")
 	\ PRINT FNP$(7%,35%);
 	\ PRINT FNSL$("comment lines if we had desired. . .")
 	\ SLEEP 2%
 
4060	  PRINT FNP$(8%,13%);FNTP$("RENEWAL CONTRACT")
 
4070	  SLEEP 3%
 	\ PRINT FNP$(L%,35%);SPACE$(39%) FOR L%=6% TO 7%
 	\ PRINT FNP$(6%,35%);
 	\ PRINT FNSL$("     Dates can take several forms. . .")
 	\ SLEEP 2%
 	\ PRINT FNP$(14%,14%);CLL$;
 	\ PRINT FNTP$("08.15.83-09.12.83")
 	\ SLEEP 3%
 	\ PRINT FNP$(7%,35%);
 	\ PRINT FNSL$("(and there can be more than one, too)")
 
4080	  PRINT FNP$(15%,14%);CLL$;
 	\ PRINT FNTP$("10.15.83")
 	\ SLEEP 3%
 
4090	  PRINT FNP$(l%,35%);SPACE$(39%) FOR L%=6% TO 7%
 	\ PRINT FNP$(6%,35%);
 	\ PRINT FNSL$("     An essentially unlimited number")
 	\ PRINT FNP$(7%,35%);
 	\ PRINT FNSL$("of schedule lines can be entered. . .")
 
4100	  PRINT FNP$(20%,2%);ARROW$;CLL$;
 	\ PRINT FNTP$("60");
 	\ PRINT FNP$(20%,2%)" ";FNP$(20%,13%);ARROW$;FNTP$("12.00");
 	\ PRINT FNP$(20%,13%);" ";FNP$(20%,39%);ARROW$;
 	\ PRINT FNTP$("");FNP$(20%,39%);" ";FNP$(20%,52%);
 	\ PRINT FNTP$("1");"  "; FOR LL%=1% TO 7%
 	\ PRINT FNP$(20%,74%);FNTP$(" 75.00")
 
4110	  PRINT FNP$(L%,35%);SPACE$(39%); FOR L%=6% TO 8%
 	\ PRINT FNP$(6%,35%);
 	\ PRINT FNSL$("     Vertical and/or horizontal")
 	\ PRINT FNP$(7%,35%);
 	\ PRINT FNSL$("rotations can be handled, as well")
 	\ PRINT FNP$(8%,35%);
 	\ PRINT FNSL$("as sophisticated orbits.")
 	\ SLEEP 3%
 
4120	  PRINT CLS$;
 	\ PRINT FNP$(5%,10%);
 	\ PRINT FNSL$("     Before we schedule that commercial, we")
 	\ PRINT FNP$(6%,10%);
 	\ PRINT FNSL$("need to discuss logs. . .")
 	\ SLEEP 2%
 
4130	  PRINT FNP$(8%,10%);
 	\ PRINT FNSL$("     TV-TRAFFIC keeps 20 weeks of logs on hand")
 	\ PRINT FNP$(9%,10%);
 	\ PRINT FNSL$("at one time.  That means right now you could have")
 	\ PRINT FNP$(10%,10%);
 	\ PRINT FNSL$("a log for ");
 
4140	  K$=MID("JanFebMarAprMayJunJulAugSepOctNovDec",
 		VAL(MID(FINAL.WEEK$,4%,2%))-2%,3%)
 	\ K1$=LEFT(FINAL.WEEK$,2%)
 	\ K2$=RIGHT(FINAL.WEEK$,7%)
 	\ PRINT FNSL$(K$+" "+K1$+", 19"+K2$+" !")
 
4150	  PRINT FNP$(12%,10%);
 	\ PRINT FNSL$("     And those are full logs for all 20 weeks")
 	\ PRINT FNP$(13%,10%);
 	\ PRINT FNSL$("including full programming.  As commercial orders")
 	\ PRINT FNP$(14%,10%);
 	\ PRINT FNSL$("come in, they are actually scheduled right into")
 	\ PRINT FNP$(15%,10%);
 	\ PRINT FNSL$("the logs.  Not only can you tell people immediately")
 	\ PRINT FNP$(16%,10%);
 	\ PRINT FNSL$("exactly when their spots are scheduled, you have")
 	\ PRINT FNP$(17%,10%);
 	\ PRINT FNSL$("completely accurate avails too.")
 	\ SLEEP 3%
 
4160	  PRINT FNP$(L%,1%);CLL$ FOR L%=8% TO 17%
 	\ PRINT FNP$(8%,10%);
 	\ PRINT FNSL$("     There's a weekly procedure for automatically")
 	\ PRINT FNP$(9%,10%);
 	\ PRINT FNSL$("removing the oldest week and adding a new week.")
 	\ PRINT FNP$(11%,10%);
 	\ PRINT FNSL$("     If a commercial order extends past the 20th")
 	\ PRINT FNP$(12%,10%);
 	\ PRINT FNSL$("week, it is automatically scheduled into the new")
 	\ PRINT FNP$(13%,10%);
 	\ PRINT FNSL$("weeks as they are created.");
 	\ SLEEP 3%
 
4170	  PRINT CLS$;
 	\ PRINT FNP$(5%,10%);
 	\ PRINT FNSL$("     Now we'll go back and schedule that commercial")
 	\ PRINT FNP$(6%,10%);
 	\ PRINT FNSL$("and show you how it works. . .");
 	\ SLEEP 5%
 
4180	  PRINT CLS$;
 	\ PRINT "CMC's TV-TRAFFIC System  V2.0"
 	\ PRINT
 	\ PRINT "*> ";
 	\ SLEEP 2%
 
4190	  PRINT FNBX$(8%,18%,30%,75%);
 	\ PRINT FNP$(10%,35%);
 	\ PRINT FNSL$("     The command here is SINGLE")
 	\ PRINT FNP$(11%,35%);
 	\ PRINT FNSL$("SKED.  As much as we hate to perpetuate")
 	\ PRINT FNP$(12%,35%);
 	\ PRINT FNSL$("a misspelling, SKED seems to be the")
 	\ PRINT FNP$(13%,35%);
 	\ PRINT FNSL$("nationally accepted abbreviation for")
 	\ PRINT FNP$(14%,35%);
 	\ PRINT FNSL$('"schedule."');
 	\ SLEEP 5%
 
4200	  PRINT FNP$(3%,4%);FNTP$("SINGLE SKED")
 	\ PRINT
 	\ PRINT FNP$(L%,1%);SPACE$(30%); FOR L%=1% TO 3%
 	\ PRINT FNP$(1%,1%);
 	\ PRINT "Customer Code ? ";
 	\ SLEEP 1%
 	\ PRINT FNTP$("LOTGSR")
 	\ PRINT "Form # ";
 	\ PRINT FNTP$("12345")
 
4210	  SLEEP 2%
 	\ PRINT
 	\ PRINT "Scheduling for LOTGSR 12345 - THE GREASY SPOON"
 	\ PRINT
 
4220	  PRINT "Scheduling for W/O 07.15.83"
 	\ PRINT
 	\ SLEEP 2%
 
4230	  PRINT "Schedule #1 :"
 	\ PRINT "  MO 08.15.00"
 	\ SLEEP 1%
 	\ PRINT "  TU 09.27.30"
 	\ SLEEP 1%
 	\ PRINT "  TH 07.58.30"
 	\ SLEEP 1%
 	\ PRINT "  FR 08.47.00"
 	\ PRINT
 
4240	  SLEEP 2%
 	\ PRINT "Scheduling for W/O 07.22.83"
 	\ PRINT
 	\ SLEEP 1%
 	\ PRINT "Schedule #1 :"
 	\ PRINT "  TU 09.05.00"
 	\ SLEEP 1%
 	\ PRINT "  WE 08.45.00"
 	\ SLEEP 1%
 	\ PRINT "  TH 07.58.30"
 	\ SLEEP 1%
 	\ PRINT "  No room for rotation !"
 	\ SLEEP 1%
 
4250	  PRINT
 	\ PRINT "Sorting. . ."
 	\ SLEEP 1%
 	\ PRINT
 	\ PRINT "Customer Code ? ";
 	\ SLEEP 5%
 
4260	  PRINT FNP$(L%,35%);SPACE$(40%); FOR L%=10% TO 14%
 
4270	  PRINT FNP$(10%,35%);
 	\ PRINT FNSL$("     You see immediately what could")
 	\ PRINT FNP$(11%,35%);
 	\ PRINT FNSL$("and couldn't schedule.  You can come")
 	\ PRINT FNP$(12%,35%);
 	\ PRINT FNSL$("back at any time and do this. . .");
 	\ SLEEP 3%
 
4280	  PRINT FNP$(L%,1%);CLL$; FOR L%=1% TO 7%
 	\ PRINT FNP$(L%,1%);SPACE$(28%); FOR L%=8% TO 24%
 	\ SLEEP 3%
 
4290	  PRINT FNP$(1%,1%);
 	\ PRINT "CMC's TV-TRAFFIC System  V2.0"
 	\ PRINT
 	\ PRINT "*> ";
 	\ SLEEP 2%
 
4300	  PRINT FNTP$("EXAMINE COMMERCIAL")
 	\ SLEEP 2%
 
4320	  PRINT CLS$;
 	\ PRINT
 	\ PRINT TAB(30%);"COMMERICAL ORDER"
 	\ PRINT
 	\ PRINT "Cust #"
 	\ PRINT "Form #     Src/Type   Conflict  Priority  Start Date"+
 		"   End Date"
 	\ PRINT STRING$(75%,ASCII("-"))
 	\ PRINT "LOTGSR :THE GREASY SPOON"
 	\ PRINT "12345  :     R/CM     FFOOD       [3]      07.15.83   TFN"
 	\ PRINT "       :"
 	\ PRINT "       :  Production : $75.00"
 	\ PRINT "       :  FALL PROMOTION"
 	\ PRINT "       :"
 	\ PRINT "       : 1>"
 	\ SLEEP 5%
 
4330	  PRINT "       :    07.15.83-07.28.83"
 	\ PRINT "       :"
 	\ PRINT "       :    Time Slot             Rot  MO TU WE TH FR SA SU"+
 		"    Rate Lngth"
 	\ PRINT "       :    07.00-09.30           (4)   1  1  1  1  1      "+
 		" $125.00 00.30"
 	\ PRINT "       :"
 	\ SLEEP 5%
 
4340	  PRINT "       :  Week 07.15.83"
 	\ PRINT "       :  MO 08.15.00                         $125.00  00.30"
 	\ PRINT "       :  TU 09.27.30                         $125.00  00.30"
 	\ PRINT "       :  TH 07.58.30                         $125.00  00.30"
 	\ PRINT "       :  FR 08.47.00                         $125.00  00.30"
 	\ PRINT "       :"
 	\ PRINT "       :  Week 07.22.83"
 	\ PRINT "       :  TU 09.05.00                         $125.00  00.30"
 	\ PRINT "       :  WE 08.45.00                         $125.00  00.30"
 	\ PRINT "       :  TH 07.58.30                         $125.00  00.30"
 	\ SLEEP 3%
 
4350	  PRINT FNP$(L%,30%);SPACE$(49%); FOR L%=6% TO 14%
 	\ PRINT FNBX$(7%,12%,31%,75%);
 	\ PRINT FNP$(9%,35%);
 	\ PRINT FNSL$("     In this way the exact status of")
 	\ PRINT FNP$(10%,35%);
 	\ PRINT FNSL$("any order can be checked at any time.")
 
4360	  SLEEP 3%
 	\ PRINT CLS$;
 	\ PRINT FNP$(5%,10%);
 	\ PRINT FNSL$("     TV-TRAFFIC has many other features which we")
 	\ PRINT FNP$(6%,10%);
 	\ PRINT FNSL$("won't take the time to show now.  You've seen just")
 	\ PRINT FNP$(7%,10%);
 	\ PRINT FNSL$("a sample of what it's like to see TV-TRAFFIC in use.")
 	\ PRINT FNP$(9%,10%);
 	\ PRINT FNSL$("     Look over the promotional literature and ask")
 	\ PRINT FNP$(10%,10%);
 	\ PRINT FNSL$("about a hands-on demonstration, and see if TV-TRAFFIC")
 	\ PRINT FNP$(11%,10%);
 	\ PRINT FNSL$("isn't just the thing for you.");
 	\ SLEEP 3%
 
4370	  PRINT FNP$(13%,10%);
 	\ PRINT FNSL$("     Thanks for your time !");
 	\ SLEEP 10%
 	\ PRINT CLS$;
 	\ CHAIN "DEMO"
 
9000 SLEEP 20%
10000	!
 	! END
 	!
 
10010	  PRINT CLS$;CHR$(155%)+"[>1l";
 	\ V$=SYS(CHR$(2%))
 	\ CLOSE 2%
 	\ GOTO 32767
 
11000	!
 	! DRAW A BOX
 	!
 
11010	  DEF FNBX$(V1%,V2%,H1%,H2%)
 	\ PRINT GON$;
 	\ PRINT FNP$(V1%,H1%);"f";STRING$(H2%-H1%-1%,97%);"c";
 	\ PRINT CHR$(8%);CHR$(10%);"`"; FOR L%=V1% TO V2%-1%
 	\ PRINT FNP$(V2%,H2%);"d";
 	\ PRINT CHR$(8%);CHR$(8%);"a"; FOR L%=H1% TO H2%-2%
 	\ PRINT FNP$(V2%,H1%);"e";
 	\ PRINT FNP$(L%,H1%);"`"; FOR L%=V2%-1% TO V1%+1% STEP -1%
 	\ PRINT GOFF$;
 	\ FNEND
12000	!
 	! Function to return the next date for a date given in yy.mm.dd format
 	!
12010	  DEF FNNXTD$(DAY$,DAY%)
 		\ QQ.MONTH%=VAL(LEFT(DAY$,2%))
 		\ QQ.DAY%=VAL(MID(DAY$,4%,2%))
 		\ QQ.YEAR%=VAL(RIGHT(DAY$,7%))
 		\ MD%(2%)=28%
 		\ MD%(2%)=29% IF QQ.YEAR%<>0% AND QQ.YEAR%/4%*4%=QQ.YEAR%
 		\ QQ.DAY%=QQ.DAY%+DAY%
 		\ IF QQ.DAY%>MD%(QQ.MONTH%)
 		  THEN	  QQ.DAY%=QQ.DAY%-MD%(QQ.MONTH%)
 			\ QQ.MONTH%=QQ.MONTH%+1%
 			\ IF QQ.MONTH%=13%
 			  THEN	  QQ.MONTH%=1%
 				\ QQ.YEAR%=QQ.YEAR%+1%
 
12012		  IF QQ.DAY%<1%
 		  THEN	  QQ.MONTH%=QQ.MONTH%-1%
 			\ QQ.MONTH%=12% IF QQ.MONTH%=0%
 			\ QQ.YEAR%=QQ.YEAR%-1% IF QQ.MONTH%=12%
 			\ QQ.YEAR%=99% IF QQ.YEAR%=0%
 			\ QQ.DAY%=QQ.DAY%+MD%(QQ.MONTH%)
 
12015		  FNNXTD$=FND7$(NUM1$(QQ.MONTH%)+"."+NUM1$(QQ.DAY%)+"."+
 			  NUM1$(QQ.YEAR%))
 	\ FNEND
 
14050	!
 	! FORMAT DATE TO MM.DD.YY , FILL WITH ZEROS
 	!
14060	DEF FND7$(D7$)
 	\ IF LEN(D7$)<6% AND D7$<>"" THEN
 		 D7=VAL(RIGHT(DATE$(0%),8%))
 		\ D7=D7+1 IF VAL(LEFT(D7$,INSTR(1%,D7$,".")-1%))<
 				FNM%(MID(DATE$(0%),4%,3%))
 		\ D7$=D7$+"."+RIGHT(NUM1$(100%+D7),2%)
14070	D7$="0"+D7$ IF INSTR(1%,D7$,".")=2%
 	: D7$=LEFT(D7$,3%)+"0"+RIGHT(D7$,4%) IF INSTR(4%,D7$,".")=5%
 	: FND7$=D7$ : FNEND
14200	DEF FND9%(M%,D%,Y%)
 	\ X%=((Y%-1%)/4%+Y%+1%) 
 	\ X%=X%-X%/7%*7%
14210	X%=X%+VAL(MID("033614625035",M%,1%))
 	\ X%=X%+1% IF Y%/4%*4%=Y% AND M%>2%
 	\ X%=X%+D%-1% 
 	\ X%=X%-X%/7%*7%+1%
 	\ X%=X%-1% 
 	\ X%=7% IF X%=0%
 	\ FND9%=X%
 	\ FNEND
14280	DEF FNM%(K$)=[
 		INSTR(
 			1%,
 			"JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",
 			CVT$$(K$,-1%)
 			)-1%
 		]/3%+1%
14300	DEF FNMON$(MON$)
 	\ MONM%=VAL(LEFT(MON$,2%))
 	\ MOND%=VAL(MID(MON$,4%,2%))
 	\ MONY%=VAL(RIGHT(MON$,7%))
14310	MON%=FND9%(MONM%,MOND%,MONY%)
14390	FNMON$=FNNXTD$(MON$,1%-MON%)
 	\ FNEND
 
15000	!
 	! SLOW-PRINT FUNCTION
 	!
 
15010	  DEF FNSL$(S$)
 		\ K=0.
 
15020	  FOR L%=1% TO LEN(S$)
 		\ PRINT MID(S$,L%,1%);STRING$(DLY%,0%);
 
15030	NEXT L%
 	\ FOR X=1 TO DLY%*15% \ NEXT X
 	\ FNEND
 
16000	!
 	! TYPING FUNCTION
 	!
 
16010	  DEF FNTP$(T$)
 	\ SLEEP 1%
 
16015	  FOR L%=1% TO LEN(T$)
 		\ PRINT MID(T$,L%,1%);CHR$(7%);STRING$(DLY2%*RND+DLY2%,0%);
 
16020	NEXT L%
 	\ SLEEP 2%
 	\ FNEND
 
17000
 	! Set up variables
 
17010	DIM MD%(12%)
17020	MD%(0%)=31%
 	\ MD%(1%)=31%
 	\ MD%(2%)=28%
 	\ MD%(3%)=31%
 	\ MD%(4%)=30%
 	\ MD%(5%)=31%
 	\ MD%(6%)=30%
 	\ MD%(7%)=31%
 	\ MD%(8%)=31%
 	\ MD%(9%)=30%
 	\ MD%(10%)=31%
 	\ MD%(11%)=30%
 	\ MD%(12%)=31%
17200	RETURN
19000	!
 	! ERROR TRAPPING
 	!
 
19010	  IF ERR=28
 	  THEN	  PRINT CLS$;
 		\ PRINT FNSL$("Even abnormal exits can be handled, too!")
 		\ SLEEP 3%
 		\ RESUME 10000
 
19900	  PRINT
 	\ PRINT "Woops, unexpected error";ERR;"at line";ERL;"."
 	\ PRINT "Demonstration ended."
 	\ SLEEP 3%
 	\ RESUME 10000
 
32767 END
                                                                                                                                                                                                                                                                                 