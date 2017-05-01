/*
 * File Layout for: CDD.CDD_WINDOW on 11-Mar-99
 *
 * Window Data
 */

struct cdd_window_cdd
{
/* Element =
   Description = Window identification number */
	long ident;
/* Element =
   Description = Title for top of window */
	char descr[40];
/* Element =
   Description = Name to be used by help function */
	char nhelp[40];
/* Element =
   Description = Channel that file is open on */
	long chan;
/* Element =
   Description = Width of the window */
	long hsize;
/* Element =
   Description = height of the window */
	long vsize;
/* Element =
   Description = Horozontal position of the window */
	long hpos;
/* Element =
   Description = Vertical position of the window */
	long vpos;
/* Element =
   Description = Number of fields in window */
	long nitems;
/* Element =
   Description = Number of keys */
	int nkeys;
/* Element =
   Description = Key names */
	char kname[8][16];
/* Element =
   Description = Fields in keys (8 Keys, 8 fields per key */
	int kfield[8][9];
/* Element =
   Description = View width */
	long hview;
/* Element =
   Description = View height */
	long vview;
/* Element =
   Description = For those routines that need to store it */
	long wnumber;
/* Element =
   Description = For a relative file, the record number t */
	long relrec;
/* Element =
   Description = For a journal, number of physical lines */
	long linrec;
/* Element =
   Description = For a journal, top line available on scr */
	long toplin;
/* Element =
   Description = For a journal, bottom line on screen ava */
	long botlin;
/* Element =
   Description = For a journal, Currently selected record */
	long currec;
/* Element =
   Description = For a journal, Line on screen for curren */
	long curlin;
/* Element =
   Description = For a journal, record on the top of the */
	long toprec;
/* Element =
   Description = For a journal, the maximum number of rec */
	long maxrec;
/* Element =
   Description = For a journal, the total number of recor */
	long totrec;
/* Element =
   Description = Place to store the window number for a p */
	long lwindow;
/* Element =
   Description = Width of the page window */
	long lwidth;
/* Element =
   Description = Height of the page window */
	long lheight;
/* Element =
   Description = Vertical position of the page window */
	long lvpos;
/* Element =
   Description = Horozontal position of the page window */
	long lhpos;
/* Element =
   Description = Last page to be usable */
	long llast;
/* Element =
   Description = Last field on each page */
	long lpage[8];
/* Element =
   Description = Current page selected */
	long lcurr;
/* Element =
   Description = View horozontal position */
	long vhpos;
/* Element =
   Description = View vertical position */
	long vvpos;
/* Element =
   Description = Fill section */
	char menu[28];
/* Element =
   Description = Hard/soft flags for items */
	int hflag[65];
/* Element =
   Description = Flag word */
	long flags;
/* Element =
   Description = Titles for screen pages */
	char ltitle[8][16];
};
