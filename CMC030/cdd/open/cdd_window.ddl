DEFINE RECORD CDD$TOP.CDD.CDD_WINDOW

        DESCRIPTION IS /*Window Data*/.

        CDD_WINDOW_CDD STRUCTURE.

        /* Element =
        Description = Window identification number */
        IDENT                   DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Title for top of window */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Name to be used by help function */
        NHELP                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Channel that file is open on */
        CHAN                    DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Width of the window */
        HSIZE                   DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = height of the window */
        VSIZE                   DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Horozontal position of the window */
        HPOS                    DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Vertical position of the window */
        VPOS                    DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Number of fields in window */
        NITEMS                  DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Number of keys */
        NKEYS                   DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Key names */
        KNAME                   ARRAY 0:7 DATATYPE IS TEXT SIZE IS 16.

        /* Element =
        Description = Fields in keys (8 Keys, 8 fields per key */
        KFIELD                  ARRAY 0:7 0:8 DATATYPE IS SIGNED WORD.

        /* Element =
        Description = View width */
        HVIEW                   DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = View height */
        VVIEW                   DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = For those routines that need to store it */
        WNUMBER                 DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = For a relative file, the record number t */
        RELREC                  DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = For a journal, number of physical lines */
        LINREC                  DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = For a journal, top line available on scr */
        TOPLIN                  DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = For a journal, bottom line on screen ava */
        BOTLIN                  DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = For a journal, Currently selected record */
        CURREC                  DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = For a journal, Line on screen for curren */
        CURLIN                  DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = For a journal, record on the top of the */
        TOPREC                  DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = For a journal, the maximum number of rec */
        MAXREC                  DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = For a journal, the total number of recor */
        TOTREC                  DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Place to store the window number for a p */
        LWINDOW                 DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Width of the page window */
        LWIDTH                  DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Height of the page window */
        LHEIGHT                 DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Vertical position of the page window */
        LVPOS                   DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Horozontal position of the page window */
        LHPOS                   DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Last page to be usable */
        LLAST                   DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Last field on each page */
        LPAGE                   ARRAY 0:7 DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Current page selected */
        LCURR                   DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = View horozontal position */
        VHPOS                   DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = View vertical position */
        VVPOS                   DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Fill section */
        MENU                    DATATYPE IS TEXT SIZE IS 28.

        /* Element =
        Description = Hard/soft flags for items */
        HFLAG                   ARRAY 0:64 DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Flag word */
        FLAGS                   DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Titles for screen pages */
        LTITLE                  ARRAY 0:7 DATATYPE IS TEXT SIZE IS 16.

        END CDD_WINDOW_CDD STRUCTURE.

END CDD_WINDOW.
