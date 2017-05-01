DEFINE RECORD CDD$TOP.TK.TK_MODINFO

        DESCRIPTION IS /*AUTHOR/MODIFICATION INFORMATION ABOUT A PROGRAM*/.

        TK_MODINFO_CDD STRUCTURE.

        /* Element =
        Description = Program name */
        PROGNAME                DATATYPE IS TEXT SIZE IS 40.

        /* Element = SYSTEMID
        Description = System id. */
        SYSTEM                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Name of Author/Modifier */
        PROGRAMMER              DATATYPE IS TEXT SIZE IS 40.

        /* Element = DATE
        Description = Modification Date (YYYYMMDD) */
        MODDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Author/Modifier Flag */
        MODFLAG                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Description of modification */
        MODDESCR                ARRAY 0:20 DATATYPE IS TEXT SIZE IS 80.

        END TK_MODINFO_CDD STRUCTURE.

END TK_MODINFO.
