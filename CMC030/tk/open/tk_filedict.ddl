DEFINE RECORD CDD$TOP.TK.TK_FILEDICT

        DESCRIPTION IS /*File Structure Dictionary*/.

        TK_FILEDICT_CDD STRUCTURE.

        /* Element =
        Description = File Name */
        FILENAME                DATATYPE IS TEXT SIZE IS 39.

        /* Element =
        Description = File Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 60.

        /* Element =
        Description = System maintaining file */
        SYSTEM                  DATATYPE IS TEXT SIZE IS 3.

        END TK_FILEDICT_CDD STRUCTURE.

END TK_FILEDICT.
