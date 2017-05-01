DEFINE RECORD CDD$TOP.TK.TK_MODDATE

        DESCRIPTION IS /*Check Modification History Dates*/.

        TK_MODDATE_CDD STRUCTURE.

        /* Element =
        Description = Program Editor's Name */
        PROG_EDITOR             DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Date Program was Writen */
        DATE                    DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Name of Program that is being checked */
        PROG_NAME               DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Program Editor who Modified program */
        MOD_EDITOR              DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Date Program was Modified */
        MOD_DATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Description of Modification */
        MOD_DESCRIPTION         DATATYPE IS TEXT SIZE IS 80.

        /* Element =
        Description = Counter to keep modifications sorted */
        XX_FIELD                DATATYPE IS TEXT SIZE IS 2.

        END TK_MODDATE_CDD STRUCTURE.

END TK_MODDATE.
