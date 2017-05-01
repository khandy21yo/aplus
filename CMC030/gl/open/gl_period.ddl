DEFINE RECORD CDD$TOP.GL.GL_PERIOD

        DESCRIPTION IS /*General Ledger Period File*/.

        GL_PERIOD_CDD STRUCTURE.

        /* Element =
        Description = */
        PERIOD                  ARRAY 0:13 DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = */
        LASTPERCLO              DATATYPE IS SIGNED WORD.

        /* Element =
        Description = */
        FPFY                    DATATYPE IS SIGNED WORD.

        /* Element =
        Description = */
        YEAR                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = */
        BTHNUM                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        SUMMARYTOTAL            DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        SUMMARYACCT             DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = */
        NEWYEAR                 DATATYPE IS SIGNED WORD.

        /* Element =
        Description = End date in period */
        ENDDATE                 ARRAY 0:13 DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = 1-Closing, 2-Resetting */
        CLOSEFLAG               DATATYPE IS TEXT SIZE IS 1.

        END GL_PERIOD_CDD STRUCTURE.

END GL_PERIOD.
