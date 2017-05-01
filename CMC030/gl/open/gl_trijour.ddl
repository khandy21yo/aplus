DEFINE RECORD CDD$TOP.GL.GL_TRIJOUR

        DESCRIPTION IS /*Tri-Spur Journal*/.

        GL_TRIJOUR_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = Transaction Date (YYYYMMDD) */
        TRANDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = DOLLAR
        Description = Dollar Amounts */
        AMOUNT                  ARRAY 0:20 DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = General Ledger Account Numbers */
        ACCOUNT                 ARRAY 0:20 DATATYPE IS TEXT SIZE IS 18.

        /* Element = DESCRIPTION
        Description = Description */
        DESCRIPTION             ARRAY 0:20 DATATYPE IS TEXT SIZE IS 20.

        END GL_TRIJOUR_CDD STRUCTURE.

END GL_TRIJOUR.
