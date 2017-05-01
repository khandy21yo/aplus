DEFINE RECORD CDD$TOP.GL.GL_TRICONTROL

        DESCRIPTION IS /*Tri-Spur Journal Control File*/.

        GL_TRICONTROL_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DESCRIPTION
        Description = Description */
        DESCRIPTION             ARRAY 0:20 DATATYPE IS TEXT SIZE IS 20.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 ARRAY 0:20 DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Flags */
        FLAG                    ARRAY 0:20 DATATYPE IS TEXT SIZE IS 1.

        END GL_TRICONTROL_CDD STRUCTURE.

END GL_TRICONTROL.
