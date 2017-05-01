DEFINE RECORD CDD$TOP.GL.GL_USERHEAD

        DESCRIPTION IS /*User Defined Journal Header*/.

        GL_USERHEAD_CDD STRUCTURE.

        /* Element =
        Description = Journal Code */
        JCODE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = DEPOSIT
        Description = Deposit number */
        DEPOSIT                 DATATYPE IS TEXT SIZE IS 6.

        /* Element = DATE
        Description = Journal Date (YYYYMMDD) */
        JDATE                   DATATYPE IS TEXT SIZE IS 8.

        END GL_USERHEAD_CDD STRUCTURE.

END GL_USERHEAD.
