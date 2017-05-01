DEFINE RECORD CDD$TOP.GL.GL_USERDEF

        DESCRIPTION IS /*User Defined GL Journal Definition File*/.

        GL_USERDEF_CDD STRUCTURE.

        /* Element =
        Description = Journal Code */
        JCODE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Line Number */
        JLINE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = DESCRIPTION
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = AR/AP Posting flag */
        ARPFLAG                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = YESNO
        Description = Input units? */
        UNITFLAG                DATATYPE IS TEXT SIZE IS 1.

        /* Element = XREF
        Description = Cross Reference */
        XREF                    DATATYPE IS TEXT SIZE IS 10.

        /* Element = YESNO
        Description = Duplicate entries allowed */
        DUPLCT                  DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = +/- Numeric Sign */
        SIGNED                  DATATYPE IS TEXT SIZE IS 1.

        END GL_USERDEF_CDD STRUCTURE.

END GL_USERDEF.
