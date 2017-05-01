DEFINE RECORD CDD$TOP.GL.GL_USERLIST

        DESCRIPTION IS /*List of Accounts Allowed for Specified Users*/.

        GL_USERLIST_CDD STRUCTURE.

        /* Element =
        Description = User Name */
        USER                    DATATYPE IS TEXT SIZE IS 16.

        /* Element = ACCOUNT
        Description = Allowed General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Access Flag */
        FLAG                    DATATYPE IS TEXT SIZE IS 1.

        END GL_USERLIST_CDD STRUCTURE.

END GL_USERLIST.
