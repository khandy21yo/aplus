DEFINE RECORD CDD$TOP.TK.TK_ALIAS

        DESCRIPTION IS /*Alias's for programmer names*/.

        TK_ALIAS_CDD STRUCTURE.

        /* Element =
        Description = Alias */
        ALIAS                   DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Programmers Name */
        PROGRAMMER              DATATYPE IS TEXT SIZE IS 30.

        END TK_ALIAS_CDD STRUCTURE.

END TK_ALIAS.
