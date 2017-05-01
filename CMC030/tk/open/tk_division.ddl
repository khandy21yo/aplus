DEFINE RECORD CDD$TOP.TK.TK_DIVISION

        DESCRIPTION IS /*Division Type Code*/.

        TK_DIVISION_CDD STRUCTURE.

        /* Element =
        Description = DIVISION TYPE CODE */
        DIVISION                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = DESCRIPTION */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        END TK_DIVISION_CDD STRUCTURE.

END TK_DIVISION.
