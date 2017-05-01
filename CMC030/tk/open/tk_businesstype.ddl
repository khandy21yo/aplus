DEFINE RECORD CDD$TOP.TK.TK_BUSINESSTYPE

        DESCRIPTION IS /*Business Type Code*/.

        TK_BUSINESSTYPE_CDD STRUCTURE.

        /* Element =
        Description = Business Type code */
        BUSINESSTYPE            DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        END TK_BUSINESSTYPE_CDD STRUCTURE.

END TK_BUSINESSTYPE.
