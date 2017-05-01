DEFINE RECORD CDD$TOP.UTL.UTL_PACKMAT

        DESCRIPTION IS /*Packiging Material Description*/.

        UTL_PACKMAT_CDD STRUCTURE.

        /* Element = PACKMAT
        Description = Pack material code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 20.

        END UTL_PACKMAT_CDD STRUCTURE.

END UTL_PACKMAT.
