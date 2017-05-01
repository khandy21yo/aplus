DEFINE RECORD CDD$TOP.AD.AD_CONVENTION

        DESCRIPTION IS /*Convention Description*/.

        AD_CONVENTION_CDD STRUCTURE.

        /* Element =
        Description = Convention code */
        CONVENTION              DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Number months dep in the first year */
        COEFF                   DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Specification (regardless,..) */
        SPECIFIC                DATATYPE IS TEXT SIZE IS 1.

        END AD_CONVENTION_CDD STRUCTURE.

END AD_CONVENTION.
