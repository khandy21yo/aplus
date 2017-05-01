DEFINE RECORD CDD$TOP.UTL.UTL_PACKFORM

        DESCRIPTION IS /*Packiging Form Description*/.

        UTL_PACKFORM_CDD STRUCTURE.

        /* Element = PACKFORM
        Description = Pack form code */
        CODE                    DATATYPE IS TEXT SIZE IS 3.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 20.

        END UTL_PACKFORM_CDD STRUCTURE.

END UTL_PACKFORM.
