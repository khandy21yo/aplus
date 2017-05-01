DEFINE RECORD CDD$TOP.PD.PD_LABEL

        DESCRIPTION IS /*Product Label Description*/.

        PD_LABEL_CDD STRUCTURE.

        /* Element = CODE
        Description = Label code */
        CODE                    DATATYPE IS TEXT SIZE IS 4.

        /* Element = DESCRIPTION
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 20.

        END PD_LABEL_CDD STRUCTURE.

END PD_LABEL.
