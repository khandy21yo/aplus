DEFINE RECORD CDD$TOP.PD.PD_PRODTYPE

        DESCRIPTION IS /*Product Type Description*/.

        PD_PRODTYPE_CDD STRUCTURE.

        /* Element = CODE
        Description = Code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Type description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 20.

        END PD_PRODTYPE_CDD STRUCTURE.

END PD_PRODTYPE.
