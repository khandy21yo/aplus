DEFINE RECORD CDD$TOP.PD.PD_CATEGORY

        DESCRIPTION IS /*Product Category Description*/.

        PD_CATEGORY_CDD STRUCTURE.

        /* Element = CODE
        Description = Code */
        CODE                    DATATYPE IS TEXT SIZE IS 4.

        /* Element = DESCRIPTION
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 20.

        END PD_CATEGORY_CDD STRUCTURE.

END PD_CATEGORY.
