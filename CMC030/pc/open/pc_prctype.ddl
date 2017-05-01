DEFINE RECORD CDD$TOP.PC.PC_PRCTYPE

        DESCRIPTION IS /*Price Type Description*/.

        PC_PRCTYPE_CDD STRUCTURE.

        /* Element = CODE
        Description = Price type */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element = DESCRIPTION
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 20.

        END PC_PRCTYPE_CDD STRUCTURE.

END PC_PRCTYPE.
