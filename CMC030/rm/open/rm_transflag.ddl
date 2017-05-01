DEFINE RECORD CDD$TOP.RM.RM_TRANSFLAG

        DESCRIPTION IS /*Transaction Flag for Worksheet*/.

        RM_TRANSFLAG_CDD STRUCTURE.

        /* Element = TRANSTYPE
        Description = Transaction type code */
        TRANSTYPE               DATATYPE IS TEXT SIZE IS 2.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        END RM_TRANSFLAG_CDD STRUCTURE.

END RM_TRANSFLAG.
