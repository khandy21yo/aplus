DEFINE RECORD CDD$TOP.PD.PD_SUBSTITUTE

        DESCRIPTION IS /*Substitute Part Numbers*/.

        PD_SUBSTITUTE_CDD STRUCTURE.

        /* Element = PRODUCT
        Description = Our Product Number */
        OUR_PRODUCT             DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Their part number */
        THEIR_PRODUCT           DATATYPE IS TEXT SIZE IS 30.

        /* Element = VENDOR
        Description = Vendor Number */
        VENDOR                  DATATYPE IS TEXT SIZE IS 10.

        END PD_SUBSTITUTE_CDD STRUCTURE.

END PD_SUBSTITUTE.
