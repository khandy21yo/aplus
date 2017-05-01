DEFINE RECORD CDD$TOP.PD.PD_PACK

        DESCRIPTION IS /*Product Pack Description*/.

        PD_PACK_CDD STRUCTURE.

        /* Element =
        Description = Pack Code */
        CODE                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Packaging form */
        FORM                    DATATYPE IS TEXT SIZE IS 3.

        /* Element =
        Description = Packaging material */
        MATERIAL                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Volume unit of measure */
        UOM                     DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Units in one pack */
        PACK_FACTOR             DATATYPE IS G_FLOATING.

        /* Element =
        Description = Weight of one unit in case */
        WEIGHT_FACTOR           DATATYPE IS G_FLOATING.

        /* Element =
        Description = Product units in one pack */
        PRODUCT_FACTOR          DATATYPE IS G_FLOATING.

        END PD_PACK_CDD STRUCTURE.

END PD_PACK.
