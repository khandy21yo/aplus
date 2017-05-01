DEFINE RECORD CDD$TOP.OE.OE_ORDERTYPE

        DESCRIPTION IS /*Sales Order Type Description Table*/.

        OE_ORDERTYPE_CDD STRUCTURE.

        /* Element =
        Description = Order Type */
        ORDTYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 30.

        END OE_ORDERTYPE_CDD STRUCTURE.

END OE_ORDERTYPE.
