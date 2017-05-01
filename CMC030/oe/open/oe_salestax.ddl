DEFINE RECORD CDD$TOP.OE.OE_SALESTAX

        DESCRIPTION IS /*Sales Tax Code Table*/.

        OE_SALESTAX_CDD STRUCTURE.

        /* Element = TAXCODE
        Description = Tax code */
        TAXCODE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = DESCRIPTION
        Description = Description */
        JURISDICTION            DATATYPE IS TEXT SIZE IS 20.

        /* Element = PERCENTAGE
        Description = State Tax Percentage */
        STATETAX                DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = GL Account Number for State */
        STATEACC                DATATYPE IS TEXT SIZE IS 18.

        /* Element = PERCENTAGE
        Description = City Sales Tax Percentage */
        CITYTAX                 DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = GL Account Number for City Tax */
        CITYACC                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = PERCENTAGE
        Description = County Sales TAx Percentage */
        COUNTYTAX               DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = GL Account Number for County Tax */
        COUNTYACC               DATATYPE IS TEXT SIZE IS 18.

        END OE_SALESTAX_CDD STRUCTURE.

END OE_SALESTAX.
