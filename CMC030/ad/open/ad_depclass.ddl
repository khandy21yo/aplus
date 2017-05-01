DEFINE RECORD CDD$TOP.AD.AD_DEPCLASS

        DESCRIPTION IS /*Depreciation Class Definition Table*/.

        AD_DEPCLASS_CDD STRUCTURE.

        /* Element = DEPCLASS
        Description = Depreciation class code */
        DEPCLASS                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element = PROPTYPE
        Description = Property type code */
        PROPTYPE                DATATYPE IS TEXT SIZE IS 2.

        /* Element = DEPMETHOD
        Description = Depreciation method */
        DEPMETHOD               DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Recovery period */
        YEARS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = DEPCONV
        Description = First year convention code */
        FYCONV                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = DEPCONV
        Description = Disposition convention code */
        DYCONV                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = OPTTABLE
        Description = Depreciation optional table code */
        OPTTABLE                DATATYPE IS TEXT SIZE IS 6.

        /* Element = CEILTABLE
        Description = Ceiling table code */
        CEILTABLE               DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Adjust basis by salvage (Y,N) */
        SALVFACTOR              DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Adjust basis by bonus (Y,N) */
        BONUSFACTOR             DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Adjust basis by ITC (Y,N) */
        ITCFACTOR               DATATYPE IS TEXT SIZE IS 1.

        END AD_DEPCLASS_CDD STRUCTURE.

END AD_DEPCLASS.
