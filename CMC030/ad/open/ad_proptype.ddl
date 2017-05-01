DEFINE RECORD CDD$TOP.AD.AD_PROPTYPE

        DESCRIPTION IS /*Property Type Description*/.

        AD_PROPTYPE_CDD STRUCTURE.

        /* Element = PROPTYPE
        Description = Property type code */
        PROPTYPE                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        END AD_PROPTYPE_CDD STRUCTURE.

END AD_PROPTYPE.
