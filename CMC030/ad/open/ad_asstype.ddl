DEFINE RECORD CDD$TOP.AD.AD_ASSTYPE

        DESCRIPTION IS /*Asset Type*/.

        AD_ASSTYPE_CDD STRUCTURE.

        /* Element =
        Description = Asset type */
        ASSET_TYPE              DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 20.

        END AD_ASSTYPE_CDD STRUCTURE.

END AD_ASSTYPE.
