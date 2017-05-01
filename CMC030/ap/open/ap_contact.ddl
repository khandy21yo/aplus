DEFINE RECORD CDD$TOP.AP.AP_CONTACT

        DESCRIPTION IS /*Contact File*/.

        AP_CONTACT_CDD STRUCTURE.

        /* Element =
        Description = */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        CONTACT_NAME            DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = */
        TITLE                   DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = */
        PHONE                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        EXTENSION               DATATYPE IS TEXT SIZE IS 4.

        END AP_CONTACT_CDD STRUCTURE.

END AP_CONTACT.
