DEFINE RECORD CDD$TOP.AR.AR_CONTACT

        DESCRIPTION IS /*Contact Files*/.

        AR_CONTACT_CDD STRUCTURE.

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

        END AR_CONTACT_CDD STRUCTURE.

END AR_CONTACT.
