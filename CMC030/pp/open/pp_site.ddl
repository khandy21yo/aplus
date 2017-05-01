DEFINE RECORD CDD$TOP.PP.PP_SITE

        DESCRIPTION IS /*Pacific Pride Site File*/.

        PP_SITE_CDD STRUCTURE.

        /* Element =
        Description = Host # */
        HOST                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Site Code */
        SITE                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = site Type */
        STYPE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element = NAME
        Description = Site Name */
        SNAME                   DATATYPE IS TEXT SIZE IS 30.

        /* Element = ADDRESS
        Description = Address Line */
        ADDRESS                 DATATYPE IS TEXT SIZE IS 25.

        /* Element = CITY
        Description = City */
        CITY                    DATATYPE IS TEXT SIZE IS 15.

        /* Element = STATE
        Description = State */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = ZIP
        Description = Zip code */
        ZIP                     DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Local Sale Location */
        LOCSALE                 DATATYPE IS TEXT SIZE IS 3.

        /* Element =
        Description = Foreign Sale Location */
        FORSALE                 DATATYPE IS TEXT SIZE IS 3.

        /* Element =
        Description = Foreign Purchase Location */
        FORPUR                  DATATYPE IS TEXT SIZE IS 3.

        END PP_SITE_CDD STRUCTURE.

END PP_SITE.
