DEFINE RECORD CDD$TOP.BS.BS_CLIENT

        DESCRIPTION IS /*Client File*/.

        BS_CLIENT_CDD STRUCTURE.

        /* Element = CLIENT
        Description = Client Number */
        CLIENT                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Client name */
        CLIENTNAME              DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = Alpha sort */
        ALPSRT                  DATATYPE IS TEXT SIZE IS 15.

        /* Element =
        Description = Client address 1 */
        ADD1                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Client address 2 */
        ADD2                    DATATYPE IS TEXT SIZE IS 21.

        /* Element =
        Description = Client city */
        CITY                    DATATYPE IS TEXT SIZE IS 15.

        /* Element = STATE
        Description = State */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = ZIP
        Description = Zip code */
        ZIP                     DATATYPE IS TEXT SIZE IS 10.

        /* Element = COUNTRY
        Description = Country */
        COUNTRY                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = COUNTY
        Description = County */
        COUNTY                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = PHONE
        Description = Phone number */
        PHONE                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        REFNO                   DATATYPE IS TEXT SIZE IS 16.

        /* Element =
        Description = */
        BIRTHDATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        SEX                     DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = */
        ONSETDATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        SSTATUS                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = */
        TERMDATE                DATATYPE IS TEXT SIZE IS 8.

        END BS_CLIENT_CDD STRUCTURE.

END BS_CLIENT.
