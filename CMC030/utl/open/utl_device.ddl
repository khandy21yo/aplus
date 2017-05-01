DEFINE RECORD CDD$TOP.UTL.UTL_DEVICE

        DESCRIPTION IS /*Utility Device File*/.

        UTL_DEVICE_CDD STRUCTURE.

        /* Element =
        Description = */
        FILENAM                 DATATYPE IS TEXT SIZE IS 39.

        /* Element =
        Description = */
        DEVICENAME              DATATYPE IS TEXT SIZE IS 127.

        /* Element =
        Description = */
        PROCOD                  DATATYPE IS TEXT SIZE IS 32.

        /* Element =
        Description = */
        CATAG                   DATATYPE IS TEXT SIZE IS 8.

        END UTL_DEVICE_CDD STRUCTURE.

END UTL_DEVICE.
