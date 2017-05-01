DEFINE RECORD CDD$TOP.UTL.UTL_MASTER_DEVICE

        DESCRIPTION IS /*Utility Device File from CMC:*/.

        UTL_MASTER_DEVICE_CDD STRUCTURE.

        /* Element =
        Description = */
        FILENAM                 DATATYPE IS TEXT SIZE IS 39.

        /* Element =
        Description = */
        DEVICE                  DATATYPE IS TEXT SIZE IS 127.

        /* Element =
        Description = */
        PROCOD                  DATATYPE IS TEXT SIZE IS 32.

        /* Element =
        Description = */
        CATAG                   DATATYPE IS TEXT SIZE IS 8.

        END UTL_MASTER_DEVICE_CDD STRUCTURE.

END UTL_MASTER_DEVICE.
