¿{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveGeneric #-}
module MessageModel where

import qualified Metadata               as Meta
import qualified Data.List              as L
import qualified Data.Text              as T

-- Enums
enumRightLevel = Meta.EnumerateField
    { Meta.fieldName    = "EnumRightLevel"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "INVALID"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "æ æé"
                        }
        , Meta.EnumItem { Meta.enumName = "LEVEL1"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "å·å¤æ°æ®è¯»çæé,å½ç¨æ·å¯ä»¥è¯»æä¸ªæ°æ®ï¼èæ æ³åä»»ä½æ°æ®æ¶è¿åè¿ä¸æéå¼ã"
                        }
        , Meta.EnumItem { Meta.enumName = "LEVEL2"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "å·å¤æ°æ®è¯»ãåçæéï¼å½ç¨æ·å¯¹æä¸ªæ°æ®å·æè¯»åæéæ¶è¿åè¿ä¸æéå¼ã"
                        }
        ]
    , Meta.fieldComment = "çæ§ç³»ç»ä¸çº§SCåä¸çº§SCæä¾çæéå®ä¹"
    }

enumResult = Meta.EnumerateField
    { Meta.fieldName    = "EnumResult"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "FAILURE"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "å¤±è´¥"
                        }
        , Meta.EnumItem { Meta.enumName = "SUCCESS"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "æå"
                        }
        ]
    , Meta.fieldComment = "æ¥æè¿åç»æ"
    }

enumType = Meta.EnumerateField
    { Meta.fieldName    = "EnumType"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "ALARM"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "åè­¦"
                        }
        , Meta.EnumItem { Meta.enumName = "DO"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "æ°å­è¾åºéï¼é¥æ§"
                        }
        , Meta.EnumItem { Meta.enumName = "AO"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "æ¨¡æè¾åºéï¼é¥è°"
                        }
        , Meta.EnumItem { Meta.enumName = "AI"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "æ¨¡æè¾å¥éï¼é¥æµ"
                        }
        , Meta.EnumItem { Meta.enumName = "DI"
                        , Meta.enumValue = 4
                        , Meta.enumComment = "æ°å­è¾å¥éï¼åå«å¤ææ°å­è¾å¥éï¼ï¼é¥ä¿¡"
                        }
        , Meta.EnumItem { Meta.enumName = "DEVICE"
                        , Meta.enumValue = 5
                        , Meta.enumComment = "è®¾å¤"
                        }
        , Meta.EnumItem { Meta.enumName = "ROOM"
                        , Meta.enumValue = 6
                        , Meta.enumComment = "æºæ¿"
                        }
        , Meta.EnumItem { Meta.enumName = "SITE"
                        , Meta.enumValue = 7
                        , Meta.enumComment = "ç«ç¹"
                        }
        , Meta.EnumItem { Meta.enumName = "AREA"
                        , Meta.enumValue = 8
                        , Meta.enumComment = "åºå"
                        }
        ]
    , Meta.fieldComment = "çæ§ç³»ç»æ°æ®çç§ç±»"
    }

enumAlarmLevel = Meta.EnumerateField
    { Meta.fieldName    = "EnumAlarmLevel"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "NOALARM"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "æ åè­¦"
                        }
        , Meta.EnumItem { Meta.enumName = "CRITICAL"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "ä¸çº§åè­¦"
                        }
        , Meta.EnumItem { Meta.enumName = "MAJOR"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "äºçº§åè­¦"
                        }
        , Meta.EnumItem { Meta.enumName = "MINOR"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "ä¸çº§åè­¦"
                        }
        , Meta.EnumItem { Meta.enumName = "HINT"
                        , Meta.enumValue = 4
                        , Meta.enumComment = "åçº§åè­¦"
                        }
        ]
    , Meta.fieldComment = "åè­¦çç­çº§"
    }

enumEnable = Meta.EnumerateField
    { Meta.fieldName    = "EnumEnable"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "DISABLE"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "ç¦æ­¢/ä¸è½"
                        }
        , Meta.EnumItem { Meta.enumName = "ENABLE"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "å¼æ¾/è½"
                        }
        ]
    , Meta.fieldComment = "ä½¿è½çå±æ§"
    }

enumFlag = Meta.EnumerateField
    { Meta.fieldName    = "EnumFlag"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "BEGIN"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "å¼å§"
                        }
        , Meta.EnumItem { Meta.enumName = "END"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "ç»æ"
                        }
        ]
    , Meta.fieldComment = "åè­¦æ å¿"
    }

enumAccessMode = Meta.EnumerateField
    { Meta.fieldName    = "EnumAccessMode"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "ASK_ANSWER"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "ä¸é®ä¸ç­æ¹å¼"
                        }
        , Meta.EnumItem { Meta.enumName = "CHANGE_TRIGGER"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "æ¹åæ¶èªå¨åéæ°æ®æ¹å¼"
                        }
        , Meta.EnumItem { Meta.enumName = "TIME_TRIGGER"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "å®æ¶åéæ°æ®æ¹å¼"
                        }
        , Meta.EnumItem { Meta.enumName = "STOP"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "åæ­¢åéæ°æ®æ¹å¼"
                        }
        ]
    , Meta.fieldComment = "ä¸çº§å®æ¶æ°æ®è®¿é®çæ¹å¼"
    }

enumState = Meta.EnumerateField
    { Meta.fieldName    = "Status"
    , Meta.fieldType    = "EnumState"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "NOALARM"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "æ­£å¸¸æ°æ®"
                        }
        , Meta.EnumItem { Meta.enumName = "CRITICAL"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "ä¸çº§åè­¦"
                        }
        , Meta.EnumItem { Meta.enumName = "MAJOR"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "äºçº§åè­¦"
                        }
        , Meta.EnumItem { Meta.enumName = "MINOR"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "ä¸çº§åè­¦"
                        }
        , Meta.EnumItem { Meta.enumName = "HINT"
                        , Meta.enumValue = 4
                        , Meta.enumComment = "åçº§åè­¦"
                        }
        , Meta.EnumItem { Meta.enumName = "OPEVENT"
                        , Meta.enumValue = 5
                        , Meta.enumComment = "æä½äºä»¶"
                        }
        , Meta.EnumItem { Meta.enumName = "INVALID"
                        , Meta.enumValue = 6
                        , Meta.enumComment = "æ ææ°æ®"
                        }
        ]
    , Meta.fieldComment = "æ°æ®å¼çç¶æ"
    }

enumAlarmMode = Meta.EnumerateField
    { Meta.fieldName    = "EnumAlarmMode"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "NOALARM"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "ä¸ååè­¦ä¸æ¥"
                        }
        , Meta.EnumItem { Meta.enumName = "CRITICAL"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "ä¸çº§åè­¦ä¸æ¥"
                        }
        , Meta.EnumItem { Meta.enumName = "MAJOR"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "äºçº§åè­¦ä¸æ¥"
                        }
        , Meta.EnumItem { Meta.enumName = "MINOR"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "ä¸çº§åè­¦ä¸æ¥"
                        }
        , Meta.EnumItem { Meta.enumName = "HINT"
                        , Meta.enumValue = 4
                        , Meta.enumComment = "åçº§åè­¦ä¸æ¥"
                        }
        ]
    , Meta.fieldComment = "åè­¦ç­çº§è®¾å®çæ¨¡å¼"
    }

enumSiteType = Meta.EnumerateField
    { Meta.fieldName    = "EnumSiteType"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "DATACENTER"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "æ°æ®ä¸­å¿"
                        }
        , Meta.EnumItem { Meta.enumName = "ROOM"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "éä¿¡æºæ¥¼"
                        }
        , Meta.EnumItem { Meta.enumName = "LOCALTRANS"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "ä¼ è¾èç¹"
                        }
        , Meta.EnumItem { Meta.enumName = "STATION"
                        , Meta.enumValue = 4
                        , Meta.enumComment = "éä¿¡åºç«"
                        }
        ]
    , Meta.fieldComment = "å±ç«ç±»å"
    }

enumRoomType = Meta.EnumerateField
    { Meta.fieldName    = "EnumRoomType"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "CONVERGE"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "æ±èæºæ¿"
                        }
        , Meta.EnumItem { Meta.enumName = "BASESTATION"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "åºç«æºæ¿"
                        }
        , Meta.EnumItem { Meta.enumName = "GENERATION"
                        , Meta.enumValue = 11
                        , Meta.enumComment = "åçµæºæ¿"
                        }
        , Meta.EnumItem { Meta.enumName = "ELECTRIC"
                        , Meta.enumValue = 12
                        , Meta.enumComment = "çµåæºæ¿"
                        }
        , Meta.EnumItem { Meta.enumName = "BATTERY"
                        , Meta.enumValue = 13
                        , Meta.enumComment = "çµæ± æºæ¿"
                        }
        , Meta.EnumItem { Meta.enumName = "AIRCONDITION"
                        , Meta.enumValue = 14
                        , Meta.enumComment = "ç©ºè°æºæ¿"
                        }
        , Meta.EnumItem { Meta.enumName = "TRANSFERS"
                        , Meta.enumValue = 51
                        , Meta.enumComment = "ä¼ è¾æºæ¿"
                        }
        , Meta.EnumItem { Meta.enumName = "EXCHANGE"
                        , Meta.enumValue = 52
                        , Meta.enumComment = "äº¤æ¢æºæ¿"
                        }
        , Meta.EnumItem { Meta.enumName = "DATA"
                        , Meta.enumValue = 53
                        , Meta.enumComment = "æ°æ®æºæ¿"
                        }
        , Meta.EnumItem { Meta.enumName = "IDC"
                        , Meta.enumValue = 54
                        , Meta.enumComment = "IDCæºæ¿"
                        }
        , Meta.EnumItem { Meta.enumName = "COLLIGATION"
                        , Meta.enumValue = 54
                        , Meta.enumComment = "ç»¼åæºæ¿"
                        }
        ]
    , Meta.fieldComment = "æºæ¿ç±»å"
    }

enumModifyType = Meta.EnumerateField
    { Meta.fieldName    = "EnumModifyType"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "ADDNONODES"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "æ°å¢ï¼æ å­èç¹ï¼"
                        }
        , Meta.EnumItem { Meta.enumName = "ADDINNODES"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "æ°å¢ï¼å«å­èç¹ï¼"
                        }
        , Meta.EnumItem { Meta.enumName = "DELETE"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "å é¤"
                        }
        , Meta.EnumItem { Meta.enumName = "MODIFYNONODES"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "ä¿®æ¹ï¼ä»ä¿®æ¹æ¬èç¹ï¼"
                        }
        , Meta.EnumItem { Meta.enumName = "MODIFYINNODES"
                        , Meta.enumValue = 4
                        , Meta.enumComment = "ä¿®æ¹ï¼æ¶åå°å­èç¹ï¼"
                        }
        ]
    , Meta.fieldComment = "å¯¹è±¡å±æ§ä¿®æ¹ç±»å"
    }

enumUpdateType = Meta.EnumerateField
    { Meta.fieldName    = "EnumUpdateType"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "StoragePeriod"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "å­å¨å¨æ"
                        }
        , Meta.EnumItem { Meta.enumName = "Absolute"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "ç»å¯¹éå¼"
                        }
        , Meta.EnumItem { Meta.enumName = "Relative"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "ç¸å¯¹éå¼"
                        }
        , Meta.EnumItem { Meta.enumName = "Static"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "ç»è®¡æ å¿"
                        }
        ]
    , Meta.fieldComment = "æ°æ®æ´æ°ç±»å"
    }

enumNodeType = Meta.EnumerateField
    { Meta.fieldName    = "EnumNodeType"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "NodeType0"
                        , Meta.enumValue = 0
                        , Meta.enumComment = "M-GSM900M"
                        }
        , Meta.EnumItem { Meta.enumName = "NodeType1"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "D-DCS1800M"
                        }
        , Meta.EnumItem { Meta.enumName = "NodeType2"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "T-DSCDMA"
                        }
        , Meta.EnumItem { Meta.enumName = "NodeType3"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "LTE"
                        }
        ]
    , Meta.fieldComment = "å±ç«ç¹å¾é»è¾åç±»"
    }

enumNeStatusType = Meta.EnumerateField
    { Meta.fieldName    = "EnumNeStatusType"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "NeStatusType1"
                        , Meta.enumValue = 1
                        , Meta.enumComment = "ç°ç½æä¸å¡æ¿è½½"
                        }
        , Meta.EnumItem { Meta.enumName = "NeStatusType2"
                        , Meta.enumValue = 2
                        , Meta.enumComment = "ç°ç½æ ä¸å¡æ¿è½½"
                        }
        , Meta.EnumItem { Meta.enumName = "NeStatusType3"
                        , Meta.enumValue = 3
                        , Meta.enumComment = "éç½"
                        }
        , Meta.EnumItem { Meta.enumName = "NeStatusType4"
                        , Meta.enumValue = 4
                        , Meta.enumComment = "å·¥ç¨"
                        }
        , Meta.EnumItem { Meta.enumName = "NeStatusType5"
                        , Meta.enumValue = 5
                        , Meta.enumComment = "å é¤"
                        }
        , Meta.EnumItem { Meta.enumName = "NeStatusType6"
                        , Meta.enumValue = 6
                        , Meta.enumComment = "æªç¡®è®¤"
                        }
        ]
    , Meta.fieldComment = "èµæºå·¥ç¨ç¶æ"
    }

enumPKType = Meta.EnumerateField
    { Meta.fieldName    = "PKType"
    , Meta.fieldType    = "EnumPKType"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ Meta.EnumItem { Meta.enumName = "LOGIN"
                        , Meta.enumValue = 101
                        , Meta.enumComment = "ç»å½"
                        }
        , Meta.EnumItem { Meta.enumName = "LOGIN_ACK"
                        , Meta.enumValue = 102
                        , Meta.enumComment = "ç»å½ååº"
                        }
        , Meta.EnumItem { Meta.enumName = "LOGOUT"
                        , Meta.enumValue = 103
                        , Meta.enumComment = "ç»åº"
                        }
        , Meta.EnumItem { Meta.enumName = "LOGOUT_ACK"
                        , Meta.enumValue = 104
                        , Meta.enumComment = "ç»åºååº"
                        }
        , Meta.EnumItem { Meta.enumName = "SET_DYN_ACCESS_MODE"
                        , Meta.enumValue = 401
                        , Meta.enumComment = "è¯·æ±å®æ¶æ°æ®æ¹å¼è®¾ç½®"
                        }
        , Meta.EnumItem { Meta.enumName = "DYN_ACCESS_MODE_ACK"
                        , Meta.enumValue = 402
                        , Meta.enumComment = "å®æ¶æ°æ®ååº"
                        }
        , Meta.EnumItem { Meta.enumName = "SET_ALARM_MODE"
                        , Meta.enumValue = 501
                        , Meta.enumComment = "è¯·æ±åè­¦æ°æ®æ¹å¼è®¾ç½®"
                        }
        , Meta.EnumItem { Meta.enumName = "ALARM_MODE_ACK"
                        , Meta.enumValue = 502
                        , Meta.enumComment = "åè­¦æ¹å¼è®¾ç½®ååº"
                        }
        , Meta.EnumItem { Meta.enumName = "SEND_ALARM"
                        , Meta.enumValue = 503
                        , Meta.enumComment = "å®æ¶åè­¦åé"
                        }
        , Meta.EnumItem { Meta.enumName = "SEND_ALARM_ACK"
                        , Meta.enumValue = 504
                        , Meta.enumComment = "å®æ¶åè­¦åéç¡®è®¤"
                        }
        , Meta.EnumItem { Meta.enumName = "SYNC_ALARM"
                        , Meta.enumValue = 505
                        , Meta.enumComment = "åè­¦åæ­¥"
                        }
        , Meta.EnumItem { Meta.enumName = "SYNC_ALARM_ACK"
                        , Meta.enumValue = 506
                        , Meta.enumComment = "åè­¦åæ­¥ç¡®è®¤"
                        }
        , Meta.EnumItem { Meta.enumName = "SET_POINT"
                        , Meta.enumValue = 1001
                        , Meta.enumComment = "åæ°æ®è¯·æ±"
                        }
        , Meta.EnumItem { Meta.enumName = "SET_POINT_ACK"
                        , Meta.enumValue = 1002
                        , Meta.enumComment = "åæ°æ®ååº"
                        }
        , Meta.EnumItem { Meta.enumName = "MODIFY_PA"
                        , Meta.enumValue = 1101
                        , Meta.enumComment = "æ¹å£ä»¤è¯·æ±"
                        }
        , Meta.EnumItem { Meta.enumName = "MODIFY_PA_ACK"
                        , Meta.enumValue = 1102
                        , Meta.enumComment = "æ¹å£ä»¤ååº"
                        }
        , Meta.EnumItem { Meta.enumName = "HEART_BEAT"
                        , Meta.enumValue = 1201
                        , Meta.enumComment = "ç¡®è®¤è¿æ¥"
                        }
        , Meta.EnumItem { Meta.enumName = "HEART_BEAT_ACK"
                        , Meta.enumValue = 1202
                        , Meta.enumComment = "ååºè¿æ¥"
                        }
        , Meta.EnumItem { Meta.enumName = "TIME_CHECK"
                        , Meta.enumValue = 1301
                        , Meta.enumComment = "åéæ¶éæ¶æ¯"
                        }
        , Meta.EnumItem { Meta.enumName = "TIME_CHECK_ACK"
                        , Meta.enumValue = 1302
                        , Meta.enumComment = "æ¶éåæ­¥ååº"
                        }
        ]
    , Meta.fieldComment = "æ¥æå®ä¹"
    }


-- Data Structure Fields

-- TTime
years = Meta.Int16Field
    { Meta.fieldName    = "Years"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "å¹´"
    }

month = Meta.Int8Field
    { Meta.fieldName    = "Month"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "æ"
    }

day = Meta.Int8Field
    { Meta.fieldName    = "Day"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "æ¥"
    }

hour = Meta.Int8Field
    { Meta.fieldName    = "Hour"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "æ¶"
    }

minute = Meta.Int8Field
    { Meta.fieldName    = "Minute"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "å"
    }

second = Meta.Int8Field
    { Meta.fieldName    = "Second"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "ç§"
    }

tTime = Meta.Struct
    { Meta.entityId      = ""
    , Meta.entityName    = "TTime"
    , Meta.entityComment = "æ¶é´çç»æ"
    , Meta.entityFields  =
        [ years
        , month
        , day
        , hour
        , minute
        , second
        ]
    }

-- TA/TD/TID
siteID = Meta.StringField
    { Meta.fieldName    = "SiteID"
    , Meta.fieldSize    = "Length::SITEID_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "ç«ç¹ç¼å·"
    }

deviceID = Meta.StringField
    { Meta.fieldName    = "DeviceID"
    , Meta.fieldSize    = "Length::DEVICEID_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "è®¾å¤ç¼å·"
    }

signalID = Meta.StringField
    { Meta.fieldName    = "SignalID"
    , Meta.fieldSize    = "Length::ID_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "çæ§ç¹ç6ä½ä¿¡å·ç¼ç ï¼å³ãå¨ç¯ä¿¡å·æ ååå­å¸è¡¨(20170927)ãä¸­çä¿¡å·ç¼ç ID"
    }

signalNumber = Meta.StringField
    { Meta.fieldName    = "SignalNumber"
    , Meta.fieldSize    = "Length::SIGNALNUM_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "åç±»çæ§ç¹é¡ºåºå·"
    }

aiValue = Meta.StringField
    { Meta.fieldName    = "Value"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "AIå¼"
    }

ta = Meta.Struct
    { Meta.entityId      = ""
    , Meta.entityName    = "TA"
    , Meta.entityComment = "æ¨¡æéçå¼çç»æ"
    , Meta.entityFields  =
        [ siteID
        , deviceID
        , signalID
        , signalNumber
        , aiValue
        , enumState
        ]
    }

diValue = Meta.StringField
    { Meta.fieldName    = "Value"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "DIå¼"
    }

td = Meta.Struct
    { Meta.entityId      = ""
    , Meta.entityName    = "TD"
    , Meta.entityComment = "æ°å­éçå¼çç»æ"
    , Meta.entityFields  =
        [ siteID
        , deviceID
        , signalID
        , signalNumber
        , diValue
        , enumState
        ]
    }

tid = Meta.Struct
    { Meta.entityId      = ""
    , Meta.entityName    = "TID"
    , Meta.entityComment = "æ°æ®å¼çç»æ"
    , Meta.entityFields  =
        [ siteID
        , deviceID
        , signalID
        , signalNumber
        ]
    }


-- TAlarm
scID = Meta.StringField
    { Meta.fieldName    = "SCID"
    , Meta.fieldSize    = "SCID_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "SC IDç¼å·ï¼7ä½æ°å­ï¼å¨ç½èå´å¯ä¸ï¼éç¨6ä½è¡æ¿å°åºç¼ç +1ä½åºå·ç»æï¼"
    }

serialNo = Meta.StringField
    { Meta.fieldName    = "SerialNo"
    , Meta.fieldSize    = "ALARMSERIALNO_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "åè­¦åºå·ï¼10ä½æ°å­ï¼èå´0~4294967295ï¼æ¯ä¸çº§SCèå´çï¼ä¸è¶³10ä½åé¢è¡¥0ï¼"
    }

nmAlarmID = Meta.StringField
    { Meta.fieldName    = "NMAlarmID"
    , Meta.fieldSize    = "ID_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "6ä½åè­¦ç¼ç ID"
    }

alarmTime = Meta.StringField
    { Meta.fieldName    = "AlarmTime"
    , Meta.fieldSize    = "TIME_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "åè­¦æ¶é´ï¼YYYY-MM-DD<SPACEé®>hh:mm:ssï¼éç¨24å°æ¶çæ¶é´å¶å¼ï¼"
    }

eventValue = Meta.Float32Field
    { Meta.fieldName    = "EventValue"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "åè­¦è§¦åå¼"
    }

alarmDesc = Meta.StringField
    { Meta.fieldName    = "AlarmDesc"
    , Meta.fieldSize    = "DES_LENGTH"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "åè­¦æè¿°"
    }

tAlarm = Meta.Struct
    { Meta.entityId      = ""
    , Meta.entityName    = "TAlarm"
    , Meta.entityComment = "å½ååè­¦å¼çç»æ"
    , Meta.entityFields  =
        [ scID
        , serialNo
        , siteID
        , deviceID
        , signalID
        , signalNumber
        , nmAlarmID
        , alarmTime
        , enumState { Meta.fieldName = "AlarmLevel" }
        , enumFlag { Meta.fieldName = "AlarmFlag" }
        , eventValue
        , alarmDesc
        ]
    }


-- Info Fields
userName = Meta.StringField
    { Meta.fieldName    = "UserName"
    , Meta.fieldSize    = "USER_LENGTH"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Login user name."
    }

passWord = Meta.StringField
    { Meta.fieldName    = "PassWord"
    , Meta.fieldSize    = "PASSWORD_LEN"
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Login password."
    }

-- Message header and tail CRC16 fields

msgHeader = Meta.Int32Field
    { Meta.fieldName    = "Header"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = "0x7E7C6B5A"
    , Meta.fieldComment = "Message header, 0x7E7C6B5A."
    }

msgLength = Meta.Int32Field
    { Meta.fieldName    = "Length"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Message length."
    }

msgSerialNo = Meta.Int32Field
    { Meta.fieldName    = "SerialNo"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Message serial number."
    }

msgPKType = Meta.Int32Field
    { Meta.fieldName    = "PKType"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "PKType."
    }

msgCRC16 = Meta.UInt16Field
    { Meta.fieldName    = "CRC16"
    , Meta.fieldSize    = ""
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Message CRC16."
    }

-- messages
heartBeat = Meta.Message
    { Meta.entityId      = "Message::HEART_BEAT"
    , Meta.entityName   = "HeartBeat"
    , Meta.entityComment = "å½ååè­¦å¼çç»æ"
    , Meta.entityFields = []
    }

login = Meta.Message
    { Meta.entityId      = "Message::LOGIN"
    , Meta.entityName   = "Login"
    , Meta.entityComment = "å½ååè­¦å¼çç»æ"
    , Meta.entityFields =
        [ userName
        , passWord
        ]
    }

loginAck = Meta.Message
    { Meta.entityId      = "Message::LOGIN_ACK"
    , Meta.entityName   = "LoginAck"
    , Meta.entityComment = "å½ååè­¦å¼çç»æ"
    , Meta.entityFields =
        [ enumRightLevel
        ]
    }

logout = Meta.Message
    { Meta.entityId      = "Message::LOGOUT"
    , Meta.entityName   = "Logout"
    , Meta.entityComment = "å½ååè­¦å¼çç»æ"
    , Meta.entityFields = []
    }

logoutAck = Meta.Message
    { Meta.entityId      = "Message::LOGOUT_ACK"
    , Meta.entityName   = "LogoutAck"
    , Meta.entityComment = "å½ååè­¦å¼çç»æ"
    , Meta.entityFields = []
    }


model = Meta.Model
    { Meta.namespace = "message"
    , Meta.headerFields = 
        [ msgHeader
        , msgLength
        , msgSerialNo
        , msgPKType
        ]
    , Meta.tailFields = 
        [ msgCRC16
        ]
    , Meta.entities =
        [ heartBeat
        , login, loginAck
        , logout, logoutAck
        ]
    , Meta.enumerates =
        [ enumRightLevel
        , enumResult
        , enumType
        , enumAlarmLevel
        , enumEnable
        , enumFlag
        , enumAccessMode
        , enumState
        , enumAlarmMode
        , enumSiteType
        , enumRoomType
        , enumModifyType
        , enumUpdateType
        , enumNodeType
        , enumNeStatusType
        , enumPKType        ]
    }

