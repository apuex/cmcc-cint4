{-# LANGUAGE QuasiQuotes #-}
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
    { Meta.fieldName    = "RightLevel"
    , Meta.fieldType    = "int32_t"
    , Meta.fieldSize    = Nothing
    , Meta.fieldValue   = ""
    , Meta.fieldEnums   =
        [ ("INVALID", 0)
        , ("LEVEL1", 1)
        , ("LEVEL2", 2)
        ]
    , Meta.fieldComment = "Right level."
    }


-- Fields
userName = Meta.StringField
    { Meta.fieldName    = "UserName"
    , Meta.fieldSize    = Just 20
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Login user name."
    }

passWord = Meta.StringField
    { Meta.fieldName    = "PassWord"
    , Meta.fieldSize    = Just 20
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Login password."
    }

-- Message header and CRC16 fields

msgHeader = Meta.Int32Field
    { Meta.fieldName    = "Header"
    , Meta.fieldSize    = Nothing
    , Meta.fieldValue   = "0x7E7C6B5A"
    , Meta.fieldComment = "Message header, 0x7E7C6B5A."
    }

msgLength = Meta.Int32Field
    { Meta.fieldName    = "Length"
    , Meta.fieldSize    = Nothing
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Message length."
    }

msgSerialNo = Meta.Int32Field
    { Meta.fieldName    = "SerialNo"
    , Meta.fieldSize    = Nothing
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Message serial number."
    }

msgPKType = Meta.Int32Field
    { Meta.fieldName    = "PKType"
    , Meta.fieldSize    = Nothing
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "PKType."
    }

msgCRC16 = Meta.UInt16Field
    { Meta.fieldName    = "CRC16"
    , Meta.fieldSize    = Nothing
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Message CRC16."
    }

-- messages
heartBeat = Meta.Message
    { Meta.entityId      = "Message::HEART_BEAT"
    , Meta.entityName   = "HeartBeat"
    , Meta.entityFields = []
    }

login = Meta.Message
    { Meta.entityId      = "Message::LOGIN"
    , Meta.entityName   = "Login"
    , Meta.entityFields =
        [ userName
        , passWord
        ]
    }

loginAck = Meta.Message
    { Meta.entityId      = "Message::LOGIN_ACK"
    , Meta.entityName   = "LoginAck"
    , Meta.entityFields =
        [ enumRightLevel
        ]
    }

logout = Meta.Message
    { Meta.entityId      = "Message::LOGOUT"
    , Meta.entityName   = "Logout"
    , Meta.entityFields = []
    }

logoutAck = Meta.Message
    { Meta.entityId      = "Message::LOGOUT_ACK"
    , Meta.entityName   = "LogoutAck"
    , Meta.entityFields = []
    }


model = Meta.Model
    { Meta.namespace = "message"
    , Meta.headerFields = 
        [ msgHeader
        , msgLength
        , msgSerialNo
        , msgPKType
        , msgCRC16
        ]
    , Meta.entities =
        [ heartBeat
        , login, loginAck
        , logout, logoutAck
        ]
    , Meta.enumerates =
        [ enumRightLevel
        ]
    }

