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

rightLevel = Meta.Int32Field
    { Meta.fieldName    = "RightLevel"
    , Meta.fieldSize    = Nothing
    , Meta.fieldValue   = ""
    , Meta.fieldComment = "Right level."
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
    , Meta.entityVersion = "1"
    , Meta.entityName   = "HeartBeat"
    , Meta.entityFields = []
    }

login = Meta.Message
    { Meta.entityId      = "Message::LOGIN"
    , Meta.entityVersion = "1"
    , Meta.entityName   = "Login"
    , Meta.entityFields =
        [ userName
        , passWord
        ]
    }

loginAck = Meta.Message
    { Meta.entityId      = "Message::LOGIN_ACK"
    , Meta.entityVersion = "1"
    , Meta.entityName   = "LoginAck"
    , Meta.entityFields =
        [ rightLevel 
        ]
    }

logout = Meta.Message
    { Meta.entityId      = "Message::LOGOUT"
    , Meta.entityVersion = "1"
    , Meta.entityName   = "Logout"
    , Meta.entityFields = []
    }

logoutAck = Meta.Message
    { Meta.entityId      = "Message::LOGOUT_ACK"
    , Meta.entityVersion = "1"
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
    }

